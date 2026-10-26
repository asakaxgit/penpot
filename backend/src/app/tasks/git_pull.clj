;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS SUBSIDIARY SL

(ns app.tasks.git-pull
  "Replace a linked file with the binfile committed in its folder.

  The folder is read through the GitHub or GitLab REST API. There is
  no merge. The bytes go through the same overwrite import a user
  would run by hand: a system snapshot is saved first, revn increases
  by one, and open editors are told to reload.

  The folder must contain one Penpot file. manifest.json is required.
  Entry sizes use the same limits as binfile import."
  (:require
   [app.binfile.common :as bfc]
   [app.binfile.v3 :as v3]
   [app.common.exceptions :as ex]
   [app.common.json :as json]
   [app.common.schema :as sm]
   [app.common.uuid :as uuid]
   [app.config :as cf]
   [app.db :as db]
   [app.git.remote :as remote]
   [app.git.token :as token]
   [app.http.client :as-alias http.client]
   [app.jobs :as jobs]
   [app.metrics :as mtx]
   [app.msgbus :as-alias mbus]
   [app.rpc.commands.binfile :as binfile]
   [app.rpc.commands.files :as files]
   [app.setup :as-alias setup]
   [app.storage :as sto]
   [app.storage.tmp :as tmp]
   [clojure.java.io :as jio]
   [cuerdas.core :as str]
   [datoteka.fs :as fs]
   [integrant.core :as ig])
  (:import
   java.util.zip.ZipEntry
   java.util.zip.ZipOutputStream
   org.postgresql.util.PSQLException))

(def schema:params
  [:map {:title "git-pull-params" :closed true}
   [:git-link-id ::sm/uuid]
   [:profile-id ::sm/uuid]])

(defn- limit
  [key default]
  (cf/get key default))

(defn import-limits
  []
  {:max-text-entry (limit :binfile-import-max-text-entry-size
                          bfc/default-max-text-entry-size)
   :max-binary-entry (limit :binfile-import-max-binary-entry-size
                            bfc/default-max-binary-entry-size)
   :max-text-total (limit :binfile-import-max-text-total-size
                          bfc/default-max-text-total-size)
   :max-entries (limit :binfile-import-max-zip-entries
                       bfc/default-max-zip-entries)})

(defn- safe-path?
  [path]
  (and (string? path)
       (not (str/blank? path))
       (not (str/starts-with? path "/"))
       (not (str/includes? path "\\"))
       (not (str/includes? path "//"))
       (not (some #(or (= % "..") (= % "."))
                  (str/split path "/")))))

(defn- text-entry?
  [path]
  (str/ends-with? path ".json"))

(defn check-folder!
  "Reject a folder that is not one Penpot file inside the import limits."
  [entries {:keys [max-text-entry max-binary-entry max-text-total max-entries]}]
  (when (empty? entries)
    (ex/raise :type :validation
              :code :git-empty-export
              :hint "the folder is empty"))
  (when (> (count entries) max-entries)
    (ex/raise :type :validation
              :code :git-tree-truncated
              :hint "the folder has too many files"))
  (let [total (volatile! 0)]
    (doseq [{:keys [path bytes]} entries]
      (when-not (safe-path? path)
        (ex/raise :type :validation
                  :code :git-folder
                  :hint "the folder contains an unsafe path"
                  :path path))
      (let [size  (alength ^bytes bytes)
            text? (text-entry? path)]
        (when (> size (if text? max-text-entry max-binary-entry))
          (ex/raise :type :validation
                    :code :git-entry-too-large
                    :hint "a file in the folder is too large"
                    :path path))
        (when text?
          (vswap! total + size)
          (when (> @total max-text-total)
            (ex/raise :type :validation
                      :code :git-entry-too-large
                      :hint "the folder is too large"))))))
  (let [manifest (some #(when (= "manifest.json" (:path %)) %) entries)]
    (when-not (and manifest (pos? (alength ^bytes (:bytes manifest))))
      (ex/raise :type :validation
                :code :git-manifest-missing
                :hint "the folder has no manifest.json"))
    (let [decoded (try
                    (json/decode (String. ^bytes (:bytes manifest) "UTF-8")
                                 :key-fn keyword)
                    (catch Throwable _
                      (ex/raise :type :validation
                                :code :git-manifest-missing
                                :hint "manifest.json is not valid")))]
      (when (not= 1 (count (:files decoded)))
        (ex/raise :type :validation
                  :code :invalid-condition
                  :hint "the folder must contain exactly one file")))))

(defn- write-zip!
  [path entries]
  (with-open [raw (jio/output-stream (jio/file (str path)))
              zip (ZipOutputStream. raw)]
    (doseq [{:keys [path bytes]} entries]
      (let [entry (ZipEntry. ^String path)]
        (.putNextEntry zip entry)
        (.write zip ^bytes bytes)
        (.closeEntry zip)))))

(defn- error-text
  [cause]
  (let [data (ex-data cause)]
    (str/prune (or (:hint data)
                   (some-> (:code data) name)
                   (ex-message cause)
                   "git pull failed")
               500)))

(defn- claim-pull!
  [cfg {:keys [job-id link-id]}]
  (if (and job-id (db/get* cfg :git-push {:job-id job-id}))
    ::skipped
    (try
      (db/insert! cfg :git-push
                  {:id (uuid/next)
                   :git-link-id link-id
                   :job-id job-id
                   :status "running"
                   :direction "pull"
                   :commit-message "Pull"})
      (catch PSQLException cause
        (if (db/duplicate-key-error? cause)
          ::skipped
          (throw cause))))))

(defn- finish-pull!
  [cfg row status extra]
  (db/update! cfg :git-push
              (merge {:status status} extra)
              {:id (:id row)}))

(defn execute
  "Read the linked folder and overwrite the file."
  [cfg context {:keys [git-link-id]}]
  (let [profile-id (:profile-id context)
        link       (db/get cfg :git-link {:id git-link-id})
        limits     (import-limits)]
    (files/check-edition-permissions! cfg profile-id (:file-id link))
    (let [row (claim-pull! cfg {:job-id (:id context)
                                :link-id (:id link)})]
      (if (= row ::skipped)
        {:skipped true}
        (let [output (tmp/tempfile :suffix ".penpot")]
          (try
            (let [pulled  (remote/pull-folder
                           cfg
                           {:provider (:provider link)
                            :repo-url (:repo-url link)
                            :branch (:branch link)
                            :folder (:folder link)
                            :token (token/decrypt cfg (:token link))
                            :max-entry-bytes (:max-binary-entry limits)})
                  entries (:entries pulled)]
              (check-folder! entries limits)
              (write-zip! output entries)
              (let [manifest (v3/get-manifest cfg output)]
                (when (not= 1 (count (:files manifest)))
                  (ex/raise :type :validation
                            :code :invalid-condition
                            :hint "the folder must contain exactly one file")))
              (binfile/overwrite-file
               cfg
               {:profile-id profile-id
                :file-id (:file-id link)
                :session-id nil
                :file {:path output
                       :filename "pull.penpot"
                       :mtype "application/zip"}})
              (finish-pull! cfg row "success"
                            {:req-data (db/json (:req-data pulled))
                             :rsp-data (db/json (:rsp-data pulled))
                             :error nil})
              {:count (count entries)})
            (catch Throwable cause
              (finish-pull! cfg row "error" {:error (error-text cause)})
              (throw cause))
            (finally
              (fs/delete output))))))))

(defn- http-client?
  [params]
  (some? (::http.client/client params)))

(defmethod ig/assert-key ::job-def
  [_ params]
  (assert (db/pool? (::db/pool params)) "expected a valid database pool")
  (assert (sto/valid-storage? (::sto/storage params)) "expected valid storage")
  (assert (http-client? params) "expected http client")
  (assert (map? (::setup/props params)) "expected setup props")
  (assert (mtx/metrics? (::mtx/metrics params)) "expected valid metrics")
  (assert (some? (::mbus/msgbus params)) "expected msgbus"))

(defmethod ig/init-key ::job-def
  [_ cfg]
  {::jobs/name :git-pull
   ::jobs/schema schema:params
   ::jobs/handler (fn [context params]
                    (execute cfg context params))
   ::jobs/decoder (sm/decoder schema:params sm/json-transformer)
   ::jobs/validator (sm/validator schema:params)})
