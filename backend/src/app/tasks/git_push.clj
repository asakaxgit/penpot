;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS SUBSIDIARY SL

(ns app.tasks.git-push
  "Push the canonical export of a file to its linked repository.

  A named version enqueues this job after the snapshot is committed.
  Push now enqueues it with no snapshot, so the export is the live
  file. Retries are disabled. A `git_push` row is inserted with this
  job id before the remote call, and a later attempt that finds the
  row does not commit again.

  The snapshot stores pages and shapes, not the file name and not a
  copy of media bytes. The name in the export is the current file
  name. Media binaries are read from current storage.

  The export type is `:link-later`. `:detach-libraries` would rewrite
  component references, and `:include-libraries` would put more than
  one file in the zip."
  (:require
   [app.binfile.common :as bfc]
   [app.binfile.v3 :as v3]
   [app.common.exceptions :as ex]
   [app.common.logging :as l]
   [app.common.schema :as sm]
   [app.common.uuid :as uuid]
   [app.config :as cf]
   [app.db :as db]
   [app.features.file-snapshots :as fsnap]
   [app.git.remote :as remote]
   [app.git.token :as token]
   [app.http.client :as-alias http.client]
   [app.jobs :as jobs]
   [app.metrics :as mtx]
   [app.rpc.commands.files :as files]
   [app.setup :as-alias setup]
   [app.storage :as sto]
   [app.storage.tmp :as tmp]
   [clojure.java.io :as jio]
   [cuerdas.core :as str]
   [datoteka.fs :as fs]
   [integrant.core :as ig])
  (:import
   java.io.ByteArrayOutputStream
   java.util.zip.ZipEntry
   java.util.zip.ZipFile
   org.postgresql.util.PSQLException))

(def schema:params
  [:map {:title "git-push-params" :closed true}
   [:git-link-id ::sm/uuid]
   [:commit-message ::sm/text]
   [:profile-id ::sm/uuid]
   [:snapshot-id {:optional true} ::sm/uuid]])

(defn snapshot-file
  "Overlay a snapshot onto the current file.

  The snapshot has no file name, so `:name` stays the current one.
  Callers that export this map still read media from current storage."
  [cfg file-id snapshot-id]
  (let [current  (bfc/get-file cfg file-id {:realize? true :include-deleted? true})
        snapshot (fsnap/get-snapshot cfg file-id snapshot-id)]
    (when-not snapshot
      (ex/raise :type :not-found
                :code :snapshot-not-found
                :hint "unable to find snapshot"
                :snapshot-id snapshot-id
                :file-id file-id))
    (cond-> (assoc current
                   :data (:data snapshot)
                   :features (:features snapshot)
                   :version (:version snapshot)
                   :revn (:revn snapshot))
      (some? (:migrations snapshot))
      (assoc :migrations (:migrations snapshot)))))

(defn- read-export
  [path]
  (with-open [zip (ZipFile. (jio/file (str path)))]
    (doall
     (for [^ZipEntry entry (enumeration-seq (.entries zip))
           :when (not (.isDirectory entry))]
       (let [bytes (with-open [in  (.getInputStream zip entry)
                               out (ByteArrayOutputStream.)]
                     (jio/copy in out)
                     (.toByteArray out))
             name  (.getName entry)]
         {:path name
          :json? (str/ends-with? name ".json")
          :bytes bytes})))))

(defn- error-text
  [cause]
  (let [data (ex-data cause)]
    (str/prune (or (:hint data)
                   (some-> (:code data) name)
                   (ex-message cause)
                   "git push failed")
               500)))

(defn- claim-push!
  "Insert the running row. Return `::skipped` when this job id already
  has one, so a retry does not commit a second time."
  [cfg {:keys [job-id link-id commit-message]}]
  (if (and job-id (db/get* cfg :git-push {:job-id job-id}))
    ::skipped
    (try
      (db/insert! cfg :git-push
                  {:id (uuid/next)
                   :git-link-id link-id
                   :job-id job-id
                   :status "running"
                   :commit-message commit-message})
      (catch PSQLException cause
        (if (db/duplicate-key-error? cause)
          ::skipped
          (throw cause))))))

(defn- finish-push!
  [cfg row status extra]
  (db/update! cfg :git-push
              (merge {:status status} extra)
              {:id (:id row)}))

(defn execute
  "Export the file and push the folder. `context` is the job context."
  [cfg context {:keys [git-link-id commit-message snapshot-id]}]
  (let [profile-id (:profile-id context)
        link       (db/get cfg :git-link {:id git-link-id})
        message    (str/prune (str commit-message) 500)]
    (files/check-edition-permissions! cfg profile-id (:file-id link))
    (let [row (claim-push! cfg {:job-id (:id context)
                                :link-id (:id link)
                                :commit-message message})]
      (if (= row ::skipped)
        {:skipped true}
        (let [output (tmp/tempfile :suffix ".penpot")]
          (try
            (let [export-cfg (-> cfg
                                 (assoc ::bfc/ids #{(:file-id link)})
                                 (assoc ::bfc/export-type :link-later)
                                 (assoc ::bfc/canonical true)
                                 (cond->
                                  snapshot-id
                                   (assoc ::bfc/file-override
                                          (snapshot-file cfg (:file-id link) snapshot-id))))]
              (v3/export-files! export-cfg output))
            (let [result (remote/push-folder
                          cfg
                          {:provider (:provider link)
                           :repo-url (:repo-url link)
                           :branch (:branch link)
                           :folder (:folder link)
                           :token (token/decrypt cfg (:token link))
                           :commit-message message
                           :entries (read-export output)})]
              (finish-push! cfg row "success"
                            {:req-data (db/json (:req-data result))
                             :rsp-data (db/json (:rsp-data result))
                             :error nil})
              result)
            (catch Throwable cause
              (finish-push! cfg row "error" {:error (error-text cause)})
              (throw cause))
            (finally
              (fs/delete output))))))))

(defn enqueue-snapshot-push
  "Queue a push of a named version when the flag is on and the file has
  a link. When the flag is off this does not query the database.

  A failure to enqueue is logged and swallowed. The snapshot is already
  committed, and saving a version must not fail because git is down."
  [cfg {:keys [file-id profile-id snapshot]}]
  (when (contains? cf/flags :git-integration)
    (try
      (when-let [link (db/get* cfg :git-link {:file-id file-id})]
        (jobs/submit cfg
                     {::jobs/name :git-push
                      ::jobs/max-retries 0
                      ::jobs/profile-id profile-id
                      ::jobs/params {:git-link-id (:id link)
                                     :commit-message (:label snapshot)
                                     :snapshot-id (:id snapshot)
                                     :profile-id profile-id}}))
      (catch Throwable cause
        (l/err :hint "git push enqueue failed"
               :file-id (str file-id)
               :cause cause)))))

(defn- http-client?
  [params]
  (some? (::http.client/client params)))

(defmethod ig/assert-key ::job-def
  [_ params]
  (assert (db/pool? (::db/pool params)) "expected a valid database pool")
  (assert (sto/valid-storage? (::sto/storage params)) "expected valid storage")
  (assert (http-client? params) "expected http client")
  (assert (map? (::setup/props params)) "expected setup props")
  (assert (mtx/metrics? (::mtx/metrics params)) "expected valid metrics"))

(defmethod ig/init-key ::job-def
  [_ cfg]
  {::jobs/name :git-push
   ::jobs/schema schema:params
   ::jobs/handler (fn [context params]
                    (execute cfg context params))
   ::jobs/decoder (sm/decoder schema:params sm/json-transformer)
   ::jobs/validator (sm/validator schema:params)})
