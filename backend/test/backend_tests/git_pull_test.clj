;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS SUBSIDIARY SL

(ns backend-tests.git-pull-test
  (:require
   [app.binfile.common :as bfc]
   [app.binfile.v3 :as v3]
   [app.common.features :as cfeat]
   [app.common.json :as json]
   [app.common.uuid :as uuid]
   [app.config :as cf]
   [app.git.remote :as remote]
   [app.http.client :as http]
   [app.rpc :as-alias rpc]
   [app.storage.tmp :as tmp]
   [app.tasks.git-pull :as git-pull]
   [backend-tests.helpers :as th]
   [clojure.java.io :as jio]
   [clojure.test :as t]
   [cuerdas.core :as str]
   [datoteka.fs :as fs])
  (:import
   java.net.URLEncoder
   java.util.Base64
   java.util.zip.ZipEntry
   java.util.zip.ZipFile))

(t/use-fixtures :once th/state-init)
(t/use-fixtures :each th/database-reset)

(defmacro with-git
  [& body]
  `(with-redefs [cf/flags (conj cf/flags :git-integration)]
     ~@body))

(defn- world
  []
  (let [profile (th/create-profile* 1)
        team    (th/create-team* 1 {:profile-id (:id profile)})
        project (th/create-project* 1 {:profile-id (:id profile)
                                       :team-id (:id team)})
        file    (th/create-file* 1 {:profile-id (:id profile)
                                    :project-id (:id project)})]
    {:profile profile :team team :project project :file file}))

(defn- create-link!
  [profile-id file-id & {:as extra}]
  (th/command! (merge {::th/type :create-git-link
                       ::rpc/profile-id profile-id
                       :file-id file-id
                       :provider "github"
                       :repo-url "https://github.com/acme/widgets"
                       :branch "main"
                       :folder "penpot"
                       :token "secret-token"}
                      extra)))

(defn- export-entries
  [file-id]
  (let [output (tmp/tempfile :suffix ".penpot")]
    (v3/export-files!
     (-> th/*system*
         (assoc ::bfc/ids #{file-id})
         (assoc ::bfc/export-type :link-later)
         (assoc ::bfc/canonical true))
     output)
    (try
      (with-open [zip (ZipFile. (jio/file (str output)))]
        (doall
         (for [^ZipEntry entry (enumeration-seq (.entries zip))
               :when (not (.isDirectory entry))]
           (let [bytes (with-open [in (.getInputStream zip entry)
                                   out (java.io.ByteArrayOutputStream.)]
                         (jio/copy in out)
                         (.toByteArray out))]
             {:path (.getName entry)
              :bytes bytes}))))
      (finally
        (fs/delete output)))))

(defn- b64
  [^bytes data]
  (String. (.encode (Base64/getEncoder) data) "UTF-8"))

(defn- url-encode
  [value]
  (-> (URLEncoder/encode (str value) "UTF-8")
      (str/replace "+" "%20")))

(defn- indexed
  [entries folder]
  (mapv (fn [i entry]
          (assoc entry
                 :sha (str "b" i)
                 :full (str folder "/" (:path entry))))
        (range)
        entries))

(defn- github-pull-mock
  [entries]
  (let [rows (indexed entries "penpot")]
    (fn [_cfg {:keys [uri]} & _]
      (cond
        (str/includes? uri "/git/refs/")
        {:status 200 :headers {}
         :body (json/encode {:object {:sha "c1"}})}

        (str/includes? uri "/git/commits/")
        {:status 200 :headers {}
         :body (json/encode {:sha "c1" :tree {:sha "t1"}})}

        (str/includes? uri "/git/trees/")
        {:status 200 :headers {}
         :body (json/encode {:truncated false
                             :tree (mapv (fn [row]
                                           {:path (:full row)
                                            :type "blob"
                                            :sha (:sha row)})
                                         rows)})}

        (str/includes? uri "/git/blobs/")
        (let [sha   (last (str/split uri "/"))
              entry (first (filter #(= sha (:sha %)) rows))]
          {:status 200 :headers {}
           :body (json/encode {:encoding "base64"
                               :content (b64 (:bytes entry))})})

        :else
        {:status 500 :headers {} :body "{}"}))))

(defn- gitlab-pull-mock
  [entries]
  (let [rows (indexed entries "designs")]
    (fn [_cfg {:keys [uri]} & _]
      (cond
        (str/includes? uri "/repository/branches/")
        {:status 200 :headers {} :body (json/encode {:name "main"})}

        (str/includes? uri "/repository/tree")
        {:status 200
         :headers {"x-next-page" ""}
         :body (json/encode (mapv (fn [row]
                                    {:path (:full row) :type "blob"})
                                  rows))}

        (str/includes? uri "/repository/files/")
        (let [entry (first (filter #(str/includes? uri (url-encode (:full %))) rows))]
          {:status 200 :headers {}
           :body (:bytes entry)})

        :else
        {:status 500 :headers {} :body "{}"}))))

(defn- raise-code
  [f]
  (try
    (f)
    nil
    (catch clojure.lang.ExceptionInfo cause
      cause)))

(defn- page-name
  [file-id]
  (let [loaded  (bfc/get-file th/*system* file-id {:realize? true})
        page-id (first (get-in loaded [:data :pages]))]
    {:page-id page-id
     :name (get-in loaded [:data :pages-index page-id :name])
     :revn (:revn loaded)}))

(defn- rename-page!
  [profile-id file-id page-id revn]
  (th/command! {::th/type :update-file
                ::rpc/profile-id profile-id
                :id file-id
                :session-id (uuid/next)
                :revn revn
                :vern 0
                :features cfeat/supported-features
                :changes [{:type :mod-page
                           :id page-id
                           :name "Renamed"}]}))

(defn- run-pull!
  [profile-id link-id]
  (git-pull/execute
   th/*system*
   {:id (uuid/next)
    :name "git-pull"
    :label nil
    :resource-id nil
    :profile-id profile-id}
   {:git-link-id link-id
    :profile-id profile-id}))

(t/deftest folder-checks-reject-bad-contents
  (let [limits {:max-text-entry 10
                :max-binary-entry 100
                :max-text-total 1000
                :max-entries 10}
        big    {:max-text-entry 100000
                :max-binary-entry 100000
                :max-text-total 100000
                :max-entries 10}
        two    (.getBytes (json/encode {:version 1
                                        :type "penpot/export-files"
                                        :files [{:id "a" :name "A" :features []}
                                                {:id "b" :name "B" :features []}]})
                          "UTF-8")]
    (t/is (th/ex-of-code?
           (raise-code #(git-pull/check-folder!
                         [{:path "manifest.json" :bytes (byte-array 11)}]
                         limits))
           :git-entry-too-large))
    (t/is (th/ex-of-code?
           (raise-code #(git-pull/check-folder!
                         [{:path "readme.txt" :bytes (.getBytes "hi" "UTF-8")}]
                         big))
           :git-manifest-missing))
    (t/is (th/ex-of-code?
           (raise-code #(git-pull/check-folder!
                         [{:path "manifest.json" :bytes two}]
                         big))
           :invalid-condition))
    (t/is (th/ex-of-code?
           (raise-code #(git-pull/check-folder!
                         [{:path "../secret" :bytes (.getBytes "x" "UTF-8")}]
                         big))
           :git-folder))))

(t/deftest pull-commands-follow-the-flag-and-edit-rights
  (let [{:keys [profile team file]} (world)
        viewer (th/create-profile* 2)]
    (th/create-team-role* {:team-id (:id team)
                           :profile-id (:id viewer)
                           :role :viewer})
    (t/is (th/ex-of-code?
           (:error (th/command! {::th/type :pull-git-link
                                 ::rpc/profile-id (:id profile)
                                 :id (uuid/next)}))
           :git-integration-disabled))
    (with-git
      (let [created (create-link! (:id profile) (:id file))
            link-id (get-in created [:result :id])
            denied  (th/command! {::th/type :pull-git-link
                                  ::rpc/profile-id (:id viewer)
                                  :id link-id})
            queued  (th/command! {::th/type :pull-git-link
                                  ::rpc/profile-id (:id profile)
                                  :id link-id})
            job     (th/db-exec-one! ["SELECT name FROM job WHERE id = ?"
                                      (get-in queued [:result :job-id])])]
        (t/is (nil? (:error created)))
        (t/is (th/ex-of-code? (:error denied) :object-not-found))
        (t/is (nil? (:error queued)))
        (t/is (= "git-pull" (:name job)))))))

(defn- pulled-name
  [profile-id file-id link-id mock]
  (let [before  (page-name file-id)
        renamed (rename-page! profile-id file-id (:page-id before) (:revn before))
        mid     (page-name file-id)]
    (with-redefs [http/req mock]
      (let [result (run-pull! profile-id link-id)
            after  (page-name file-id)
            snaps  (th/db-exec! ["SELECT created_by FROM file_change WHERE file_id = ?"
                                 file-id])
            row    (th/db-exec-one!
                    ["SELECT status, direction, req_data::text AS req, error
                        FROM git_push ORDER BY created_at DESC LIMIT 1"])]
        {:before before
         :renamed renamed
         :mid mid
         :after after
         :result result
         :snaps snaps
         :row row}))))

(defn- assert-replaced
  [created seen]
  (t/is (nil? (:error created)))
  (t/is (nil? (:error (:renamed seen))))
  (t/is (= "Renamed" (:name (:mid seen))))
  (t/is (= (:name (:before seen)) (:name (:after seen))))
  (t/is (= (inc (:revn (:mid seen))) (:revn (:after seen))))
  (t/is (some #(= "system" (:created-by %)) (:snaps seen)))
  (t/is (= "success" (:status (:row seen))))
  (t/is (= "pull" (:direction (:row seen))))
  (t/is (not (str/includes? (str (:req (:row seen)) (:error (:row seen)))
                            "secret-token"))))

(t/deftest github-pull-replaces-the-file
  (with-git
    (let [{:keys [profile file]} (world)
          created (create-link! (:id profile) (:id file))
          link-id (get-in created [:result :id])
          entries (export-entries (:id file))
          seen    (pulled-name (:id profile) (:id file) link-id
                               (github-pull-mock entries))]
      (assert-replaced created seen))))

(t/deftest gitlab-pull-replaces-the-file
  (with-git
    (let [{:keys [profile file]} (world)
          created (create-link! (:id profile) (:id file)
                                :provider "gitlab"
                                :repo-url "https://gitlab.com/acme/widgets"
                                :folder "designs")
          entries (export-entries (:id file))
          seen    (pulled-name (:id profile) (:id file)
                               (get-in created [:result :id])
                               (gitlab-pull-mock entries))]
      (assert-replaced created seen))))

(t/deftest pull-of-a-folder-without-a-manifest-does-not-backup
  (with-git
    (let [{:keys [profile file]} (world)
          created (create-link! (:id profile) (:id file))
          cause   (with-redefs [http/req (github-pull-mock
                                          [{:path "readme.txt"
                                            :bytes (.getBytes "hello" "UTF-8")}])]
                    (raise-code #(run-pull! (:id profile)
                                            (get-in created [:result :id]))))
          snaps   (th/db-exec! ["SELECT id FROM file_change WHERE file_id = ? AND created_by = 'system'"
                                (:id file)])
          row     (th/db-exec-one! ["SELECT status, error FROM git_push LIMIT 1"])]
      (t/is (th/ex-of-code? cause :git-manifest-missing))
      (t/is (empty? snaps))
      (t/is (= "error" (:status row))))))

(t/deftest missing-branch-is-not-a-generic-remote-error
  (let [cause (with-redefs [http/req (fn [_cfg _request & _]
                                       {:status 404 :headers {} :body "{}"})]
                (raise-code
                 #(remote/pull-folder
                   th/*system*
                   {:provider "github"
                    :repo-url "https://github.com/acme/widgets"
                    :branch "missing"
                    :folder "penpot"
                    :token "secret-token"
                    :max-entry-bytes 1000})))]
    (t/is (th/ex-of-code? cause :git-branch-not-found))))
