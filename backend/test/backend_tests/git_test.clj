;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS SUBSIDIARY SL

(ns backend-tests.git-test
  (:require
   [app.binfile.common :as bfc]
   [app.common.features :as cfeat]
   [app.common.json :as json]
   [app.common.uuid :as uuid]
   [app.config :as cf]
   [app.db :as db]
   [app.git.remote :as remote]
   [app.git.token :as token]
   [app.http.client :as http]
   [app.rpc :as-alias rpc]
   [app.tasks.git-push :as git-push]
   [backend-tests.helpers :as th]
   [clojure.test :as t]
   [cuerdas.core :as str]))

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

(defn- decode-params
  [value]
  (if (db/pgobject? value)
    (db/decode-json-pgobject value)
    value))

(defn- raise-code
  [f]
  (try
    (f)
    nil
    (catch clojure.lang.ExceptionInfo cause
      cause)))

(defn- github-mock
  [calls]
  (fn [_cfg {:keys [method uri body headers]}]
    (swap! calls conj {:method method :uri uri :body body :headers headers})
    (cond
      (and (= method :get) (str/includes? uri "/git/refs/"))
      {:status 200
       :headers {}
       :body (json/encode {:object {:sha "c1" :type "commit"}})}

      (and (= method :get) (str/includes? uri "/git/commits/"))
      {:status 200
       :headers {}
       :body (json/encode {:sha "c1" :tree {:sha "t1"}})}

      (and (= method :get) (str/includes? uri "/git/trees/"))
      {:status 200
       :headers {}
       :body (json/encode {:sha "t1"
                           :truncated false
                           :tree [{:path "penpot/obsolete.json"
                                   :mode "100644"
                                   :type "blob"
                                   :sha "old"}]})}

      (and (= method :post) (str/includes? uri "/git/blobs"))
      {:status 201
       :headers {}
       :body (json/encode {:sha (str "blob-" (count @calls))})}

      (and (= method :post) (str/includes? uri "/git/trees"))
      (let [tree (:tree (json/decode body :key-fn keyword))]
        (t/is (some #(= "penpot/manifest.json" (:path %)) tree))
        (t/is (some #(and (= "penpot/obsolete.json" (:path %))
                          (nil? (:sha %)))
                    tree))
        {:status 201 :headers {} :body (json/encode {:sha "t2"})})

      (and (= method :post) (str/includes? uri "/git/commits"))
      {:status 201 :headers {} :body (json/encode {:sha "c2"})}

      (and (= method :patch) (str/includes? uri "/git/refs/"))
      {:status 200 :headers {} :body (json/encode {:object {:sha "c2"}})}

      :else
      {:status 500 :headers {} :body "{}"})))

(defn- gitlab-mock
  [calls]
  (fn [_cfg {:keys [method uri body headers]}]
    (swap! calls conj {:method method :uri uri :body body :headers headers})
    (cond
      (and (= method :get) (str/includes? uri "/repository/branches/"))
      {:status 200 :headers {} :body (json/encode {:name "main"})}

      (and (= method :get) (str/includes? uri "/repository/tree"))
      {:status 200
       :headers {"x-next-page" ""}
       :body (json/encode [{:path "designs/obsolete.json" :type "blob"}])}

      (and (= method :post) (str/includes? uri "/repository/commits"))
      (let [actions (:actions (json/decode body :key-fn keyword))]
        (t/is (some #(and (= "delete" (:action %))
                          (= "designs/obsolete.json" (:file_path %)))
                    actions))
        (t/is (some #(= "designs/manifest.json" (:file_path %)) actions))
        {:status 201 :headers {} :body (json/encode {:id "gl1"})})

      :else
      {:status 500 :headers {} :body "{}"})))

(t/deftest git-commands-are-off-without-the-flag
  (let [{:keys [profile file]} (world)
        out (create-link! (:id profile) (:id file))]
    (t/is (th/ex-of-code? (:error out) :git-integration-disabled))))

(t/deftest link-crud-permissions-and-encryption
  (with-git
    (let [{:keys [profile team file]} (world)
          viewer (th/create-profile* 2)
          _      (th/create-team-role* {:team-id (:id team)
                                        :profile-id (:id viewer)
                                        :role :viewer})
          denied (create-link! (:id viewer) (:id file))
          bad-url (create-link! (:id profile) (:id file)
                                :repo-url "http://github.com/acme/widgets")
          bad-folder (create-link! (:id profile) (:id file)
                                   :folder "/penpot")
          created (create-link! (:id profile) (:id file))
          again   (create-link! (:id profile) (:id file))
          link    (:result created)
          row     (th/db-exec-one! ["SELECT token FROM git_link WHERE id = ?"
                                    (:id link)])
          listed  (th/command! {::th/type :get-git-links
                                ::rpc/profile-id (:id viewer)
                                :file-id (:id file)})
          updated (th/command! {::th/type :update-git-link
                                ::rpc/profile-id (:id profile)
                                :id (:id link)
                                :provider "github"
                                :repo-url "https://github.com/acme/widgets"
                                :branch "dev"
                                :folder "penpot"})
          row2    (th/db-exec-one! ["SELECT token FROM git_link WHERE id = ?"
                                    (:id link)])]
      (t/is (th/ex-of-code? (:error denied) :object-not-found))
      (t/is (th/ex-of-code? (:error bad-url) :git-repo-url))
      (t/is (th/ex-of-code? (:error bad-folder) :git-folder))
      (t/is (nil? (:error created)))
      (t/is (th/ex-of-code? (:error again) :git-link-exists))
      (t/is (true? (:token-set link)))
      (t/is (nil? (:token link)))
      (t/is (not= "secret-token" (:token row)))
      (t/is (= "secret-token" (token/decrypt th/*system* (:token row))))
      (t/is (nil? (:error listed)))
      (t/is (= 1 (count (:result listed))))
      (t/is (not (str/includes? (pr-str (:result listed)) "secret-token")))
      (t/is (nil? (:error updated)))
      (t/is (= "dev" (:branch (:result updated))))
      (t/is (= "secret-token" (token/decrypt th/*system* (:token row2))))
      (let [pushed (th/command! {::th/type :push-git-link
                                 ::rpc/profile-id (:id profile)
                                 :id (:id link)})
            job    (th/db-exec-one! ["SELECT params FROM job WHERE id = ?"
                                     (get-in pushed [:result :job-id])])]
        (t/is (nil? (:error pushed)))
        (t/is (= "Push now" (:commit-message (decode-params (:params job))))))
      (let [deleted (th/command! {::th/type :delete-git-link
                                  ::rpc/profile-id (:id profile)
                                  :id (:id link)})]
        (t/is (nil? (:error deleted)))
        (t/is (empty? (th/db-exec! ["SELECT id FROM git_link WHERE file_id = ?"
                                    (:id file)])))))))

(t/deftest named-version-enqueues-only-when-linked
  (let [{:keys [profile file]} (world)]
    (with-git
      (t/is (nil? (:error (create-link! (:id profile) (:id file))))))
    (let [off (th/command! {::th/type :create-file-snapshot
                            ::rpc/profile-id (:id profile)
                            :file-id (:id file)
                            :label "while off"})]
      (t/is (nil? (:error off)))
      (t/is (empty? (th/db-exec! ["SELECT id FROM job WHERE name = 'git-push'"]))))
    (with-git
      (let [on (th/command! {::th/type :create-file-snapshot
                             ::rpc/profile-id (:id profile)
                             :file-id (:id file)
                             :label "while on"})
            jobs (th/db-exec! ["SELECT params FROM job WHERE name = 'git-push'"])
            params (decode-params (:params (first jobs)))]
        (t/is (nil? (:error on)))
        (t/is (= 1 (count jobs)))
        (t/is (= (str (get-in on [:result :id])) (str (:snapshot-id params))))
        (t/is (= "while on" (:commit-message params)))))))

(t/deftest snapshot-file-uses-saved-pages-and-current-name
  (let [{:keys [profile file]} (world)
        file-id (:id file)
        loaded  (bfc/get-file th/*system* file-id {:realize? true})
        page-id (first (get-in loaded [:data :pages]))
        old-name (get-in loaded [:data :pages-index page-id :name])
        snap    (th/command! {::th/type :create-file-snapshot
                              ::rpc/profile-id (:id profile)
                              :file-id file-id
                              :label "Named version"})
        _       (th/command! {::th/type :update-file
                              ::rpc/profile-id (:id profile)
                              :id file-id
                              :session-id (uuid/next)
                              :revn 0
                              :vern 0
                              :features cfeat/supported-features
                              :changes [{:type :mod-page
                                         :id page-id
                                         :name "Renamed"}]})
        source  (git-push/snapshot-file th/*system* file-id (get-in snap [:result :id]))
        live    (bfc/get-file th/*system* file-id {:realize? true})]
    (t/is (some? page-id))
    (t/is (nil? (:error snap)))
    (t/is (= old-name (get-in source [:data :pages-index page-id :name])))
    (t/is (= "Renamed" (get-in live [:data :pages-index page-id :name])))
    (t/is (= (:name live) (:name source)))))

(t/deftest github-push-commits-the-folder
  (with-git
    (let [{:keys [profile file]} (world)
          created (create-link! (:id profile) (:id file))
          calls   (atom [])
          job-id  (uuid/next)]
      (t/is (nil? (:error created)))
      (with-redefs [http/req (github-mock calls)]
        (let [result (git-push/execute
                      th/*system*
                      {:id job-id
                       :name "git-push"
                       :label nil
                       :resource-id nil
                       :profile-id (:id profile)}
                      {:git-link-id (get-in created [:result :id])
                       :commit-message "Push now"
                       :profile-id (:id profile)})
              sent   (count @calls)
              again  (git-push/execute
                      th/*system*
                      {:id job-id
                       :name "git-push"
                       :label nil
                       :resource-id nil
                       :profile-id (:id profile)}
                      {:git-link-id (get-in created [:result :id])
                       :commit-message "Push now"
                       :profile-id (:id profile)})
              row    (th/db-exec-one!
                      ["SELECT status, error, req_data::text AS req,
                               rsp_data::text AS rsp
                          FROM git_push WHERE job_id = ?"
                       job-id])]
          (t/is (= "c2" (:commit-sha result)))
          (t/is (true? (:skipped again)))
          (t/is (= sent (count @calls)))
          (t/is (= "success" (:status row)))
          (t/is (not (str/includes? (str (:req row) (:rsp row) (:error row))
                                    "secret-token")))
          (t/is (some #(= "Bearer secret-token"
                          (get-in % [:headers "Authorization"]))
                      @calls)))))))

(t/deftest gitlab-push-commits-the-folder
  (with-git
    (let [{:keys [profile file]} (world)
          created (create-link! (:id profile) (:id file)
                                :provider "gitlab"
                                :repo-url "https://gitlab.com/acme/widgets"
                                :folder "designs")
          calls   (atom [])]
      (t/is (nil? (:error created)))
      (with-redefs [http/req (gitlab-mock calls)]
        (let [result (git-push/execute
                      th/*system*
                      {:id (uuid/next)
                       :name "git-push"
                       :label nil
                       :resource-id nil
                       :profile-id (:id profile)}
                      {:git-link-id (get-in created [:result :id])
                       :commit-message "Push now"
                       :profile-id (:id profile)})]
          (t/is (= "gl1" (:commit-sha result)))
          (t/is (some #(= "secret-token" (get (:headers %) "PRIVATE-TOKEN"))
                      @calls)))))))

(t/deftest missing-branch-is-reported
  (let [entries [{:path "manifest.json"
                  :json? true
                  :bytes (.getBytes "[]" "UTF-8")}]
        cause (with-redefs [http/req (fn [_cfg _request]
                                       {:status 404 :headers {} :body "{}"})]
                (raise-code
                 #(remote/push-folder
                   th/*system*
                   {:provider "github"
                    :repo-url "https://github.com/acme/widgets"
                    :branch "missing"
                    :folder "penpot"
                    :token "secret-token"
                    :commit-message "x"
                    :entries entries})))]
    (t/is (th/ex-of-code? cause :git-branch-not-found))))

(t/deftest truncated-github-tree-is-rejected
  (let [entries [{:path "manifest.json"
                  :json? true
                  :bytes (.getBytes "[]" "UTF-8")}]
        cause (with-redefs [http/req
                            (fn [_cfg {:keys [uri]}]
                              (cond
                                (str/includes? uri "/git/refs/")
                                {:status 200 :headers {}
                                 :body (json/encode {:object {:sha "c1"}})}

                                (str/includes? uri "/git/commits/")
                                {:status 200 :headers {}
                                 :body (json/encode {:sha "c1" :tree {:sha "t1"}})}

                                (str/includes? uri "/git/trees/")
                                {:status 200 :headers {}
                                 :body (json/encode {:truncated true :tree []})}

                                :else
                                {:status 500 :headers {} :body "{}"}))]
                (raise-code
                 #(remote/push-folder
                   th/*system*
                   {:provider "github"
                    :repo-url "https://github.com/acme/widgets"
                    :branch "main"
                    :folder "penpot"
                    :token "secret-token"
                    :commit-message "x"
                    :entries entries})))]
    (t/is (th/ex-of-code? cause :git-tree-truncated))))
