;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS SUBSIDIARY SL

(ns app.rpc.commands.git
  "Link a file to a Git repository and queue a push.

  The token is encrypted before it is stored and is never returned.
  Every command checks the `git-integration` flag, which is off unless
  the instance sets `enable-git-integration`."
  (:require
   [app.common.exceptions :as ex]
   [app.common.schema :as sm]
   [app.common.time :as ct]
   [app.common.uuid :as uuid]
   [app.config :as cf]
   [app.db :as db]
   [app.git.remote :as remote]
   [app.git.token :as token]
   [app.jobs :as jobs]
   [app.loggers.audit :as-alias audit]
   [app.rpc :as-alias rpc]
   [app.rpc.commands.files :as files]
   [app.rpc.doc :as-alias doc]
   [app.rpc.helpers :as rph]
   [app.util.services :as sv]
   [cuerdas.core :as str])
  (:import
   org.postgresql.util.PSQLException))

(def ^:private providers #{"github" "gitlab"})

(def ^:private sql:get-links
  "SELECT l.id, l.file_id, l.provider, l.repo_url, l.branch, l.folder,
          l.created_at, l.updated_at,
          (l.token IS NOT NULL) AS token_set,
          p.status AS last_push_status,
          p.created_at AS last_push_at,
          p.commit_message AS last_push_message,
          p.error AS last_push_error,
          p.direction AS last_direction
     FROM git_link AS l
     LEFT JOIN LATERAL (
       SELECT status, created_at, commit_message, error, direction
         FROM git_push
        WHERE git_link_id = l.id
        ORDER BY created_at DESC
        LIMIT 1
     ) AS p ON true
    WHERE l.file_id = ?")

(defn- check-enabled!
  []
  (when-not (contains? cf/flags :git-integration)
    (ex/raise :type :restriction
              :code :git-integration-disabled
              :hint "git integration is disabled")))

(defn- check-folder!
  [folder]
  (when-not (remote/valid-folder? folder)
    (ex/raise :type :validation
              :code :git-folder
              :hint "folder must be a non-empty path without a leading slash")))

(defn- check-branch!
  [branch]
  (when-not (remote/valid-branch? branch)
    (ex/raise :type :validation
              :code :git-branch
              :hint "branch name is empty or invalid")))

(defn- normalize-link
  [provider repo-url branch folder]
  (check-branch! branch)
  (check-folder! folder)
  (let [parsed (remote/parse-repo-url provider repo-url)]
    {:provider provider
     :repo-url (str "https://" (:host parsed) "/" (:path parsed))
     :branch (str/trim branch)
     :folder (str/trim folder)}))

(defn- read-links
  [conn file-id]
  (db/exec! conn [sql:get-links file-id]))

(defn- present
  [row]
  (dissoc row :token))

(defn- insert-link!
  [conn params ciphertext]
  (try
    (db/insert! conn :git-link
                {:id (uuid/next)
                 :file-id (:file-id params)
                 :profile-id (:profile-id params)
                 :provider (:provider params)
                 :repo-url (:repo-url params)
                 :branch (:branch params)
                 :folder (:folder params)
                 :token ciphertext
                 :created-at (ct/now)
                 :updated-at (ct/now)})
    (catch PSQLException cause
      (if (db/duplicate-key-error? cause)
        (ex/raise :type :validation
                  :code :git-link-exists
                  :hint "this file already has a git link"
                  :cause cause)
        (throw cause)))))

(def ^:private schema:create-git-link
  [:map {:title "create-git-link"}
   [:file-id ::sm/uuid]
   [:provider [::sm/one-of {:format "string"} providers]]
   [:repo-url :string]
   [:branch :string]
   [:folder :string]
   [:token [:string {:min 1 :max 4096}]]])

(sv/defmethod ::create-git-link
  {::doc/added "2.15"
   ::sm/params schema:create-git-link}
  [{:keys [::db/pool] :as cfg} {:keys [::rpc/profile-id file-id] :as params}]
  (check-enabled!)
  (files/check-edition-permissions! pool profile-id file-id)
  (let [link       (normalize-link (:provider params) (:repo-url params)
                                   (:branch params) (:folder params))
        ciphertext (token/encrypt cfg (:token params))]
    (insert-link! pool (assoc link :file-id file-id :profile-id profile-id) ciphertext)
    (rph/with-meta (first (read-links pool file-id))
      {::audit/props {:file-id file-id}})))

(def ^:private schema:update-git-link
  [:map {:title "update-git-link"}
   [:id ::sm/uuid]
   [:provider [::sm/one-of {:format "string"} providers]]
   [:repo-url :string]
   [:branch :string]
   [:folder :string]
   [:token {:optional true} [:string {:min 1 :max 4096}]]])

(sv/defmethod ::update-git-link
  {::doc/added "2.15"
   ::sm/params schema:update-git-link}
  [{:keys [::db/pool] :as cfg} {:keys [::rpc/profile-id id token] :as params}]
  (check-enabled!)
  (let [current (db/get pool :git-link {:id id})]
    (files/check-edition-permissions! pool profile-id (:file-id current))
    (let [link       (normalize-link (:provider params) (:repo-url params)
                                     (:branch params) (:folder params))
          ciphertext (if (and (string? token) (not (str/blank? token)))
                       (token/encrypt cfg token)
                       (:token current))]
      (db/update! pool :git-link
                  (assoc link :token ciphertext :updated-at (ct/now))
                  {:id id})
      (rph/with-meta (first (read-links pool (:file-id current)))
        {::audit/props {:file-id (:file-id current)}}))))

(def ^:private schema:delete-git-link
  [:map {:title "delete-git-link"}
   [:id ::sm/uuid]])

(sv/defmethod ::delete-git-link
  {::doc/added "2.15"
   ::sm/params schema:delete-git-link}
  [{:keys [::db/pool]} {:keys [::rpc/profile-id id]}]
  (check-enabled!)
  (let [current (db/get pool :git-link {:id id})]
    (files/check-edition-permissions! pool profile-id (:file-id current))
    (db/delete! pool :git-link {:id id})
    (rph/with-meta (rph/wrap)
      {::audit/props {:file-id (:file-id current)}})))

(def ^:private schema:get-git-links
  [:map {:title "get-git-links"}
   [:file-id ::sm/uuid]])

(sv/defmethod ::get-git-links
  {::doc/added "2.15"
   ::sm/params schema:get-git-links}
  [{:keys [::db/pool] :as cfg} {:keys [::rpc/profile-id file-id]}]
  (check-enabled!)
  (files/check-read-permissions! cfg profile-id file-id)
  (mapv present (read-links pool file-id)))

(def ^:private schema:push-git-link
  [:map {:title "push-git-link"}
   [:id ::sm/uuid]])

(sv/defmethod ::push-git-link
  {::doc/added "2.15"
   ::sm/params schema:push-git-link}
  [{:keys [::db/pool] :as cfg} {:keys [::rpc/profile-id id]}]
  (check-enabled!)
  (let [current (db/get pool :git-link {:id id})]
    (files/check-edition-permissions! pool profile-id (:file-id current))
    (let [job-id (jobs/submit cfg
                              {::jobs/name :git-push
                               ::jobs/max-retries 0
                               ::jobs/profile-id profile-id
                               ::jobs/params {:git-link-id id
                                              :commit-message "Push now"
                                              :profile-id profile-id}})]
      (rph/with-meta {:job-id job-id}
        {::audit/props {:file-id (:file-id current)}}))))

(def ^:private schema:pull-git-link
  [:map {:title "pull-git-link"}
   [:id ::sm/uuid]])

(sv/defmethod ::pull-git-link
  {::doc/added "2.15"
   ::sm/params schema:pull-git-link}
  [{:keys [::db/pool] :as cfg} {:keys [::rpc/profile-id id]}]
  (check-enabled!)
  (let [current (db/get pool :git-link {:id id})]
    (files/check-edition-permissions! pool profile-id (:file-id current))
    (let [job-id (jobs/submit cfg
                              {::jobs/name :git-pull
                               ::jobs/max-retries 0
                               ::jobs/profile-id profile-id
                               ::jobs/params {:git-link-id id
                                              :profile-id profile-id}})]
      (rph/with-meta {:job-id job-id}
        {::audit/props {:file-id (:file-id current)}}))))
