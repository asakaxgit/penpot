;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS SUBSIDIARY SL

(ns app.git.remote
  "Push a folder to GitHub or GitLab through their REST APIs.

  No git binary and no extra library. The provider is the one stored on
  the link. It is not guessed from the host, so a self-hosted GitHub
  Enterprise or GitLab is addressed by the repository URL plus that
  provider."
  (:require
   [app.common.exceptions :as ex]
   [app.common.json :as json]
   [app.common.time :as ct]
   [app.http.client :as http]
   [clojure.java.io :as jio]
   [cuerdas.core :as str])
  (:import
   java.io.ByteArrayOutputStream
   java.io.InputStream
   java.net.URI
   java.net.URLEncoder
   java.util.Base64))

(def ^:private timeout (ct/duration "60s"))

(def ^:private github-api-version "2022-11-28")

(defn- url-encode
  [value]
  (-> (URLEncoder/encode (str value) "UTF-8")
      (str/replace "+" "%20")))

(defn- fail!
  [code hint]
  (ex/raise :type :validation
            :code code
            :hint hint))

(defn valid-folder?
  "A non-empty folder, with no leading or trailing slash and no parent
  segment. The root is rejected so a push cannot replace the whole
  repository."
  [folder]
  (and (string? folder)
       (not (str/blank? folder))
       (not (str/starts-with? folder "/"))
       (not (str/ends-with? folder "/"))
       (not (str/includes? folder "//"))
       (not (some #(or (= % "..") (= % "."))
                  (str/split folder "/")))))

(defn valid-branch?
  [branch]
  (and (string? branch)
       (not (str/blank? branch))
       (not (str/starts-with? branch "/"))
       (not (str/ends-with? branch "/"))
       (not (str/includes? branch ".."))))

(defn parse-repo-url
  "Check an https repository URL and return its host and path.

  `provider` is `github` or `gitlab`. GitHub needs `owner/repo`. GitLab
  needs at least `group/project` and may include nested groups."
  [provider repo-url]
  (let [^URI uri (try
                   (URI. repo-url)
                   (catch Exception _
                     nil))]
    (when-not (and (some? uri) (= "https" (.getScheme uri)))
      (fail! :git-repo-url "repository URL must start with https://"))
    (when (some? (.getUserInfo uri))
      (fail! :git-repo-url "repository URL must not include credentials"))
    (let [host (.getHost uri)
          port (.getPort uri)
          path (-> (or (.getPath uri) "")
                   (str/replace #"^/" "")
                   (str/replace #"\.git$" "")
                   (str/replace #"/$" ""))
          parts (->> (str/split path "/")
                     (remove str/blank?)
                     vec)]
      (when (or (str/blank? host) (< (count parts) 2))
        (fail! :git-repo-url "repository URL is missing the project path"))
      (when (and (= provider "github") (not= 2 (count parts)))
        (fail! :git-repo-url "a GitHub URL must be https://host/owner/repo"))
      {:host (if (pos? port) (str host ":" port) host)
       :path (str/join "/" parts)})))

(defn- api-base
  [provider host]
  (case provider
    "github" (if (= host "github.com")
               "https://api.github.com"
               (str "https://" host "/api/v3"))
    "gitlab" (str "https://" host "/api/v4")
    (fail! :git-repo-url "unsupported git provider")))

(defn- header-value
  [rsp name]
  (let [target (str/lower name)]
    (some (fn [[k v]]
            (when (= target (str/lower (str k)))
              (cond
                (string? v) v
                (sequential? v) (first v)
                (some? v) (str v))))
          (:headers rsp))))

(defn- decode-body
  [rsp]
  (let [body (:body rsp)]
    (when-not (str/blank? (str body))
      (json/decode body :key-fn keyword))))

(defn- call!
  [cfg request _token {:keys [branch-lookup?]}]
  (let [rsp    (http/req cfg (assoc request :timeout timeout))
        status (:status rsp)]
    (cond
      (and branch-lookup? (= status 404))
      (fail! :git-branch-not-found "the branch does not exist")

      (not (<= 200 status 299))
      (fail! :git-remote-error
             (str "the git provider rejected the request (" status ")"))

      :else
      {:status status
       :headers (:headers rsp)
       :data (decode-body rsp)})))

(defn- github-headers
  [token]
  {"Authorization" (str "Bearer " token)
   "Accept" "application/vnd.github+json"
   "X-GitHub-Api-Version" github-api-version
   "User-Agent" "penpot"
   "Content-Type" "application/json"})

(defn- gitlab-headers
  [token]
  {"PRIVATE-TOKEN" token
   "User-Agent" "penpot"
   "Content-Type" "application/json"})

(defn- bytes->b64
  [^bytes data]
  (String. (.encode (java.util.Base64/getEncoder) data) "UTF-8"))

(defn- entry-content
  [{:keys [json? bytes]}]
  (if json?
    {:content (String. ^bytes bytes "UTF-8")
     :encoding "utf-8"}
    {:content (bytes->b64 bytes)
     :encoding "base64"}))

(defn- prefixed
  [folder path]
  (str folder "/" path))

(defn- github-push
  [cfg {:keys [repo-url branch folder token entries commit-message]}]
  (let [{:keys [host path]} (parse-repo-url "github" repo-url)
        base    (api-base "github" host)
        headers (github-headers token)
        repo    (->> (str/split path "/")
                     (map url-encode)
                     (str/join "/"))
        root    (str base "/repos/" repo)
        ref-uri (str root "/git/refs/heads/" (url-encode branch))
        ref     (call! cfg {:method :get :uri ref-uri :headers headers}
                       token {:branch-lookup? true})
        commit-sha (get-in ref [:data :object :sha])
        commit  (call! cfg {:method :get
                            :uri (str root "/git/commits/" commit-sha)
                            :headers headers}
                       token {})
        tree-sha (get-in commit [:data :tree :sha])
        tree    (call! cfg {:method :get
                            :uri (str root "/git/trees/" tree-sha "?recursive=1")
                            :headers headers}
                       token {})]
    (when (:truncated (:data tree))
      (fail! :git-tree-truncated "the repository tree is too large to update"))
    (let [prefix     (str folder "/")
          wanted     (into {}
                           (map (fn [entry]
                                  [(prefixed folder (:path entry)) entry]))
                           entries)
          existing   (->> (:tree (:data tree))
                          (filter #(and (= "blob" (:type %))
                                        (str/starts-with? (:path %) prefix)))
                          (map :path)
                          set)
          blob-sha   (fn [entry]
                       (let [body (merge {:encoding "utf-8"} (entry-content entry))
                             rsp  (call! cfg {:method :post
                                              :uri (str root "/git/blobs")
                                              :headers headers
                                              :body (json/encode body)}
                                         token {})]
                         (:sha (:data rsp))))
          tree-items (vec
                      (concat
                       (for [[path entry] wanted]
                         {:path path
                          :mode "100644"
                          :type "blob"
                          :sha (blob-sha entry)})
                       (for [path (remove wanted existing)]
                         {:path path
                          :mode "100644"
                          :type "blob"
                          :sha nil})))
          new-tree   (call! cfg {:method :post
                                 :uri (str root "/git/trees")
                                 :headers headers
                                 :body (json/encode {:base_tree tree-sha
                                                     :tree tree-items})}
                            token {})
          new-commit (call! cfg {:method :post
                                 :uri (str root "/git/commits")
                                 :headers headers
                                 :body (json/encode {:message commit-message
                                                     :tree (:sha (:data new-tree))
                                                     :parents [commit-sha]})}
                            token {})
          new-sha    (:sha (:data new-commit))]
      (call! cfg {:method :patch
                  :uri ref-uri
                  :headers headers
                  :body (json/encode {:sha new-sha})}
             token {})
      {:commit-sha new-sha
       :req-data {:method "PATCH" :uri ref-uri}
       :rsp-data {:status 200 :commit-sha new-sha}})))

(defn- gitlab-project
  [path]
  (url-encode path))

(defn- gitlab-tree
  [cfg {:keys [root headers folder branch]}]
  (loop [page 1
         acc  []]
    (when (> page 20)
      (fail! :git-tree-truncated "the repository tree is too large to update"))
    (let [uri (str root "/repository/tree?path=" (url-encode folder)
                   "&recursive=true&ref=" (url-encode branch)
                   "&per_page=100&page=" page)
          rsp (http/req cfg {:method :get :uri uri :headers headers :timeout timeout})
          status (:status rsp)]
      (cond
        (and (= page 1) (= status 404))
        []

        (not (<= 200 status 299))
        (fail! :git-remote-error
               (str "the git provider rejected the request (" status ")"))

        :else
        (let [rows (or (decode-body rsp) [])
              acc  (into acc rows)
              next (header-value rsp "x-next-page")]
          (if (str/blank? next)
            acc
            (recur (inc page) acc)))))))

(defn- gitlab-push
  [cfg {:keys [repo-url branch folder token entries commit-message]}]
  (let [{:keys [host path]} (parse-repo-url "gitlab" repo-url)
        base    (api-base "gitlab" host)
        headers (gitlab-headers token)
        root    (str base "/projects/" (gitlab-project path))
        _       (call! cfg {:method :get
                            :uri (str root "/repository/branches/" (url-encode branch))
                            :headers headers}
                       token {:branch-lookup? true})
        listed  (gitlab-tree cfg {:root root
                                  :headers headers
                                  :folder folder
                                  :branch branch})
        prefix  (str folder "/")
        wanted  (into {}
                      (map (fn [entry]
                             [(prefixed folder (:path entry)) entry]))
                      entries)
        existing (->> listed
                      (filter #(and (= "blob" (:type %))
                                    (str/starts-with? (:path %) prefix)))
                      (map :path)
                      set)
        actions  (vec
                  (concat
                   (for [[path entry] wanted]
                     (let [content (entry-content entry)]
                       {:action (if (contains? existing path) "update" "create")
                        :file_path path
                        :content (:content content)
                        :encoding (if (= "base64" (:encoding content)) "base64" "text")}))
                   (for [path (remove wanted existing)]
                     {:action "delete"
                      :file_path path})))]
    (when (empty? actions)
      (fail! :git-empty-export "the export produced no files"))
    (let [rsp (call! cfg {:method :post
                          :uri (str root "/repository/commits")
                          :headers headers
                          :body (json/encode {:branch branch
                                              :commit_message commit-message
                                              :actions actions})}
                     token {})
          sha (or (:id (:data rsp)) (:short_id (:data rsp)))]
      {:commit-sha (str sha)
       :req-data {:method "POST" :uri (str root "/repository/commits")}
       :rsp-data {:status (:status rsp) :commit-sha (str sha)}})))

(defn- body-bytes
  [body]
  (cond
    (bytes? body)
    body

    (string? body)
    (.getBytes ^String body "UTF-8")

    (instance? InputStream body)
    (with-open [^InputStream in body
                out (ByteArrayOutputStream.)]
      (jio/copy in out)
      (.toByteArray out))

    :else
    (fail! :git-remote-error "the git provider returned an empty file")))

(defn- decode-b64
  [value]
  (try
    (.decode (Base64/getDecoder) ^String (str/replace (str value) #"\s" ""))
    (catch IllegalArgumentException _
      (fail! :git-remote-error "the git provider returned a blob that is not base64"))))

(defn- github-blob
  [cfg {:keys [root headers token sha max-entry-bytes]}]
  (let [rsp  (call! cfg {:method :get
                         :uri (str root "/git/blobs/" sha)
                         :headers headers}
                    token {})
        data (:data rsp)
        bytes (cond
                (= "base64" (:encoding data)) (decode-b64 (:content data))
                (= "utf-8" (:encoding data)) (.getBytes ^String (:content data) "UTF-8")
                :else (fail! :git-remote-error "unsupported blob encoding"))]
    (when (and max-entry-bytes (> (alength ^bytes bytes) max-entry-bytes))
      (fail! :git-entry-too-large "a file in the folder is too large"))
    bytes))

(defn- github-pull
  [cfg {:keys [repo-url branch folder token max-entry-bytes]}]
  (let [{:keys [host path]} (parse-repo-url "github" repo-url)
        base     (api-base "github" host)
        headers  (github-headers token)
        repo     (->> (str/split path "/")
                      (map url-encode)
                      (str/join "/"))
        root     (str base "/repos/" repo)
        ref-uri  (str root "/git/refs/heads/" (url-encode branch))
        ref      (call! cfg {:method :get :uri ref-uri :headers headers}
                        token {:branch-lookup? true})
        commit   (call! cfg {:method :get
                             :uri (str root "/git/commits/" (get-in ref [:data :object :sha]))
                             :headers headers}
                        token {})
        tree-sha (get-in commit [:data :tree :sha])
        tree-uri (str root "/git/trees/" tree-sha "?recursive=1")
        tree     (call! cfg {:method :get :uri tree-uri :headers headers}
                        token {})]
    (when (:truncated (:data tree))
      (fail! :git-tree-truncated "the repository tree is too large to read"))
    (let [prefix (str folder "/")
          blobs  (->> (:tree (:data tree))
                      (filter #(and (= "blob" (:type %))
                                    (str/starts-with? (:path %) prefix)))
                      vec)]
      (when (empty? blobs)
        (fail! :git-empty-export "the folder is empty"))
      {:entries (mapv (fn [item]
                        {:path (subs (:path item) (count prefix))
                         :bytes (github-blob cfg {:root root
                                                  :headers headers
                                                  :token token
                                                  :sha (:sha item)
                                                  :max-entry-bytes max-entry-bytes})})
                      blobs)
       :req-data {:method "GET" :uri tree-uri}
       :rsp-data {:status 200 :count (count blobs)}})))

(defn- gitlab-raw
  [cfg {:keys [root headers path branch max-entry-bytes]}]
  (let [uri (str root "/repository/files/" (url-encode path) "/raw?ref=" (url-encode branch))
        rsp (http/req cfg {:method :get :uri uri :headers headers :timeout timeout}
                      {:response-type :input-stream})
        status (:status rsp)]
    (if (<= 200 status 299)
      (let [bytes (body-bytes (:body rsp))]
        (when (and max-entry-bytes (> (alength ^bytes bytes) max-entry-bytes))
          (fail! :git-entry-too-large "a file in the folder is too large"))
        bytes)
      (do
        (when (instance? InputStream (:body rsp))
          (.close ^InputStream (:body rsp)))
        (fail! :git-remote-error
               (str "the git provider rejected the request (" status ")"))))))

(defn- gitlab-pull
  [cfg {:keys [repo-url branch folder token max-entry-bytes]}]
  (let [{:keys [host path]} (parse-repo-url "gitlab" repo-url)
        base    (api-base "gitlab" host)
        headers (gitlab-headers token)
        root    (str base "/projects/" (gitlab-project path))
        _       (call! cfg {:method :get
                            :uri (str root "/repository/branches/" (url-encode branch))
                            :headers headers}
                       token {:branch-lookup? true})
        listed  (gitlab-tree cfg {:root root
                                  :headers headers
                                  :folder folder
                                  :branch branch})
        prefix  (str folder "/")
        blobs   (->> listed
                     (filter #(and (= "blob" (:type %))
                                   (str/starts-with? (:path %) prefix)))
                     vec)]
    (when (empty? blobs)
      (fail! :git-empty-export "the folder is empty"))
    {:entries (mapv (fn [item]
                      {:path (subs (:path item) (count prefix))
                       :bytes (gitlab-raw cfg {:root root
                                               :headers headers
                                               :path (:path item)
                                               :branch branch
                                               :max-entry-bytes max-entry-bytes})})
                    blobs)
     :req-data {:method "GET" :uri (str root "/repository/tree")}
     :rsp-data {:status 200 :count (count blobs)}}))

(defn pull-folder
  "Read the files under `folder` on `branch`.

  Paths in `:entries` are relative to the folder. The recorded request
  is the method and URI only, never the token."
  [cfg {:keys [provider] :as params}]
  (case provider
    "github" (github-pull cfg params)
    "gitlab" (gitlab-pull cfg params)
    (fail! :git-repo-url "unsupported git provider")))

(defn push-folder
  "Commit `entries` under `folder` on `branch`.

  Each entry is `{:path :bytes :json?}`. Paths are relative to the
  folder. Files already in the folder and absent from `entries` are
  deleted. The returned maps record the method and URI only, never the
  token."
  [cfg {:keys [provider] :as params}]
  (when (empty? (:entries params))
    (fail! :git-empty-export "the export produced no files"))
  (case provider
    "github" (github-push cfg params)
    "gitlab" (gitlab-push cfg params)
    (fail! :git-repo-url "unsupported git provider")))
