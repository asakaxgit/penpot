;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS SUBSIDIARY SL

(ns app.rpc.commands.binfile
  (:refer-clojure :exclude [assert])
  (:require
   [app.binfile.common :as bfc]
   [app.binfile.v1 :as bf.v1]
   [app.binfile.v3 :as bf.v3]
   [app.common.exceptions :as ex]
   [app.common.schema :as sm]
   [app.common.time :as ct]
   [app.common.uri :as u]
   [app.config :as cf]
   [app.db :as db]
   [app.features.file-snapshots :as fsnap]
   [app.features.logical-deletion :as ldel]
   [app.http.sse :as sse]
   [app.loggers.audit :as audit]
   [app.loggers.webhooks :as-alias webhooks]
   [app.media.validation :as media.v]
   [app.msgbus :as mbus]
   [app.rpc :as-alias rpc]
   [app.rpc.climit :as-alias climit]
   [app.rpc.commands.files :as files]
   [app.rpc.commands.media :as media-cmd]
   [app.rpc.commands.projects :as projects]
   [app.rpc.commands.teams :as teams]
   [app.rpc.doc :as-alias doc]
   [app.storage :as sto]
   [app.storage.tmp :as tmp]
   [app.tasks.file-gc]
   [app.util.services :as sv]
   [app.worker :as-alias wrk]
   [datoteka.fs :as fs]))

(set! *warn-on-reflection* true)

;; --- Command: export-binfile

(def ^:private
  schema:export-binfile
  [:map {:title "export-binfile"}
   [:file-id ::sm/uuid]
   [:type {:optional true} [::sm/one-of #{:include-libraries :merge-libraries :detach-libraries :link-later}]]
   [:include-libraries {:optional true} ::sm/boolean]
   [:embed-assets {:optional true} ::sm/boolean]
   [:canonical {:optional true} ::sm/boolean]])

(defn- export-binfile
  [{:keys [::sto/storage] :as cfg} {:keys [file-id canonical] :as params}]
  (let [output (tmp/tempfile*)]

    (try
      (-> (bfc/export-cfg cfg {:ids #{file-id}
                               :export-type (bfc/resolve-export-type params)})
          (cond-> canonical
            (assoc ::bfc/canonical true))
          (bf.v3/export-files! output))

      (let [data   (sto/content output)
            object (sto/put-object! storage
                                    {::sto/content data
                                     ::sto/touched-at (ct/in-future {:minutes 60})
                                     :content-type "application/zip"
                                     :bucket sto/tempfile-bucket})]

        (-> (cf/get :public-uri)
            (u/ensure-path-slash)
            (u/join "assets/by-id/")
            (u/join (str (:id object)))))

      (finally
        (fs/delete output)))))

(sv/defmethod ::export-binfile
  "Export a penpot file in a binary format.

  `canonical` drops volatile fields (revn, vern, timestamps, metadata,
  manifest generatedBy, thumbnails), sorts keys and lists, and stamps
  zip entries with a fixed time so two exports of the same file match."
  {::doc/added "1.15"
   ::doc/changes [["2.12" "Remove version parameter, only one version is supported"]
                  ["2.19" "Deprecated `include-libraries` and `embed-assets` params"]]
   ::webhooks/event? true
   ::sm/params schema:export-binfile}
  [cfg {:keys [::rpc/profile-id file-id] :as params}]
  (files/check-read-permissions! cfg profile-id file-id)
  (sse/response (partial export-binfile cfg params)))

;; --- Command: import-binfile

(defn- import-binfile
  [{:keys [::db/pool] :as cfg} {:keys [profile-id project-id version name file upload-id] :as params}]
  (let [team
        (teams/get-team pool
                        :profile-id profile-id
                        :project-id project-id)

        cfg
        (bfc/import-cfg cfg {:profile-id profile-id
                             :project-id project-id
                             :team team
                             :name name
                             :input (:path file)})

        owned? (some? upload-id)

        result
        (try
          (case (int version)
            1 (bf.v1/import-files! cfg)
            3 (bf.v3/import-files! cfg)
            (throw (ex-info (str "Unsupported binfile version: " version)
                            {:type :validation
                             :code :unsupported-version
                             :version version})))
          (finally
            (when owned?
              (fs/delete (:path file)))))]

    (db/update! pool :project
                {:modified-at (ct/now)}
                {:id project-id}
                {::db/return-keys false})

    ;; The imported files get no event from their own command, so emit one per
    ;; file inside a single transaction.
    (db/tx-run! cfg
                (fn [cfg]
                  (doseq [file-id (:file-ids result)]
                    (audit/submit-file-created-event cfg params file-id
                                                     {:team-id (:id team)}))))

    result))

(def ^:private schema:import-binfile
  [:and
   [:map {:title "import-binfile" :closed true}
    [:name {:optional true}
     [:or [:string {:max 250}]
      [:map-of ::sm/uuid [:string {:max 250}]]]]
    [:project-id {:optional true} ::sm/uuid]
    ;; Overwrite target. This is an existing file the caller can edit.
    ;; It is not an id for a newly inserted file.
    [:file-id {:optional true} ::sm/uuid]
    [:version {:optional true} [:enum 1 3]]
    [:file {:optional true} media.v/schema:upload]
    [:upload-id {:optional true} ::sm/uuid]]
   [:fn {:error/message "one of :file or :upload-id is required"}
    (fn [{:keys [file upload-id]}]
      (or (some? file) (some? upload-id)))]
   [:fn {:error/message ":name and :project-id are required unless :file-id is set"}
    (fn [{:keys [name project-id file-id]}]
      (or (some? file-id)
          (and (some? name) (some? project-id))))]])

(defn overwrite-file
  "Replace one existing file with the single file inside a v3 binfile.

  A system snapshot of the current file is committed first, the same way
  a version restore does. revn increases by one. Open editors receive the
  same `:file-restored` message a version restore sends.

  The caller has already checked that the upload is a v3 binfile with
  exactly one file. This function checks edit rights again."
  [{:keys [::mbus/msgbus] :as cfg}
   {:keys [profile-id file-id session-id file]}]
  (files/check-edition-permissions! cfg profile-id file-id)
  (let [current (bfc/get-file cfg file-id :realize? true)
        team    (teams/get-team cfg
                                :profile-id profile-id
                                :file-id file-id)
        delay   (ldel/get-deletion-delay team)]
    (fsnap/create! cfg current
                   {:profile-id profile-id
                    :deleted-at (ct/in-future delay)
                    :created-by "system"})
    (let [cfg    (bfc/import-cfg cfg {:profile-id profile-id
                                      :project-id (:project-id current)
                                      :team team
                                      :name (:name current)
                                      :input (:path file)})
          cfg    (-> cfg
                     (assoc ::bfc/file-id file-id)
                     (assoc ::bfc/expected-revn (:revn current)))
          result (bf.v3/import-files! cfg)]
      (mbus/pub! msgbus
                 :topic file-id
                 :message {:type :file-restored
                           :session-id session-id
                           :file-id file-id
                           :vern (:vern result)})
      result)))

(sv/defmethod ::import-binfile
  "Import a penpot file in a binary format.

  The file content may be provided either as a multipart `file` upload
  or as an `upload-id` referencing a completed chunked-upload session,
  which allows importing files larger than the multipart size limit.

  When `file-id` is set, the zip replaces that existing file. The zip
  must be v3 and hold exactly one file. The caller needs edit rights.
  A backup snapshot is saved first and revn increases by one.
  "
  {::doc/added "1.15"
   ::doc/changes [["1.20" "Set default version to 3"]
                  ["2.15" "Add upload-id param for chunked upload support"]]

   ::webhooks/event? true
   ::sse/stream? true
   ::sm/params schema:import-binfile
   ::rpc/multipart-only-params [:file]
   ::climit/id [[:import-binfile/by-profile ::rpc/profile-id]
                [:import-binfile/global]]}
  [{:keys [::db/pool] :as cfg}
   {:keys [::rpc/profile-id ::rpc/session-id project-id file-id version upload-id] :as params}]
  (let [params (if (some? upload-id)
                 (let [file (db/tx-run! cfg media-cmd/assemble-chunks profile-id upload-id)]
                   (assoc params :file file))
                 params)]
    (if (some? file-id)
      (let [version (or version
                        (case (bfc/parse-file-format (-> params :file :path))
                          :binfile-v1 1
                          :binfile-v3 3))]
        ;; Rights, version and the single-file check fail the command.
        ;; They must not be an error event inside the stream, and they
        ;; must run before the backup snapshot is written.
        (files/check-edition-permissions! cfg profile-id file-id)
        (when-not (= 3 (int version))
          (ex/raise :type :validation
                    :code :unsupported-version
                    :hint "in-place import requires a v3 binfile"
                    :version version))
        (let [manifest (bf.v3/get-manifest cfg (-> params :file :path))
              params   (-> params
                           (assoc :profile-id profile-id)
                           (assoc :session-id session-id)
                           (assoc :version version))]
          (when (not= 1 (count (:files manifest)))
            (ex/raise :type :validation
                      :code :invalid-condition
                      :hint "in-place import requires a binfile with exactly one file"))
          (with-meta
            (sse/response (fn []
                            (try
                              (overwrite-file cfg params)
                              (finally
                                (when (some? upload-id)
                                  (fs/delete (-> params :file :path)))))))
            {::audit/props {:file nil
                            :generated-by (:generated-by manifest)
                            :referer (:referer manifest)}})))

      (do
        (projects/check-edition-permissions! pool profile-id project-id)
        (let [version (or version
                          (case (bfc/parse-file-format (-> params :file :path))
                            :binfile-v1 1
                            :binfile-v3 3))

              params  (-> params
                          (assoc :profile-id profile-id)
                          (assoc :version version))

              manifest
              (case (int version)
                1 nil
                3 (bf.v3/get-manifest cfg (-> params :file :path))
                (throw (ex-info (str "Unsupported binfile version: " version)
                                {:type :validation
                                 :code :unsupported-version
                                 :version version})))]

          (with-meta
            (sse/response (partial import-binfile cfg params))
            {::audit/props {:file nil
                            :generated-by (:generated-by manifest)
                            :referer (:referer manifest)}}))))))
