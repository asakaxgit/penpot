;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS SUBSIDIARY SL

(ns backend-tests.binfile-canonical-test
  (:require
   [app.binfile.common :as bfc]
   [app.binfile.v3 :as v3]
   [app.common.json :as json]
   [app.msgbus :as mbus]
   [app.rpc :as-alias rpc]
   [app.storage.tmp :as tmp]
   [backend-tests.helpers :as th]
   [clojure.java.io :as jio]
   [clojure.test :as t]
   [datoteka.fs :as fs]
   [datoteka.io :as io])
  (:import
   java.util.Arrays
   java.util.zip.ZipFile))

(t/use-fixtures :once th/state-init)
(t/use-fixtures :each th/database-reset)

(defn- file-bytes
  [path]
  (java.nio.file.Files/readAllBytes (fs/path path)))

(defn- export-to
  [file-id & {:keys [canonical ids]}]
  (let [output (tmp/tempfile :suffix ".penpot")]
    (v3/export-files!
     (-> th/*system*
         (assoc ::bfc/ids (or ids #{file-id}))
         (assoc ::bfc/export-type :detach-libraries)
         (cond-> canonical (assoc ::bfc/canonical true)))
     (io/output-stream output))
    output))

(defn- zip-json
  [path entry-name]
  (with-open [zip (ZipFile. (jio/file (str path)))]
    (let [entry (.getEntry zip entry-name)]
      (t/is (some? entry) entry-name)
      (with-open [reader (jio/reader (.getInputStream zip entry))]
        (json/read reader :key-fn identity)))))

(defn- zip-names
  [path]
  (with-open [zip (ZipFile. (jio/file (str path)))]
    (mapv #(.getName ^java.util.zip.ZipEntry %)
          (enumeration-seq (.entries zip)))))

(defn- upload
  [path]
  {:filename "file.penpot"
   :size (java.nio.file.Files/size (fs/path path))
   :path path
   :mtype "application/zip"})

(defn- overwrite!
  [profile-id file-id path & {:keys [version]}]
  (th/multipart-command!
   {::th/type :import-binfile
    ::rpc/profile-id profile-id
    :file-id file-id
    :version (or version 3)
    :file (upload path)}))

(t/deftest canonical-export-is-byte-stable
  (let [profile (th/create-profile* 1)
        file    (th/create-file* 1 {:profile-id (:id profile)
                                    :project-id (:default-project-id profile)})
        first   (export-to (:id file) :canonical true)
        second  (export-to (:id file) :canonical true)]
    (t/is (Arrays/equals (file-bytes first) (file-bytes second)))))

(t/deftest canonical-export-drops-volatile-fields
  (let [profile  (th/create-profile* 1)
        file     (th/create-file* 1 {:profile-id (:id profile)
                                     :project-id (:default-project-id profile)})
        plain    (export-to (:id file))
        stable   (export-to (:id file) :canonical true)
        manifest (zip-json stable "manifest.json")
        file-json (zip-json stable (str "files/" (:id file) ".json"))
        plain-manifest (zip-json plain "manifest.json")
        plain-file (zip-json plain (str "files/" (:id file) ".json"))]
    (t/is (contains? plain-manifest "generatedBy"))
    (t/is (contains? plain-file "revn"))
    (t/is (not (contains? manifest "generatedBy")))
    (t/is (not (contains? file-json "revn")))
    (t/is (not (contains? file-json "vern")))
    (t/is (not (contains? file-json "createdAt")))
    (t/is (not (contains? file-json "modifiedAt")))
    (t/is (not (contains? file-json "metadata")))
    (t/is (not (some #(re-find #"thumbnails" %) (zip-names stable))))))

(t/deftest canonical-export-flag-is-threaded
  (let [profile (th/create-profile* 1)
        file    (th/create-file* 1 {:profile-id (:id profile)
                                    :project-id (:default-project-id profile)})
        seen    (atom nil)]
    (with-redefs [v3/export-files! (fn [cfg _output]
                                     (reset! seen cfg)
                                     nil)]
      (let [out (th/command! {::th/type :export-binfile
                              ::rpc/profile-id (:id profile)
                              :file-id (:id file)
                              :canonical true})]
        (t/is (nil? (:error out)))
        (th/consume-sse (:result out))))
    (t/is (true? (::bfc/canonical @seen)))))

(t/deftest overwrite-import-round-trips-canonical-bytes
  (let [profile (th/create-profile* 1)
        file    (th/create-file* 1 {:profile-id (:id profile)
                                    :project-id (:default-project-id profile)})
        file-id (:id file)
        before  (th/db-exec-one! ["select revn from file where id = ?" file-id])
        path    (export-to file-id :canonical true)
        bytes   (file-bytes path)
        pubs    (atom [])]
    (with-redefs [mbus/pub! (fn [_ & {:keys [message]}]
                              (swap! pubs conj message))]
      (let [out    (overwrite! (:id profile) file-id path)
            events (th/consume-sse (:result out))]
        (t/is (nil? (:error out)))
        (t/is (empty? (filter #(= :error (first %)) events)))))
    (let [after (th/db-exec-one! ["select revn from file where id = ?" file-id])
          snaps (th/db-exec! ["select created_by from file_change where file_id = ?" file-id])
          again (file-bytes (export-to file-id :canonical true))]
      (t/is (= (inc (:revn before)) (:revn after)))
      (t/is (some #(= "system" (:created-by %)) snaps))
      (t/is (Arrays/equals bytes again))
      (t/is (= :file-restored (:type (first @pubs))))
      (t/is (= file-id (:file-id (first @pubs))))
      (t/is (int? (:vern (first @pubs)))))))

(t/deftest overwrite-import-rejects-a-viewer
  (let [owner   (th/create-profile* 1)
        team    (th/create-team* 1 {:profile-id (:id owner)})
        project (th/create-project* 1 {:profile-id (:id owner)
                                       :team-id (:id team)})
        file    (th/create-file* 1 {:profile-id (:id owner)
                                    :project-id (:id project)})
        viewer  (th/create-profile* 2)
        path    (export-to (:id file) :canonical true)]
    (th/create-team-role* {:team-id (:id team)
                           :profile-id (:id viewer)
                           :role :viewer})
    (let [out   (overwrite! (:id viewer) (:id file) path)
          snaps (th/db-exec! ["select id from file_change where file_id = ?" (:id file)])]
      (t/is (th/ex-of-code? (:error out) :object-not-found))
      (t/is (empty? snaps)))))

(t/deftest overwrite-import-rejects-more-than-one-file
  (let [profile (th/create-profile* 1)
        file-1  (th/create-file* 1 {:profile-id (:id profile)
                                    :project-id (:default-project-id profile)})
        file-2  (th/create-file* 2 {:profile-id (:id profile)
                                    :project-id (:default-project-id profile)})
        path    (export-to (:id file-1)
                           :ids #{(:id file-1) (:id file-2)})
        out     (overwrite! (:id profile) (:id file-1) path)
        snaps   (th/db-exec! ["select id from file_change where file_id = ?" (:id file-1)])]
    (t/is (th/ex-of-code? (:error out) :invalid-condition))
    (t/is (empty? snaps))))

(t/deftest overwrite-import-rejects-v1
  (let [profile (th/create-profile* 1)
        file    (th/create-file* 1 {:profile-id (:id profile)
                                    :project-id (:default-project-id profile)})
        path    (export-to (:id file) :canonical true)
        out     (overwrite! (:id profile) (:id file) path :version 1)
        snaps   (th/db-exec! ["select id from file_change where file_id = ?" (:id file)])]
    (t/is (th/ex-of-code? (:error out) :unsupported-version))
    (t/is (empty? snaps))))
