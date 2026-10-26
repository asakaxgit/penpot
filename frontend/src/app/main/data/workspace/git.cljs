;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS SUBSIDIARY SL

(ns app.main.data.workspace.git
  (:require
   [app.main.data.notifications :as ntf]
   [app.main.repo :as rp]
   [app.util.i18n :refer [tr]]
   [beicon.v2.core :as rx]
   [potok.v2.core :as ptk]))

(defn- put-link
  [link]
  (ptk/reify ::put-link
    ptk/UpdateEvent
    (update [_ state]
      (assoc state :workspace-git {:status :loaded :link link}))))

(defn fetch-link
  []
  (ptk/reify ::fetch-link
    ptk/WatchEvent
    (watch [_ state _]
      (when-let [file-id (:current-file-id state)]
        (->> (rp/cmd! :get-git-links {:file-id file-id})
             (rx/map (fn [links]
                       (put-link (first links))))
             (rx/catch (fn [_cause]
                         (rx/of (put-link nil)
                                (ntf/error (tr "workspace.git.error"))))))))))

(defn save-link
  [params]
  (ptk/reify ::save-link
    ptk/WatchEvent
    (watch [_ state _]
      (let [file-id (:current-file-id state)
            link    (:link (:workspace-git state))
            params  (assoc params :file-id file-id)
            request (if link
                      (rp/cmd! :update-git-link (assoc params :id (:id link)))
                      (rp/cmd! :create-git-link params))]
        (->> request
             (rx/mapcat (fn [saved]
                          (rx/of (put-link saved)
                                 (ntf/success (tr "workspace.git.saved")))))
             (rx/catch (fn [_cause]
                         (rx/of (ntf/error (tr "workspace.git.error"))))))))))

(defn delete-link
  []
  (ptk/reify ::delete-link
    ptk/WatchEvent
    (watch [_ state _]
      (when-let [link (:link (:workspace-git state))]
        (->> (rp/cmd! :delete-git-link {:id (:id link)})
             (rx/map (fn [_]
                       (put-link nil)))
             (rx/catch (fn [_cause]
                         (rx/of (ntf/error (tr "workspace.git.error"))))))))))

(defn pull-now
  []
  (ptk/reify ::pull-now
    ptk/WatchEvent
    (watch [_ state _]
      (when-let [link (:link (:workspace-git state))]
        (->> (rp/cmd! :pull-git-link {:id (:id link)})
             (rx/mapcat (fn [_]
                          (rx/of (fetch-link)
                                 (ntf/success (tr "workspace.git.pull-queued")))))
             (rx/catch (fn [_cause]
                         (rx/of (ntf/error (tr "workspace.git.error"))))))))))

(defn push-now
  []
  (ptk/reify ::push-now
    ptk/WatchEvent
    (watch [_ state _]
      (when-let [link (:link (:workspace-git state))]
        (->> (rp/cmd! :push-git-link {:id (:id link)})
             (rx/mapcat (fn [_]
                          (rx/of (fetch-link)
                                 (ntf/success (tr "workspace.git.queued")))))
             (rx/catch (fn [_cause]
                         (rx/of (ntf/error (tr "workspace.git.error"))))))))))
