;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS SUBSIDIARY SL

(ns app.main.ui.workspace.git
  (:require-macros [app.main.style :as stl])
  (:require
   [app.main.data.modal :as modal]
   [app.main.data.workspace.git :as dwg]
   [app.main.refs :as refs]
   [app.main.store :as st]
   [app.main.ui.ds.buttons.button :refer [button*]]
   [app.main.ui.ds.buttons.icon-button :refer [icon-button*]]
   [app.main.ui.ds.foundations.assets.icon :as i]
   [app.util.dom :as dom]
   [app.util.i18n :refer [tr]]
   [rumext.v2 :as mf]))

(defn- field
  [id label value on-change & {:keys [type]}]
  [:div {:class (stl/css :field)}
   [:label {:class (stl/css :label) :for id} label]
   [:input {:id id
            :class (stl/css :input)
            :type (or type "text")
            :value value
            :on-change #(on-change (dom/get-value (dom/get-target %)))}]])

(mf/defc git-modal
  {::mf/register modal/components
   ::mf/register-as :git-link}
  []
  (let [git        (mf/deref refs/workspace-git)
        link       (:link git)
        provider*  (mf/use-state "github")
        repo-url*  (mf/use-state "")
        branch*    (mf/use-state "main")
        folder*    (mf/use-state "penpot")
        token*     (mf/use-state "")
        filled*    (mf/use-ref false)

        status-text
        (cond
          (nil? link)
          (tr "workspace.git.none")

          (= (:last-push-status link) "success")
          (tr "workspace.git.success")

          (= (:last-push-status link) "error")
          (tr "workspace.git.error")

          (= (:last-push-status link) "running")
          (tr "workspace.git.queued")

          :else
          (tr "workspace.git.last-push"))

        can-save?
        (and (seq @repo-url*)
             (seq @branch*)
             (seq @folder*)
             (or link (seq @token*)))

        on-save
        (mf/use-fn
         (mf/deps @provider* @repo-url* @branch* @folder* @token* link)
         (fn [event]
           (dom/prevent-default event)
           (let [params {:provider @provider*
                         :repo-url @repo-url*
                         :branch @branch*
                         :folder @folder*}
                 params (if (seq @token*)
                          (assoc params :token @token*)
                          params)]
             (st/emit! (dwg/save-link params)))))]

    (mf/with-effect []
      (st/emit! (dwg/fetch-link)))

    (mf/with-effect [link]
      (when (and link (not (mf/ref-val filled*)))
        (mf/set-ref-val! filled* true)
        (reset! provider* (or (:provider link) "github"))
        (reset! repo-url* (or (:repo-url link) ""))
        (reset! branch* (or (:branch link) "main"))
        (reset! folder* (or (:folder link) "penpot"))))

    [:div {:class (stl/css :modal-overlay)}
     [:div {:class (stl/css :modal-container)}
      [:div {:class (stl/css :modal-header)}
       [:h2 {:class (stl/css :modal-title)} (tr "workspace.git.title")]
       [:> icon-button* {:variant "ghost"
                         :tooltip-class (stl/css :modal-close-btn)
                         :aria-label (tr "labels.close")
                         :icon i/close
                         :on-click modal/hide!}]]

      [:form {:on-submit on-save}
       [:div {:class (stl/css :field)}
        [:label {:class (stl/css :label) :for "git-provider"}
         (tr "workspace.git.provider")]
        [:select {:id "git-provider"
                  :class (stl/css :input)
                  :value @provider*
                  :on-change #(reset! provider* (dom/get-value (dom/get-target %)))}
         [:option {:value "github"} (tr "workspace.git.github")]
         [:option {:value "gitlab"} (tr "workspace.git.gitlab")]]]

       (field "git-repo-url" (tr "workspace.git.repo-url") @repo-url* #(reset! repo-url* %))
       (field "git-branch" (tr "workspace.git.branch") @branch* #(reset! branch* %))
       (field "git-folder" (tr "workspace.git.folder") @folder* #(reset! folder* %))
       (field "git-token" (tr "workspace.git.token") @token* #(reset! token* %) :type "password")

       (when (:token-set link)
         [:p {:class (stl/css :status)} (tr "workspace.git.token-set")])

       [:p {:class (stl/css :status)} status-text]
       (when (:last-push-error link)
         [:p {:class (stl/css :status)} (:last-push-error link)])

       [:div {:class (stl/css :actions)}
        (when link
          [:> button* {:variant "secondary"
                       :type "button"
                       :on-click #(st/emit! (dwg/delete-link))}
           (tr "workspace.git.delete")])
        (when link
          [:> button* {:variant "secondary"
                       :type "button"
                       :on-click #(st/emit! (dwg/pull-now))}
           (tr "workspace.git.pull")])
        (when link
          [:> button* {:variant "secondary"
                       :type "button"
                       :on-click #(st/emit! (dwg/push-now))}
           (tr "workspace.git.push")])
        [:> button* {:variant "primary"
                     :type "submit"
                     :disabled (not can-save?)}
         (tr "workspace.git.save")]]]]]))
