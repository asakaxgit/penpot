;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS SUBSIDIARY SL

(ns app.git.token
  "Encrypt provider tokens with a key derived from PENPOT_SECRET_KEY.

  Penpot generates a new secret when PENPOT_SECRET_KEY is unset. That
  secret changes on restart, and tokens stored with the previous secret
  can no longer be decrypted. Set a fixed PENPOT_SECRET_KEY before
  linking a repository."
  (:require
   [app.common.exceptions :as ex]
   [app.setup :as-alias setup]
   [app.setup.keys :as keys]
   [buddy.sign.jwe :as jwe]))

(def ^:private salt "git-integration")

(defn- secret
  [cfg]
  (let [value (get-in cfg [::setup/props :secret-key])]
    (when-not (and (string? value) (seq value))
      (ex/raise :type :validation
                :code :git-secret-key
                :hint "PENPOT_SECRET_KEY is required to store a git token"))
    value))

(defn- key-bytes
  [cfg]
  (keys/derive (secret cfg) :salt salt))

(defn encrypt
  "Return the JWE ciphertext of a provider token."
  [cfg token]
  (jwe/encrypt token (key-bytes cfg) {:alg :a256kw :enc :a256gcm}))

(defn- as-text
  [value]
  (cond
    (string? value) value
    (bytes? value) (String. ^bytes value "UTF-8")
    :else (throw (ex-info "git token payload is not text" {}))))

(defn decrypt
  "Return the provider token stored in a ciphertext."
  [cfg token]
  (-> (jwe/decrypt token (key-bytes cfg) {:alg :a256kw :enc :a256gcm})
      as-text))
