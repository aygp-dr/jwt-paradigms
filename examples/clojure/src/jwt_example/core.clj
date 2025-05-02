(ns jwt-example.core
  (:require [clojure.data.json :as json])
  (:import [java.util Base64])
  (:gen-class))

;; WARNING: This example demonstrates JWT header parsing WITHOUT signature verification.
;; NEVER use this approach for authentication or authorization in production.
;; ALWAYS verify the JWT signature before trusting any data in the token.

(defn decode-jwt-header [auth-header]
  (try
    (let [token (second (clojure.string/split auth-header #" "))
          header-part (first (clojure.string/split token #"\."))
          decoder (Base64/getUrlDecoder)
          decoded-bytes (.decode decoder header-part)
          decoded-str (String. decoded-bytes)
          header (json/read-str decoded-str)]
      
      ;; Validate structure
      (when-not (map? header)
        (throw (Exception. "JWT header is not a JSON object")))
      
      ;; Verify required fields
      (when-not (contains? header "alg")
        (throw (Exception. "JWT header missing 'alg' field")))
      (when-not (contains? header "typ")
        (throw (Exception. "JWT header missing 'typ' field")))
      
      ;; Return valid header
      header)
    (catch Exception e
      (throw (Exception. (str "Invalid JWT header: " (.getMessage e)))))))

(defn -main [& args]
  (let [auth-header "Bearer eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJzdWIiOiIxMjM0NTY3ODkwIn0.dozjgNryP4J3jVmNHl0w5N_XgL0n3I9PlFUP0THsR8U"]
    (println (decode-jwt-header auth-header))))

;; SECURE ALTERNATIVE:
;; (defn securely-verify-jwt [token secret-key]
;;   (try
;;     (let [signer (-> (com.auth0.jwt.algorithms.Algorithm/HMAC256 secret-key)
;;                    (com.auth0.jwt.JWT/require)
;;                    (.build))
;;           verified (.verify signer token)
;;           header (-> (com.auth0.jwt.JWT/decode token)
;;                    (.getHeader))]
;;       {:valid true
;;        :header header
;;        :payload verified})
;;     (catch Exception e
;;       {:valid false
;;        :error (.getMessage e)})))
