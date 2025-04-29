(ns jwt-example.core
  (:require [clojure.data.json :as json])
  (:import [java.util Base64])
  (:gen-class))

(defn decode-jwt-header [auth-header]
  (let [token (second (clojure.string/split auth-header #" "))
        header-part (first (clojure.string/split token #"\."))
        decoder (Base64/getUrlDecoder)
        decoded-bytes (.decode decoder header-part)
        decoded-str (String. decoded-bytes)
        header (json/read-str decoded-str)]
    header))

(defn -main [& args]
  (let [auth-header "Bearer eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJzdWIiOiIxMjM0NTY3ODkwIn0.dozjgNryP4J3jVmNHl0w5N_XgL0n3I9PlFUP0THsR8U"]
    (println (decode-jwt-header auth-header))))
