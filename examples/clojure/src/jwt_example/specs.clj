(ns jwt-example.specs
  "Data specs for JWT header parsing (https://clojure.org/guides/spec).

  A JWT in JWS compact serialization is three segments joined by dots
  (RFC 7515 section 7.1). Each segment is base64url without padding (RFC 4648 section 5,
  RFC 7515 section 2). The first decodes to the JOSE header: a UTF-8 JSON object that
  must contain \"alg\" (RFC 7515 section 4.1.1), plus \"typ\" for JWTs (RFC 7519 section 5.1).
  jwt-example.core parses it with clojure.data.json, so header maps have
  string keys.

  The s/fdef for jwt-example.core/decode-jwt-header sits next to its defn
  (tangled from examples.org)."
  (:require [clojure.data.json :as json]
            [clojure.spec.alpha :as s]
            [clojure.spec.gen.alpha :as gen]
            [clojure.string :as str])
  (:import (java.nio.charset StandardCharsets)
           (java.util Base64)))

;; --- base64url segments ---

(defn base64url
  "Encode bytes as unpadded base64url, as JWS compact serialization does."
  ^String [^bytes bs]
  (.encodeToString (.withoutPadding (Base64/getUrlEncoder)) bs))

(defn json-segment
  "Encode a value as a JWT segment: JSON, UTF-8, unpadded base64url."
  [x]
  (base64url (.getBytes ^String (json/write-str x) StandardCharsets/UTF_8)))

(defn segment-json
  "Decode a JWT segment as UTF-8 JSON (string keys), or ::s/invalid when it
  isn't base64url-encoded JSON. A reference decoder, independent of
  jwt-example.core."
  [^String segment]
  (try
    (json/read-str (String. (.decode (Base64/getUrlDecoder) segment) StandardCharsets/UTF_8))
    (catch Exception _ ::s/invalid)))

;; The URL-safe alphabet with no padding, and never 1 char past a multiple
;; of 4 (a lone 6-bit char can't complete a byte). "" encodes zero bytes,
;; e.g. the signature of an unsecured JWT.
(s/def ::segment
  (s/with-gen (s/and string?
                     #(re-matches #"[A-Za-z0-9_-]*" %)
                     #(not= 1 (mod (count %) 4)))
    #(gen/fmap base64url (gen/bytes))))

;; --- JOSE header ---

;; Registered algorithm names (RFC 7518 section 3.1, RFC 8037). "alg" may be any
;; StringOrURI; the registered names feed the generator.
(def registered-algs
  #{"HS256" "HS384" "HS512" "RS256" "RS384" "RS512" "ES256" "ES384" "ES512"
    "PS256" "PS384" "PS512" "EdDSA" "none"})

(s/def :jwt-example.header/alg
  (s/with-gen string? #(gen/elements (sort registered-algs))))
(s/def :jwt-example.header/typ
  (s/with-gen string? #(gen/elements ["JWT" "jwt" "at+jwt" "JOSE" "application/jwt"])))
(s/def :jwt-example.header/kid string?)
(s/def :jwt-example.header/cty string?)
;; a non-empty array of header parameter names (RFC 7515 section 4.1.11)
(s/def :jwt-example.header/crit (s/coll-of string? :kind vector? :min-count 1 :gen-max 3))

;; The header with keyword keys. ::jose-header is the same map with the
;; string keys that clojure.data.json produces. It checks the keyword view
;; with a predicate, not a conformer, so conform (and an fdef's :ret in
;; :fn) keeps the original string-keyed map.
(s/def ::jose-header-params
  (s/keys :req-un [:jwt-example.header/alg :jwt-example.header/typ]
          :opt-un [:jwt-example.header/kid :jwt-example.header/cty :jwt-example.header/crit]))

(s/def ::jose-header
  (s/with-gen
    (s/and (s/map-of string? any?)
           #(s/valid? ::jose-header-params (update-keys % keyword)))
    #(gen/fmap (fn [m] (update-keys m name)) (s/gen ::jose-header-params))))

;; --- Tokens ---

;; JWT claims set (RFC 7519 section 4): a JSON object. decode-jwt-header never reads
;; it; the generator uses two registered claims.
(s/def ::claims
  (s/with-gen (s/map-of string? any?)
    #(gen/hash-map "sub" (gen/string-alphanumeric)
                   "iat" (gen/choose 1500000000 2000000000))))

(defn- segments [^String token]
  (str/split token #"\." -1))

;; JWS compact serialization: header.payload.signature, each a ::segment,
;; where the header decodes to a ::jose-header.
(s/def ::token
  (s/with-gen
    (s/and string?
           #(let [parts (segments %)]
              (and (= 3 (count parts))
                   (every? (fn [p] (s/valid? ::segment p)) parts)
                   (s/valid? ::jose-header (segment-json (first parts))))))
    #(gen/fmap (fn [[header claims signature]]
                 (str/join "." [(json-segment header) (json-segment claims) signature]))
               (gen/tuple (s/gen ::jose-header) (s/gen ::claims) (s/gen ::segment)))))

;; An HTTP Authorization header carrying a bearer token (RFC 6750 section 2.1).
(s/def ::auth-header
  (s/with-gen
    (s/and string? #(str/starts-with? % "Bearer ") #(s/valid? ::token (subs % 7)))
    #(gen/fmap (fn [token] (str "Bearer " token)) (s/gen ::token))))

(defn auth-header->jose-header
  "The JOSE header carried by a valid ::auth-header, decoded with
  segment-json (the reference for decode-jwt-header's :fn)."
  [^String auth-header]
  (segment-json (first (segments (subs auth-header 7)))))
