(ns jwt-example.core-test
  (:require [clojure.spec.alpha :as s]
            [clojure.spec.gen.alpha :as gen]
            [clojure.spec.test.alpha :as stest]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [clojure.test.check :as tc]
            [clojure.test.check.properties :as prop]
            [jwt-example.core :as sut]
            [jwt-example.specs :as specs]))

;; Exercise every s/fdef :args spec while the unit tests run.
(use-fixtures :once
  (fn [f] (stest/instrument) (try (f) (finally (stest/unstrument)))))

;; The token in examples.org and in jwt-example.core/-main.
(def ^:private sample-token
  "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJzdWIiOiIxMjM0NTY3ODkwIn0.dozjgNryP4J3jVmNHl0w5N_XgL0n3I9PlFUP0THsR8U")

(defn- bearer [header]
  (str "Bearer " (specs/json-segment header) ".e30."))

(defn- decode-unchecked
  "Call decode-jwt-header with instrumentation off, so invalid input reaches
  the fn's own validation instead of failing the :args spec."
  [auth-header]
  (stest/with-instrument-disabled (sut/decode-jwt-header auth-header)))

(deftest decodes-the-sample-token
  (is (= {"alg" "HS256" "typ" "JWT"} (sut/decode-jwt-header (str "Bearer " sample-token)))))

(deftest rejects-invalid-headers
  (testing "missing required fields"
    (is (thrown-with-msg? Exception #"^Invalid JWT header: JWT header missing 'typ' field$"
                          (decode-unchecked (bearer {"alg" "none"}))))
    (is (thrown-with-msg? Exception #"^Invalid JWT header: JWT header missing 'alg' field$"
                          (decode-unchecked (bearer {"typ" "JWT"})))))
  (testing "a header that isn't a JSON object"
    (is (thrown-with-msg? Exception #"^Invalid JWT header: JWT header is not a JSON object$"
                          (decode-unchecked (bearer ["alg" "typ"])))))
  (testing "a segment in the standard (not URL-safe) base64 alphabet"
    (is (thrown-with-msg? Exception #"^Invalid JWT header: Illegal base64 character"
                          (decode-unchecked "Bearer eyJ+eyJ/.e30."))))
  (testing "no token after the scheme"
    (is (thrown-with-msg? Exception #"^Invalid JWT header: "
                          (decode-unchecked "Bearer")))))

(defn- passes? [result]
  (is (:pass? result) (pr-str (select-keys result [:shrunk :seed]))))

(deftest decoding-round-trips
  ;; Any JOSE header, encoded into a bearer token, decodes back to itself.
  (passes?
   (tc/quick-check 100
                   (prop/for-all [header    (s/gen ::specs/jose-header)
                                  claims    (s/gen ::specs/claims)
                                  signature (s/gen ::specs/segment)]
                                 (= header (sut/decode-jwt-header
                                            (str "Bearer " (specs/json-segment header) "."
                                                 (specs/json-segment claims) "." signature)))))))

(deftest headers-without-alg-or-typ-are-rejected
  (passes?
   (tc/quick-check 50
                   (prop/for-all [header (s/gen ::specs/jose-header)
                                  field  (gen/elements ["alg" "typ"])]
                                 (try (decode-unchecked (bearer (dissoc header field)))
                                      false
                                      (catch Exception e
                                        (= (str "Invalid JWT header: JWT header missing '" field "' field")
                                           (.getMessage e))))))))

(deftest non-object-headers-are-rejected
  (passes?
   (tc/quick-check 50
                   (prop/for-all [v (gen/one-of [(gen/vector (gen/large-integer))
                                                 (gen/string-alphanumeric)
                                                 (gen/large-integer)
                                                 (gen/boolean)
                                                 (gen/return nil)])]
                                 (try (decode-unchecked (bearer v))
                                      false
                                      (catch Exception e
                                        (= "Invalid JWT header: JWT header is not a JSON object"
                                           (.getMessage e))))))))
