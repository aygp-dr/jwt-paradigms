(ns jwt-example.specs-test
  "Generative checks for every pure s/fdef'd fn, plus data-spec sanity.
  Per https://clojure.org/guides/spec (Testing)."
  (:require [clojure.spec.alpha :as s]
            [clojure.spec.test.alpha :as stest]
            [clojure.test :refer [deftest is testing]]
            [jwt-example.core :as sut]
            [jwt-example.specs :as specs]))

(def ^:private check-opts {:clojure.spec.test.check/opts {:num-tests 50}})

;; Side-effecting fns: fdef'd for instrumentation, never generatively checked.
(def ^:private side-effecting
  #{`sut/-main})

(defn- checkable []
  (remove side-effecting (stest/enumerate-namespace 'jwt-example.core)))

(deftest fdefs-hold-under-generative-testing
  (let [results (stest/check (checkable) check-opts)]
    (is (= #{`sut/decode-jwt-header} (set (map :sym results))))
    (doseq [r results]
      (testing (str (:sym r))
        (is (nil? (:failure r))
            (pr-str (stest/abbrev-result r)))))))

(deftest data-specs-generate-and-conform
  (doseq [k [::specs/segment ::specs/jose-header ::specs/claims ::specs/token ::specs/auth-header]]
    (testing (str k)
      (is (every? (fn [[v _]] (s/valid? k v)) (s/exercise k 10))))))

;; The token in examples.org and in jwt-example.core/-main.
(def ^:private sample-token
  "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJzdWIiOiIxMjM0NTY3ODkwIn0.dozjgNryP4J3jVmNHl0w5N_XgL0n3I9PlFUP0THsR8U")

(deftest real-values-conform
  (testing "the examples.org sample"
    (is (s/valid? ::specs/token sample-token))
    (is (s/valid? ::specs/auth-header (str "Bearer " sample-token)))
    (is (s/valid? ::specs/jose-header {"alg" "HS256" "typ" "JWT"})))
  (testing "what the specs reject"
    (is (not (s/valid? ::specs/segment "eyJ+")) "standard base64 alphabet")
    (is (not (s/valid? ::specs/segment "eyJhb=")) "padding")
    (is (not (s/valid? ::specs/segment "abcde")) "1 char past a multiple of 4")
    (is (not (s/valid? ::specs/jose-header {"alg" "HS256"})) "no typ")
    (is (not (s/valid? ::specs/jose-header {"alg" nil "typ" "JWT"})) "alg not a string")
    (is (not (s/valid? ::specs/token (subs sample-token 0 36))) "two segments")
    (is (not (s/valid? ::specs/auth-header (str "Basic " sample-token))) "not a bearer token")))
