(ns typespec.pg-precision-test
  (:require [clojure.test :refer [deftest is testing]]
            [typespec.check :as c]
            [typespec.parse :as p]))

(defn checked [source]
  (c/check-files [(p/parse-string "precision.clj" (str "(ns precision) " source))] {}))
(defn diagnostics [source] (:diagnostics (checked source)))

(deftest loop-fixed-point
  (is (empty? (diagnostics "(loop [i 0] (if (pos? i) i (recur (inc i))))")))
  (is (empty? (diagnostics "(loop [i 0] #(recur \"bad\") (inc i))")))
  (is (empty? (diagnostics "(loop [i 0 j (inc i)] (if (pos? j) (inc i) (recur (inc i) (inc j))))")))
  (testing "a changing back edge cannot prove the numeric operation"
    (is (some #(= "TSP2345" (:code %))
              (diagnostics "(loop [i 0] (if (pos? i) i (recur \"bad\")))"))))
  (testing "unknown back edges retain incomplete checking"
    (is (some #(= "TSP210-01" (:code %))
              (diagnostics "(defn f [x] (loop [i 0] (if (pos? i) i (recur x))))"))))
  (testing "nested loop and fn recur belong to their own targets"
    (is (empty? (diagnostics "(loop [i 0] (loop [s \"x\"] (if (empty? s) s (recur (str s)))) (fn [x] (recur x)) (inc i))"))))
  (testing "unbounded structural growth falls back conservatively"
    (is (some #(= "TSP210-01" (:code %))
              (diagnostics "(loop [i 0] (if (pos? i) i (recur [i])))")))))

(deftest standalone-import-and-receiver-hint
  (is (empty? (diagnostics "(import '[java.util UUID]) (def value nil) (subs (.toString ^UUID value) 0)")))
  (is (empty? (diagnostics "(clojure.core/import java.util.UUID) (defn f [x] (subs (.toString ^UUID x) 0))")))
  (is (= {'Reader "java.io.Reader"}
         (:imports (p/parse-string "imports.clj" "(ns imports) (import java.io.Reader) (comment (import java.io.Writer)) '(import java.io.File) (import (computed foo))"))))
  (is (= {'Writer "java.io.Writer"}
         (:imports (p/parse-string "imports.clj" "(ns imports) (#_discarded import java.io.Writer)"))))
  (doseq [opts [{} {:strict-nil true}]]
    (is (some #(and (= "TSP2531" (:code %)) (re-find #"may return null" (:message %)))
              (:diagnostics (c/check-files
                             [(p/parse-string "nullable.clj" "(ns nullable) (defn f [^java.util.Map headers] (.length ^String (.get headers \"x\")))")]
                             opts)))))
  (is (some #(= "TSP2322" (:code %))
            (diagnostics "(let [x 42] (.length ^String x))")))
  (is (some #(= "TSP2345" (:code %))
            (diagnostics "(import '[java.util UUID]) (defn f [x] (inc (.toString ^UUID x)))")))
  (testing "unhinted unknown receivers remain incomplete"
    (is (some #(= "TSP7020" (:code %)) (diagnostics "(defn f [x] (.toString x))")))))

(deftest abs-contract
  (is (empty? (diagnostics "(subs \"hello\" (min (abs (- 3 10)) 0)) (abs -2/3) (abs -2.5)")))
  (is (some #(= "TSP2345" (:code %)) (diagnostics "(abs \"bad\")")))
  (is (some #(= "TSP210-01" (:code %)) (diagnostics "(defn f [x] (abs x))"))))
