(ns typespec.generic-extern-test
  (:require [clojure.test :refer [deftest is]]
            [typespec.externs-regression-test :as fixture]))

(deftest generic-library-independent-calls
  (fixture/project
   "(ns app (:require [vendor :as v]))
    (defn text-id {:t/sig '[string? :-> string?]} [x] (v/id x))
    (defn int-id {:t/sig '[int? :-> int?]} [x] (v/id x))"
   "{vendor/id (t/all [a] [a :-> a])}" {}
   (fn [res] (is (empty? (:diagnostics res)) (pr-str (:diagnostics res))))))

(deftest generic-extern-checks-own-body
  (doseq [[body valid?] [["x" true] ["42" false]]]
    (fixture/project
     (str "(ns app) (defn id [x] " body ")")
     "{app/id (t/all [a] [a :-> a])}" {}
     (fn [res]
       (if valid?
         (is (empty? (:diagnostics res)) (pr-str (:diagnostics res)))
         (is (contains? (fixture/codes res) "TSP2322") (pr-str (:diagnostics res))))))))

(deftest equivalent-source-extern-contract
  (fixture/project
   "(ns app) (defn id {:t/sig '(t/all [a] [a :-> a])} [x] x)"
   "{app/id (t/all [renamed] [renamed :-> renamed])}" {}
   (fn [res] (is (empty? (:diagnostics res)) (pr-str (:diagnostics res))))))

(deftest conflicting-generic-contract
  (fixture/project
   "(ns app) (defn id {:t/sig '(t/all [a] [a :-> a])} [x] x)"
   "{app/id (t/all [a] [a :-> string?])}" {}
   (fn [res] (is (contains? (fixture/codes res) "TSP2790")))) )
