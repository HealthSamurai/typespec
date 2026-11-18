(ns typespec.generic-integration-test
  "Acceptance tests for the rank-1 call integration; initially red until connected."
  (:require [clojure.test :refer [deftest is testing]]
            [typespec.check :as check]
            [typespec.parse :as parse]
            [typespec.main :as main]))

(defn checked [source]
  (check/check-files [(parse/parse-string "generic.clj" (str "(ns generic)\n" source))] {}))
(defn codes [res] (mapv :code (:diagnostics res)))

(def identity-source
  "(defn identity-value {:t/sig '(t/all [a] [a :-> a])} [x] x)\n")

(deftest independent-generic-calls
  (doseq [bodies [["(defn text-value {:t/sig '[string? :-> string?]} [s] (identity-value s))"
                  "(defn number-value {:t/sig '[int? :-> int?]} [n] (identity-value n))"]
                 ["(defn number-value {:t/sig '[int? :-> int?]} [n] (identity-value n))"
                  "(defn text-value {:t/sig '[string? :-> string?]} [s] (identity-value s))"]]]
    (let [r (checked (apply str identity-source bodies))]
      (is (empty? (:diagnostics r)) (pr-str (:diagnostics r)))
      (is (some #(re-find #"t/all" %) (main/sig-lines r 'generic))))))

(deftest generic-body-is-universal
  (let [r (checked "(defn bad-identity {:t/sig '(t/all [a] [a :-> a])} [x] 42)")]
    (is (some #{"TSP2322"} (codes r)) (pr-str (:diagnostics r)))
    (is (not-any? #{"TSP9001"} (codes r)))))

(def hof-source
  "(defn apply-one {:t/sig '(t/all [a b] [a (t/fn [a :-> b]) :-> b])} [x f] (f x))\n")

(deftest contextual-callback
  (let [r (checked (str hof-source
                       "(defn length-of {:t/sig '[string? :-> int?]} [s] (apply-one s (fn [text] (count text))))"))]
    (is (empty? (:diagnostics r)) (pr-str (:diagnostics r))))
  (let [r (checked (str hof-source
                       "(defn bad-length {:t/sig '[string? :-> int?]} [s] (apply-one s (fn [text] \"wrong\")))"))]
    (is (some #{"TSP2322"} (codes r)) (pr-str (:diagnostics r)))))

(deftest bound-rejects-concrete-incompatible-input
  (let [r (checked
           "(defn numeric-id {:t/sig '(t/all [(a :< number?)] [a :-> a])} [x] x)
            (defn bad [] (numeric-id \"wrong\"))")]
    (is (seq (filter #(= :error (:severity %)) (:diagnostics r))))
    (is (not-any? #{"TSP9001"} (codes r)) (pr-str (:diagnostics r)))))

(deftest local-generic-aliases
  (let [r (checked (str identity-source
                       "(defn text-value {:t/sig '[string? :-> string?]} [s]
                          (let [f identity-value] (f s)))
                        (defn numbers {:t/sig '[int? :-> int?]} [n]
                          (let [f identity-value g f] (g n)))"))]
    (is (empty? (:diagnostics r)) (pr-str (:diagnostics r)))))

(deftest unknown-evidence-is-incomplete
  (doseq [strict? [false true]]
    (let [r (check/check-files
             [(parse/parse-string "generic.clj"
                (str "(ns generic)\n" identity-source
                     "(defn- consume [x] (identity-value x))"))]
             {:strict strict?})
          ds (:diagnostics r)]
      (is (= ["TSP210-01"] (codes r)) (pr-str ds))
      (is (= (if strict? :error :warning) (:severity (first ds)))))))

(deftest callback-context-is-independent-of-argument-order
  (let [r (checked
           "(defn apply-last {:t/sig '(t/all [a b] [(t/fn [a :-> b]) a :-> b])} [f x] (f x))
            (defn length-of {:t/sig '[string? :-> int?]} [s]
              (apply-last (fn [text] (count text)) s))")]
    (is (empty? (:diagnostics r)) (pr-str (:diagnostics r)))))

(deftest polymorphic-callback-is-explicitly-incomplete
  (let [r (checked (str identity-source hof-source
                       "(defn f [] (apply-one 42 identity-value))"))]
    (is (= ["TSP210-01"] (codes r)) (pr-str (:diagnostics r)))))

(deftest unresolved-callback-input-is-incomplete
  (let [r (checked
           "(defn make-one {:t/sig '(t/all [a b] [(t/fn [a :-> b]) :-> b])} [f] (throw (Exception.)))
            (defn f [] (make-one (fn [x] x)))")]
    (is (= ["TSP210-01"] (codes r)) (pr-str (:diagnostics r)))))

(deftest generic-through-core-callback-application
  (let [r (checked (str identity-source
                       "(defn f {:t/sig '[int? :-> (s/coll-of int? :kind vector?)]} [n]
                          (mapv identity-value [n]))"))]
    (is (empty? (:diagnostics r)) (pr-str (:diagnostics r)))))


(deftest failed-generic-call-does-not-repeat-return-diagnostic
  (let [r (checked (str identity-source
                       "(defn f {:t/sig '[any? :-> string?]} [x] (identity-value x))"))]
    (is (= ["TSP210-01"] (codes r)) (pr-str (:diagnostics r)))))

(deftest collection-evidence-contextualizes-callback
  (let [r (checked
           "(defn first-mapped {:t/sig '(t/all [a b] [(s/coll-of a :kind vector?) (t/fn [a :-> b]) :-> b])}
              [xs f] (f (nth xs 0)))
            (defn f {:t/sig '[string? :-> int?]} [s]
              (first-mapped [s] (fn [text] (count text))))")]
    (is (empty? (:diagnostics r)) (pr-str (:diagnostics r)))))


(deftest incomplete-callback-still-checks-its-body
  (let [r (checked (str hof-source
                       "(defn f [x] (apply-one x (fn [v] (subs 42 0))))"))]
    (is (some #{"TSP2345"} (codes r)) (pr-str (:diagnostics r)))
    (is (= 1 (count (filter #{"TSP210-01"} (codes r)))) (pr-str (:diagnostics r)))))

(deftest unresolved-inference-variables-do-not-escape
  (let [r (checked (str hof-source "(defn f [x] (apply-one x (fn [v] v)))"))]
    (is (not-any? #(and (map? %) (= :ivar (:t %)))
                  (tree-seq coll? seq (:types r))) (pr-str (:types r)))))
