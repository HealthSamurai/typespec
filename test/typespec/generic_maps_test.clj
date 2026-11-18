(ns typespec.generic-maps-test
  (:require [clojure.test :refer [deftest is]]
            [typespec.check :as check]
            [typespec.parse :as parse]))

(def prelude
  "(ns map-example (:require [clojure.spec.alpha :as s]))
   (s/def ::title string?)
   (s/def ::item (s/keys :req-un [::title]))
   (defn identity-item {:t/sig '(t/all [a] [a :-> a])} [x] x)
   (defn apply-item {:t/sig '(t/all [a b] [a (t/fn [a :-> b]) :-> b])} [x f] (f x))")

(defn checked [source]
  (check/check-files [(parse/parse-string "generic-maps.clj" (str prelude source))] {}))

(deftest named-map-through-generic-call
  (let [r (checked "(defn f {:t/sig '[::item :-> ::item]} [x] (identity-item x))")]
    (is (empty? (:diagnostics r)) (pr-str (:diagnostics r)))))

(deftest named-map-contextual-callback
  (let [r (checked "(defn f {:t/sig '[::item :-> string?]} [x]
                    (apply-item x (fn [item] (:title item))))")]
    (is (empty? (:diagnostics r)) (pr-str (:diagnostics r)))))

(deftest invalid-map-results-are-not-proved
  (doseq [body ["(identity-item {})" "(identity-item {:title 42})"
                "(apply-item x (fn [item] 42))"]]
    (let [r (checked (str "(defn f {:t/sig '[::item :-> ::item]} [x] " body ")"))]
      (is (some #(= :error (:severity %)) (:diagnostics r)) (pr-str (:diagnostics r))))))

(deftest optional-field-does-not-become-required
  (let [r (checked "(s/def ::maybe-item (s/keys :opt-un [::title]))
                    (defn f {:t/sig '[::maybe-item :-> ::item]} [x] (identity-item x))")]
    (is (some #(= :error (:severity %)) (:diagnostics r)) (pr-str (:diagnostics r)))))

(deftest bad-field-access-in-contextual-callback
  (let [r (checked "(defn f {:t/sig '[::item :-> string?]} [x]
                    (apply-item x (fn [item] (subs (:title item) \"wrong-index\"))))")]
    (is (some #(= :error (:severity %)) (:diagnostics r)) (pr-str (:diagnostics r)))))
