(ns probe.sql-fragment-vector
  (:require [clojure.spec.alpha :as s]))

(s/def ::input (s/coll-of string? :kind vector? :min-count 1 :max-count 2))
(s/def ::result (s/coll-of string? :kind vector? :min-count 3 :max-count 4))

(s/fdef append-fragments :args (s/cat :parts ::input) :ret ::result)
(defn append-fragments [parts]
  (into [] (concat parts ["name" ","])))
