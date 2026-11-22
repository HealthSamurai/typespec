(ns probe.sql-fragment-pop
  (:require [clojure.spec.alpha :as s]))

(s/def ::input (s/coll-of string? :kind vector? :min-count 1 :max-count 2))
(s/def ::result (s/coll-of string? :kind vector? :min-count 2 :max-count 3))

(s/fdef joined-fragments :args (s/cat :parts ::input) :ret ::result)
(defn joined-fragments [parts]
  (pop (into [] (concat parts ["name" ","]))))
