(ns probe.coalesce-vector
  (:require [clojure.spec.alpha :as s]))

(s/def ::input (s/coll-of string? :kind vector? :min-count 2 :max-count 4))
(s/def ::result (s/coll-of string? :kind vector? :min-count 1 :max-count 3))

(s/fdef trim-fragments :args (s/cat :parts ::input) :ret ::result)
(defn trim-fragments [parts]
  (-> parts drop-last vec))

(s/fdef close-fragments :args (s/cat :parts ::input) :ret ::input)
(defn close-fragments [parts]
  (-> parts drop-last vec (conj ")")))
