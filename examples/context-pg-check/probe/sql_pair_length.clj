(ns probe.sql-pair-length
  (:require [clojure.spec.alpha :as s]))

(s/def ::pair (s/coll-of string? :kind seq? :min-count 1 :max-count 2))

(s/fdef pick-fragment :args (s/cat :pair ::pair) :ret string?)
(defn pick-fragment [pair]
  (if (< (count pair) 2)
    (first pair)
    (second pair)))
