(ns probe.http-parts
  (:require [clojure.spec.alpha :as s]))

(s/def ::candidate-paths (s/coll-of (s/nilable string?) :kind vector? :max-count 3))
(s/def ::paths (s/coll-of string? :kind seq? :max-count 3))
(s/fdef available-paths :args (s/cat :paths ::candidate-paths) :ret ::paths)
(defn available-paths [paths]
  (filter #(not= nil %) paths))

(s/def ::qualified-parts (s/coll-of string? :kind seq? :max-count 2))
(s/fdef qualified-parts :args (s/cat :value keyword?) :ret ::qualified-parts)
(defn qualified-parts [value]
  (remove nil? [(namespace value) (name value)]))
