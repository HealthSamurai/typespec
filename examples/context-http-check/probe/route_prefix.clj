(ns probe.route-prefix
  (:require [clojure.spec.alpha :as s]))

(s/def ::path (s/coll-of string? :kind vector? :min-count 2 :max-count 4))
(s/def ::prefix (s/coll-of string? :kind vector? :min-count 1 :max-count 3))

(s/fdef prefix :args (s/cat :path ::path) :ret ::prefix)
(defn prefix [path]
  (into [] (butlast path)))
