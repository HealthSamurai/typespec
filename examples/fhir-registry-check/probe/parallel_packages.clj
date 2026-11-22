(ns probe.parallel-packages
  (:require [clojure.spec.alpha :as s]))

(s/def ::names (s/coll-of string? :kind vector? :min-count 1 :max-count 3))
(s/def ::packages (s/coll-of string? :kind seq? :min-count 1 :max-count 3))

(s/fdef realize-packages :args (s/cat :names ::names) :ret ::packages)
(defn realize-packages [names]
  (->> names (pmap clojure.string/upper-case) (doall)))
