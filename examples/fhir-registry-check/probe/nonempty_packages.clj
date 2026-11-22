(ns probe.nonempty-packages
  (:require [clojure.spec.alpha :as s]))

(s/def ::names (s/coll-of string? :kind vector? :max-count 3))

(s/fdef first-package :args (s/cat :names ::names) :ret string?)
(defn first-package [names]
  (if (empty? names) "none" (first names)))
