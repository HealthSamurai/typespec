(ns probe.latest-nonempty
  (:require [clojure.spec.alpha :as s]
            [fhir.registry.semver :as semver]))

(s/def ::versions (s/coll-of string? :kind vector? :min-count 1 :max-count 3))
(s/fdef latest :args (s/cat :versions ::versions) :ret string?)
(defn latest [versions]
  (last (sort semver/semver-comparator versions)))
