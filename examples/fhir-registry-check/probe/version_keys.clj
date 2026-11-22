(ns probe.version-keys
  (:require [clojure.spec.alpha :as s]
            [fhir.registry.semver :as semver]))

(s/def ::index (s/map-of string? map?))

(s/fdef version-names :args (s/cat :versions ::index) :ret (s/coll-of string? :kind seq?))
(defn version-names [versions]
  (sort semver/semver-comparator (keys versions)))

(s/fdef numeric-ids :args (s/cat :versions ::index) :ret (s/coll-of int? :kind seq?))
(defn numeric-ids [versions]
  (sort semver/semver-comparator (keys versions)))
