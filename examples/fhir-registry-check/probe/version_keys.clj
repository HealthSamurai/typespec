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

(s/def ::versions (s/coll-of string? :kind vector? :min-count 1 :max-count 3))
(s/def ::newest-first (s/coll-of string? :kind seq? :min-count 1 :max-count 3))
(s/fdef newest-first :args (s/cat :versions ::versions) :ret ::newest-first)
(defn newest-first [versions]
  (reverse (sort semver/semver-comparator versions)))
