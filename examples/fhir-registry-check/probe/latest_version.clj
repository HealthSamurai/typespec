(ns probe.latest-version
  (:require [clojure.spec.alpha :as s]
            [fhir.registry.semver :as semver]))

(s/def ::version string?)
(s/def ::package (s/keys :req-un [::version]))
(s/def ::versions (s/map-of string? ::package))

(s/fdef latest-package :args (s/cat :versions ::versions) :ret (s/nilable ::package))
(defn latest-package [versions]
  (let [latest-version (last (sort semver/semver-comparator (keys versions)))]
    (get versions latest-version)))

(s/fdef unexpected-number :args (s/cat :versions ::versions) :ret number?)
(defn unexpected-number [versions]
  (let [latest-version (last (sort semver/semver-comparator (keys versions)))]
    (get versions latest-version)))
