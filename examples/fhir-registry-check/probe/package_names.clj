(ns probe.package-names
  (:require [clojure.spec.alpha :as s]))

(s/def ::name string?)
(s/def ::package (s/keys :req-un [::name]))
(s/def ::packages (s/coll-of ::package :kind vector? :min-count 1 :max-count 3))
(s/def ::names (s/coll-of string? :kind vector? :min-count 1 :max-count 3))

(s/fdef names :args (s/cat :packages ::packages) :ret ::names)
(defn names [packages]
  (mapv :name packages))
