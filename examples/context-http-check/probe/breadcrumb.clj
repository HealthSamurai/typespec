(ns probe.breadcrumb
  (:require [clojure.spec.alpha :as s]))

(s/def ::labels (s/coll-of string? :kind vector? :min-count 2 :max-count 3))
(s/def ::breadcrumb (s/coll-of string? :kind seq? :min-count 3 :max-count 5))

(s/fdef breadcrumb :args (s/cat :labels ::labels) :ret ::breadcrumb)
(defn breadcrumb [labels]
  (->> labels
       (map-indexed (fn [i label] (str i label)))
       (interpose " / ")))
