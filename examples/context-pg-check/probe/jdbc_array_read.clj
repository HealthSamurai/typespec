(ns probe.jdbc-array-read
  (:require [clojure.spec.alpha :as s]))

(s/def ::labels (s/coll-of string? :kind vector?))

(s/fdef first-label :args (s/cat :labels ::labels) :ret (s/nilable string?))
(defn first-label [labels]
  (aget (into-array String labels) 0))

(s/fdef first-id :args (s/cat :labels ::labels) :ret int?)
(defn first-id [labels]
  (aget (into-array String labels) 0))
