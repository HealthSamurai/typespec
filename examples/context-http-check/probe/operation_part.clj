(ns probe.operation-part
  (:require [clojure.spec.alpha :as s]))

(s/def ::parts (s/tuple string? keyword? string?))
(s/def ::text-position #{0 2})

(s/fdef text-part :args (s/cat :parts ::parts :i ::text-position) :ret string?)
(defn text-part [parts i]
  (nth parts i))
