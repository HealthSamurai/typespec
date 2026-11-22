(ns probe.column-tail
  (:require [clojure.spec.alpha :as s]))

(s/def ::parts (s/tuple int? string?))
(s/fdef last-fragment :args (s/cat :parts ::parts) :ret string?)
(defn last-fragment [parts]
  (peek parts))
