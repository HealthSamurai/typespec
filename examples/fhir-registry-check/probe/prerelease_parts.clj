(ns probe.prerelease-parts
  (:require [clojure.spec.alpha :as s]))

(s/def ::parts (s/nilable (s/coll-of string? :kind vector?)))

(s/fdef part-at :args (s/cat :parts ::parts :idx int?) :ret (s/nilable string?))
(defn part-at [parts idx] (get parts idx))

(s/fdef incorrect-part-at :args (s/cat :parts ::parts :idx int?) :ret (s/nilable int?))
(defn incorrect-part-at [parts idx] (get parts idx))
