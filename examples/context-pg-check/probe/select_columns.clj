(ns probe.select-columns
  (:require [clojure.spec.alpha :as s]))

(s/def ::id int?)
(s/def ::name string?)
(s/def ::row (s/keys :req-un [::id ::name]))
(s/def ::projection (s/keys :req-un [::id]))
(s/def ::columns (s/tuple #{:id}))

(s/fdef project-row :args (s/cat :row ::row :columns ::columns) :ret ::projection)
(defn project-row [row columns]
  (select-keys row columns))
