(ns probe.migration-files
  (:require [clojure.spec.alpha :as s]))

(s/def ::file string?)
(s/def ::migration (s/keys :req-un [::file]))
(s/def ::migrations (s/coll-of ::migration :kind vector? :min-count 1 :max-count 3))
(s/def ::selected (s/coll-of ::migration :kind vector? :max-count 3))

(s/fdef select-file :args (s/cat :file string? :migrations ::migrations) :ret ::selected)
(defn select-file [file migrations]
  (filterv (fn [x] (= file (:file x))) migrations))
