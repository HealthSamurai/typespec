(ns probe.artifact-index
  (:require [clojure.spec.alpha :as s]))

(s/def ::artifacts (s/map-of string? string?))
(s/def ::index (s/map-of string? ::artifacts))
(s/def ::slot (s/tuple string? string?))

(s/fdef register :args (s/cat :index ::index :slot ::slot :artifact string?) :ret ::index)
(defn register [index slot artifact]
  (assoc-in index slot artifact))

(s/fdef bad-register :args (s/cat :index ::index :slot ::slot) :ret ::index)
(defn bad-register [index slot]
  (assoc-in index slot 42))
