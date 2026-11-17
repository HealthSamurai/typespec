(ns ex.more
  (:require [clojure.spec.alpha :as s]))

(s/def :patient/id string?)
(s/def :patient/name string?)
(s/def :patient/status #{"active" "inactive"})
(s/def ::patient (s/keys :req [:patient/id :patient/status] :opt [:patient/name]))
(s/def :job/retries (s/nilable int?))
(s/def :entry/resource map?)
(s/def ::entry (s/keys :req [:entry/resource]))
(s/def :bundle/entry (s/coll-of ::entry))
(s/def ::bundle (s/keys :req [:bundle/entry]))

(defn slug ^String [^String title ^long max]
  (subs (.toLowerCase title) 0 (min max (.length title))))

(defn a [] (slug 42 "hello"))

(defn ^long total [xs] (reduce + xs))

(defn first-len
  {:t/sig '[(s/coll-of int?) :-> int?]}
  [ids]
  (let [^String s (first ids)]
    (.length s)))

(defn size
  {:t/sig '[java.io.File :-> long]}
  [f]
  (.length f))

(defn request-id-length ^long [^java.util.Map headers]
  (.length ^String (.get headers "x-request-id")))

(def by-pair
  (reify java.util.Comparator
    (compare [_ a b] (str a b))))

(defn display-name
  {:t/sig '[::patient :-> string?]}
  [{:patient/keys [id nmae]}]
  (str id " " nmae))

(defn next-try
  {:t/sig '[(s/keys :opt [:job/retries]) :-> int?]}
  [{:job/keys [retries] :or {retries 0}}]
  (inc retries))

(defn b
  {:t/sig '[::patient ::bundle :-> any?]}
  [patient bundle]
  (assoc patient :patient/status "archived")
  (get-in bundle [:bundle/entry 0 :entry/resourse]))

(defn label
  {:t/sig '[(s/nilable ::patient) :-> string?]}
  [p]
  (if p (:patient/id p) "unknown"))
