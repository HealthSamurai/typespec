(ns clinic.export
  "Type hints and Java interop. Hints are read as signatures; Java types come from the classpath."
  (:require [clojure.spec.alpha :as s]
            [clojure.string :as str])
  (:import [java.io File]
           [java.time LocalDate]
           [java.util Map]))

;; OK: a fully hinted function needs no other signature
(defn slug ^String [^String title ^long max-len]
  (let [s (str/lower-case title)]
    (subs s 0 (min max-len (.length s)))))

;; BUG 12: the arguments are swapped
(defn invoice-file-name ^String [^String title]
  (str (slug 40 title) ".pdf"))

;; BUG 13: plusDays takes a number
(defn next-visit ^LocalDate [^LocalDate last-visit]
  (.plusDays last-visit "30"))

;; BUG 14: Map.get returns null for a missing key
(defn request-id-length ^long [^Map headers]
  (.length ^String (.get headers "x-request-id")))

;; BUG 15: the hint says String, the value is a number
(s/fdef first-id-length
  :args (s/cat :ids (s/coll-of int?))
  :ret int?)
(defn first-id-length [ids]
  (let [^String s (first ids)]
    (.length s)))

;; WARNING 16: a reflective call, although the type is known from s/fdef. The fix is ready.
(s/fdef file-size
  :args (s/cat :file #(instance? File %))
  :ret int?)
(defn file-size [file]
  (.length file))

;; WARNING 17: a reflective call, and nothing is known about f
(defn extension [f]
  (last (str/split (.getName f) #"\."))) 

;; BUG 18: a hint on the name is not a return type
(defn ^long total-days [xs]
  (reduce + 0 xs))

;; BUG 19: LocalDate has no such method
(defn year ^long [^LocalDate date]
  (.getYears date))
