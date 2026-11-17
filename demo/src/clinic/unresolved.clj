;; Names that do not exist. Clojure reports these too, but only when the namespace is loaded;
;; Typespec reports them from the text. A typical agent mistake: a function that sounds right.
(ns clinic.unresolved
  (:require [clojure.string :as str]
            [clinic.patients :as patients]))

;; BUG: clojure.string has no such function
(defn initials [patient]
  (str/uppercase (subs (:patient/id patient) 0 1)))

;; BUG: clinic.patients has no such function
(defn find-all [db]
  (patients/find-patients db))
