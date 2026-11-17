(ns clinic.legacy
  "Escape hatches. They are comments for Clojure, and they are counted by Typespec."
  (:require [clinic.billing :as billing]
            [clinic.patients :as patients]))

(defn- parse-row [row]
  (zipmap [:invoice/id :invoice/patient :invoice/lines] row))

;; a cast: trust me, this is an invoice
(defn import-total [row]
  (billing/total #_(t/cast :clinic/invoice) (parse-row row)))

;; an ignore with a reason: the error below is silenced and counted
(defn legacy-greeting [db]
  #_(t/ignore "CLINIC-12: legacy ids are numbers")
  (patients/greet-or-default db 42))

;; WARNING 23: nothing to ignore here
(defn greeting [db]
  #_(t/ignore "was needed before the fix")
  (patients/greet-or-default db "p1"))
