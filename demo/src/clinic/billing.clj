(ns clinic.billing
  "Maps, collections, and types taken from s/fdef and from :pre conditions."
  (:require [clojure.spec.alpha :as s]
            [clinic.specs]))

(s/fdef line-total
  :args (s/cat :line :clinic/line)
  :ret number?)
(defn line-total [{:line/keys [price qty]}]
  (* price qty))

;; OK: map and reduce over a typed collection
(s/fdef total
  :args (s/cat :invoice :clinic/invoice)
  :ret number?)
(defn total [invoice]
  (reduce + 0 (map line-total (:invoice/lines invoice))))

;; BUG 7: a typo in a key of a literal, and a required key is missing
(s/fdef new-invoice
  :args (s/cat :id :invoice/id :patient :clinic/patient)
  :ret :clinic/invoice)
(defn new-invoice [id patient]
  {:invoice/id id
   :invoice/patient (:patient/id patient)
   :invoice/payed? false})

;; BUG 8: titles are strings, they cannot be summed
(s/fdef title-length
  :args (s/cat :invoice :clinic/invoice)
  :ret number?)
(defn title-length [invoice]
  (reduce + 0 (map :line/title (:invoice/lines invoice))))

;; BUG 9: a typo in a path
(s/fdef first-price
  :args (s/cat :invoice :clinic/invoice)
  :ret (s/nilable number?))
(defn first-price [invoice]
  (get-in invoice [:invoice/lines 0 :line/prise]))

;; No s/fdef here: the parameter types are read from :pre
(defn with-tax [invoice rate]
  {:pre [(s/valid? :clinic/invoice invoice) (number? rate)]}
  (* (total invoice) (+ 1 rate)))

;; BUG 10: the rate is a string
(s/fdef monthly-total
  :args (s/cat :invoice :clinic/invoice)
  :ret number?)
(defn monthly-total [invoice]
  (with-tax invoice "0.2"))

;; BUG 11: the function may return nil, the signature says it does not
(s/fdef paid?
  :args (s/cat :invoice :clinic/invoice)
  :ret boolean?)
(defn paid? [invoice]
  (:invoice/paid? invoice))
