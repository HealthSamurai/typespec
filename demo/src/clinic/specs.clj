(ns clinic.specs
  "The domain model. Plain clojure.spec: nothing here was written for Typespec."
  (:require [clojure.spec.alpha :as s]))

(s/def :patient/id string?)
(s/def :patient/name string?)
(s/def :patient/status #{"active" "inactive" "deceased"})
(s/def :address/city string?)
(s/def :address/zip string?)
(s/def :patient/address (s/keys :req [:address/city] :opt [:address/zip]))
(s/def :clinic/patient
  (s/keys :req [:patient/id :patient/status]
          :opt [:patient/name :patient/address]))

(s/def :line/title string?)
(s/def :line/price number?)
(s/def :line/qty int?)
(s/def :clinic/line (s/keys :req [:line/title :line/price :line/qty]))

(s/def :invoice/id string?)
(s/def :invoice/patient :patient/id)
(s/def :invoice/lines (s/coll-of :clinic/line))
(s/def :invoice/paid? boolean?)
(s/def :clinic/invoice
  (s/keys :req [:invoice/id :invoice/patient :invoice/lines]
          :opt [:invoice/paid?]))

(s/def :db/patients (s/map-of :patient/id :clinic/patient))
(s/def :clinic/db (s/keys :req [:db/patients]))

(s/def :user/email string?)
(s/def :user/name string?)
