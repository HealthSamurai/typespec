(ns clinic.patients
  "Signatures come from s/fdef, the way spec already writes them."
  (:require [clojure.spec.alpha :as s]
            [clojure.string :as str]
            [clinic.specs]))

(s/fdef find-patient
  :args (s/cat :db :clinic/db :id :patient/id)
  :ret (s/nilable :clinic/patient))
(defn find-patient [db id]
  (get (:db/patients db) id))

;; BUG 1: a typo in a destructured key
(s/fdef display-name
  :args (s/cat :patient :clinic/patient)
  :ret string?)
(defn display-name [{:patient/keys [id nmae]}]
  (str id " " nmae))

;; BUG 2: "deceased" is not handled
(s/fdef status-label
  :args (s/cat :patient :clinic/patient)
  :ret string?)
(defn status-label [{:patient/keys [status]}]
  (case status
    "active" "Active"
    "inactive" "Inactive"))

;; BUG 3: :patient/name is optional
(s/fdef shout-name
  :args (s/cat :patient :clinic/patient)
  :ret string?)
(defn shout-name [patient]
  (str/upper-case (:patient/name patient)))

;; OK: the same thing done right, nil is handled before use
(s/fdef greeting
  :args (s/cat :patient :clinic/patient)
  :ret string?)
(defn greeting [patient]
  (if-let [n (:patient/name patient)]
    (str "Hello, " (str/capitalize n))
    "Hello"))

;; BUG 4: find-patient may return nil
(s/fdef greet-by-id
  :args (s/cat :db :clinic/db :id :patient/id)
  :ret string?)
(defn greet-by-id [db id]
  (greeting (find-patient db id)))

;; OK: nil is handled
(s/fdef greet-or-default
  :args (s/cat :db :clinic/db :id :patient/id)
  :ret string?)
(defn greet-or-default [db id]
  (let [patient (find-patient db id)]
    (if patient
      (greeting patient)
      "Hello, stranger")))

;; BUG 5: "discharged" is not a status
(s/fdef discharge
  :args (s/cat :patient :clinic/patient)
  :ret :clinic/patient)
(defn discharge [patient]
  (assoc patient :patient/status "discharged"))

;; BUG 6: the arguments are in the wrong order
(s/fdef greet-first
  :args (s/cat :db :clinic/db)
  :ret string?)
(defn greet-first [db]
  (greet-or-default "p1" db))
