(ns app.patients
  (:require [clojure.spec.alpha :as s]
            [clojure.string :as str]))

(s/def ::db map?)
(s/def :patient/id     string?)
(s/def :patient/name   string?)
(s/def :patient/status #{"active" "inactive"})
(s/def ::patient (s/keys :req [:patient/id :patient/status] :opt [:patient/name]))

(s/fdef find-patient
  :args (s/cat :db ::db :id :patient/id)
  :ret (s/nilable ::patient))
(defn find-patient [db id]
  (get db id))

(s/fdef greeting
  :args (s/cat :patient ::patient)
  :ret string?)
(defn greeting [{:patient/keys [name status]}]
  (str (case status "active" "Hi, ")
       (str/upper-case name)))

(defn greet-first [db]
  (greeting (find-patient db 42)))
