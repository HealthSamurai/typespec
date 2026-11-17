(ns clinic.intake
  "Clojure 1.13 destructuring: :keys!, required keyword arguments, :missing."
  (:require [clojure.spec.alpha :as s]
            [clinic.specs]))

;; BUG 20: :keys! needs :patient/address, but the spec says it is optional
(s/fdef city
  :args (s/cat :patient :clinic/patient)
  :ret string?)
(defn city [{{:address/keys! [city]} :patient/address}]
  city)

;; OK: the same thing for an optional address
(s/fdef city-or-unknown
  :args (s/cat :patient :clinic/patient)
  :ret string?)
(defn city-or-unknown [{{:address/keys [city]} :patient/address}]
  (or city "unknown"))

(defn- connect [& {:keys! [host port]}]
  (str host ":" port))

;; BUG 21: a required keyword argument is missing
(defn start []
  (connect :host "localhost"))

(s/fdef send-welcome
  :args (s/cat :email :user/email :name :user/name)
  :ret string?)
(defn send-welcome [email name]
  (str "Welcome, " name " <" email ">"))

;; OK: with :missing the keys may be absent, and the check narrows them back
(defn register [{:user/keys! [email name] :missing m}]
  (if m
    {:error m}
    (send-welcome email name)))

;; BUG 22: :missing is bound but never checked
(defn register-fast [{:user/keys! [email name] :missing m}]
  (send-welcome email name))
