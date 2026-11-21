(ns response-probe
  (:require [clojure.spec.alpha :as s]))

(s/def ::headers (s/map-of string? string?))
(s/def ::response (s/keys :req-un [::headers]))
(s/fdef accept-response :args (s/cat :response ::response) :ret any?)
(defn accept-response [response] response)

(accept-response (assoc-in {:headers {"vary" "Accept"}} [:headers "content-type"] "application/json"))
(accept-response (assoc-in {:headers {"vary" "Accept"}} [:headers "content-type"] 42))
