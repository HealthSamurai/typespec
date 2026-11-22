(ns probe.json-resource-type
  (:require [clojure.spec.alpha :as s]))

(s/fdef resource-type :args (s/cat) :ret string?)
(defn resource-type []
  (let [res {"url" "urn:fixture" "resourceType" "StructureDefinition"}]
    (get res "resourceType")))

(s/fdef incorrect-resource-type :args (s/cat) :ret int?)
(defn incorrect-resource-type []
  (let [res {"url" "urn:fixture" "resourceType" "StructureDefinition"}]
    (get res "resourceType")))
