(ns probe.json-resource-write
  (:require [clojure.spec.alpha :as s]))

(s/fdef filename :args (s/cat :file string?) :ret string?)
(defn filename [file]
  (let [res {"resourceType" "StructureDefinition" "text" "old"}
        resource (assoc (dissoc res "text") "_filename" file)]
    (get resource "_filename")))

(s/fdef wrong-filename :args (s/cat :file string?) :ret int?)
(defn wrong-filename [file]
  (let [res {"resourceType" "StructureDefinition" "text" "old"}
        resource (assoc (dissoc res "text") "_filename" file)]
    (get resource "_filename")))
