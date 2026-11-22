(ns probe.route-write
  (:require [clojure.spec.alpha :as s]))

(s/def ::path (s/tuple #{:home} #{:get}))

(s/fdef register :args (s/cat :path ::path) :ret string?)
(defn register [path]
  (get-in (assoc-in {} path "handler") [:home :get]))

(s/fdef change :args (s/cat :path ::path) :ret string?)
(defn change [path]
  (get-in (update-in {:home {:get "handler"}} path str "!") [:home :get]))
