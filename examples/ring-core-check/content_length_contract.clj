(ns ring.middleware.content-length
  (:require [clojure.spec.alpha :as s]))

(s/fdef body-size-in-bytes
  :args (s/cat :body any? :response any?)
  :ret (s/nilable int?))
