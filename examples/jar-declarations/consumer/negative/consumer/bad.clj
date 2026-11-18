(ns consumer.bad
  (:require [vendor-demo.core :as vendor]))

;; Generic identity must preserve int: passing its result to greet is invalid.
(defn bad-greeting []
  (vendor/greet (vendor/identity-value 42)))
