(ns consumer.good
  (:require [vendor-demo.core :as vendor]))

(defn greeting {:t/sig '[:-> string?]} []
  (vendor/greet (vendor/identity-value "Ada")))

(defn integer-result {:t/sig '[:-> int?]} []
  (inc (vendor/identity-value 41)))
