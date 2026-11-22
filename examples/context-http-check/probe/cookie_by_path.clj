(ns probe.cookie-by-path
  (:require [clojure.spec.alpha :as s]))

(s/def ::cookie-path (s/tuple #{:cookies} string? #{:value}))

(s/fdef cookie-at :args (s/cat :request :probe.cookie/request :path ::cookie-path)
  :ret (s/nilable string?))
(defn cookie-at [request path]
  (get-in request path))
