(ns http
  (:require [clojure.spec.alpha :as s]))

(s/def :probe.cookie/value string?)
(s/def :probe.cookie/cookie (s/keys :req-un [:probe.cookie/value]))
(s/def :probe.cookie/cookies (s/map-of string? :probe.cookie/cookie))
(s/def :probe.cookie/request (s/keys :req-un [:probe.cookie/cookies]))

(s/fdef get-cookie
  :args (s/cat :request :probe.cookie/request :name string?)
  :ret (s/nilable string?))
