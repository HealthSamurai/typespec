(ns ring.util.request
  (:require [clojure.spec.alpha :as s]))

(s/def :probe.ring/body
  (s/nilable (s/or :string string?
                   :sequence (partial instance? clojure.lang.ISeq)
                   :file (partial instance? java.io.File)
                   :stream (partial instance? java.io.InputStream))))
(s/def :probe.ring/request (s/keys :opt-un [:probe.ring/body]))
(s/fdef body-string
  :args (s/cat :request :probe.ring/request)
  :ret (s/nilable string?))
