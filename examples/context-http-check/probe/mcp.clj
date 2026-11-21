;; Supplemental Spec for the two request variants exercised by the MCP probe.
(ns http.mcp
  (:require [clojure.spec.alpha :as s]))

(s/def :probe.call/method #{"tools/call"})
(s/def :probe.call/name string?)
(s/def :probe.call/arguments any?)
(s/def :probe.call/params
  (s/keys :req-un [:probe.call/name] :opt-un [:probe.call/arguments]))
(s/def :probe/call
  (s/keys :req-un [:probe.call/method :probe.call/params] :opt-un [:probe.call/name]))

(s/def :probe.list/method #{"tools/list"})
(s/def :probe.list/params nil?)
(s/def :probe/list
  (s/keys :req-un [:probe.list/method :probe.list/params] :opt-un [:probe.call/name]))

(s/def :probe/request (s/or :call :probe/call :list :probe/list))
(s/fdef mcp-handler :args (s/cat :context any? :request :probe/request) :ret map?)
