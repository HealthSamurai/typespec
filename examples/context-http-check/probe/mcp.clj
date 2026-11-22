;; Supplemental Spec for the MCP request variants and sorted tool list.
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

(s/def :probe/tool (s/keys :req-un [:probe.call/name]))
(s/def :probe/tools (s/coll-of :probe/tool :kind vector? :min-count 1 :max-count 3))
(s/def :probe/sorted-tools (s/coll-of :probe/tool :kind seq? :min-count 1 :max-count 3))
(s/fdef probe-sort-tools :args (s/cat :tools :probe/tools) :ret :probe/sorted-tools)
(defn probe-sort-tools [tools] (sort-by :name tools))
