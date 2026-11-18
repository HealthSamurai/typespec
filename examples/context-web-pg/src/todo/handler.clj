(ns todo.handler
  (:require [http] [todo.repo :as repo] [clojure.string :as str]
            [clojure.spec.alpha :as s]))

(s/def ::status int?)
(s/def ::body any?)
(s/def ::response (s/keys :req-un [::status ::body]))

(defn response
  {:t/sig '[int? any? :-> ::response]}
  [status body]
  {:status status :body body})

;; Propagate the query result into a response builder; callback gets its input type.
(defn with-result
  {:t/sig '(t/all [a b] [a (t/fn [a :-> b]) :-> b])}
  [value build]
  (build value))

(defn list-handler
  {:t/sig '[:context/context map? :-> ::response]}
  [ctx _request]
  (with-result (repo/list-todos ctx) (fn [todos] (response 200 todos))))

(defn create-handler
  {:t/sig '[:context/context map? :-> ::response]}
  [ctx request]
  (let [title (:title (try (http/parse-body (:body request))
                           (catch com.fasterxml.jackson.core.JsonParseException _ nil)))]
    (if (and (string? title) (not (str/blank? title)) (<= (count title) 200))
      (response 201 (repo/create-todo ctx title))
      (response 400 {:error "title must be a nonblank string of at most 200 characters"}))))
