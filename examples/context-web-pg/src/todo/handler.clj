(ns todo.handler
  (:require [http] [todo.repo :as repo] [clojure.string :as str]
            [clojure.spec.alpha :as s]))

(s/def ::status int?)
(s/def ::error string?)
(s/def ::error-body (s/keys :req-un [::error]))
(s/def ::body (s/or :todo :todo.repo/todo
                   :todos (s/coll-of :todo.repo/todo :kind vector?)
                   :error ::error-body))
(s/def ::response (s/keys :req-un [::status ::body]))

(defn response
  {:t/sig '[int? (t/get ::response :body) :-> ::response]}
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

(s/def :todo.handler.valid/kind #{:valid})
(s/def :todo.handler.invalid/kind #{:invalid})
(s/def ::valid-title (s/keys :req-un [:todo.handler.valid/kind :todo.repo/title]))
(s/def ::invalid-title (s/keys :req-un [:todo.handler.invalid/kind ::error]))
(s/def ::title-result (s/or :valid ::valid-title :invalid ::invalid-title))

(defn- valid-title? [title]
  (and (string? title) (not (str/blank? title)) (<= (count title) 200)))

(s/fdef parse-title :args (s/cat :request map?) :ret ::title-result)
(defn- parse-title [request]
  (let [title (:title (try (http/parse-body (:body request))
                           (catch com.fasterxml.jackson.core.JsonParseException _ nil)))]
    (if (valid-title? title)
      {:kind :valid :title title}
      {:kind :invalid :error "title must be a nonblank string of at most 200 characters"})))

(defn create-handler
  {:t/sig '[:context/context map? :-> ::response]}
  [ctx request]
  (let [result (parse-title request)]
    (case (:kind result)
      :valid (let [create (comp (partial response 201) (partial repo/create-todo ctx))]
               (create (:title result)))
      :invalid (response 400 (select-keys result [:error])))))
