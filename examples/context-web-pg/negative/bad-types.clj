(ns bad-types
  (:require [clojure.string :as str]
            [todo.handler :as handler]
            [todo.repo :as repo]))

;; Static-only checks using the application's real specs and signatures.
(defn missing-title-case
  {:t/sig '[:todo.handler/title-result :-> string?]}
  [result]
  (case (:kind result) :valid (:title result)))

(defn unknown-response-key
  {:t/sig '[(t/get :todo.handler/response :content) :-> any?]}
  [x]
  x)

(defn wrong-list-callback
  {:t/sig '[:context/context :-> string?]}
  [ctx]
  (handler/with-result (repo/list-todos ctx) (fn [rows] (str/upper-case rows))))

(defn wrong-create-wrapper
  {:t/sig '[:context/context :-> any?]}
  [ctx]
  (comp str/upper-case (partial repo/create-todo ctx)))
