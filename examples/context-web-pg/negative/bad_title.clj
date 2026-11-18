(ns bad-title (:require [todo.repo :as repo]))
;; Static-only fixture: never require/evaluate this file, it would access a DB.
(defn bad-create {:t/sig '[:context/context :-> any?]} [ctx]
  (repo/create-todo ctx 42))
