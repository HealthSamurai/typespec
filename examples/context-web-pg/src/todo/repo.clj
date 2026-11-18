(ns todo.repo
  (:require [pg] [clojure.spec.alpha :as s]))

(s/def ::id pos-int?)
(s/def ::title string?)
(s/def ::done boolean?)
(s/def ::todo (s/keys :req-un [::id ::title ::done]))

(defn checked-todo
  {:t/sig '[map? :-> ::todo]}
  [row]
  ;; SQL is not a type: validate the external row instead of trusting a cast.
  (if (s/valid? ::todo row)
    row
    (throw (ex-info "Invalid database row" {:row row}))))

(defn list-todos
  {:t/sig '[:context/context :-> (s/coll-of ::todo)]}
  [ctx]
  (mapv checked-todo
        (pg/execute! ctx {:sql ["SELECT id, title, done FROM context_example_todos ORDER BY id"]})))

(defn create-todo
  {:t/sig '[:context/context string? :-> ::todo]}
  [ctx title]
  (checked-todo
   (first (pg/execute! ctx {:sql ["INSERT INTO context_example_todos (title) VALUES (?) RETURNING id, title, done" title]}))))
