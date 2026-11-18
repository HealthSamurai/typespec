(ns todo.unit-test
  (:require [clojure.test :refer :all] [pg] [todo.repo :as repo]
            [todo.handler :as handler]))
(def ctx {:system (atom {})})
(def row {:id 1 :title "learn context" :done false})

(deftest queries-are-parameterized
  (let [calls (atom []) title "'); DROP TABLE context_example_todos; --"]
    (with-redefs [pg/execute! (fn [c query]
                              (is (identical? ctx c))
                              (swap! calls conj query)
                              [(assoc row :title title)])]
      (is (= title (:title (repo/create-todo ctx title)))))
    (is (= ["INSERT INTO context_example_todos (title) VALUES (?) RETURNING id, title, done" title]
           (:sql (first @calls))))))

(deftest list-query-and-row-validation
  (with-redefs [pg/execute! (fn [_ q]
                            (is (= ["SELECT id, title, done FROM context_example_todos ORDER BY id"] (:sql q)))
                            [row])]
    (is (= [row] (repo/list-todos ctx))))
  (with-redefs [pg/execute! (fn [_ _] [{:id "wrong"}])]
    (is (thrown? clojure.lang.ExceptionInfo (repo/list-todos ctx)))))

(deftest handlers
  (with-redefs [repo/list-todos (fn [c] (is (= ctx c)) [row])
                repo/create-todo (fn [c title] (is (= ctx c)) (assoc row :title title))]
    (is (= {:status 200 :body [row]} (handler/list-handler ctx {})))
    (is (= 201 (:status (handler/create-handler ctx {:body "{\"title\":\"hello\"}"})))))
  (with-redefs [repo/create-todo (fn [& _] (throw (ex-info "must not reach DB" {})))]
    (doseq [body [nil {} {:title nil} {:title 42} {:title " "} {:title (apply str (repeat 201 "a"))} "{broken" "null" "42" "[]" "true" "\"text\""]]
      (is (= 400 (:status (handler/create-handler ctx {:body body})))))))
