(ns todo.integration-test
  (:require [clojure.test :refer :all] [system] [todo.main :as main]
            [cheshire.core :as json])
  (:import [java.time Duration] [java.net URI] [java.net.http HttpClient HttpRequest HttpRequest$BodyPublishers HttpResponse$BodyHandlers]))

(defn request [method body]
  (let [builder (HttpRequest/newBuilder (URI/create (str "http://127.0.0.1:" (main/env "TODO_HTTP_PORT" "18081") "/todos")))
        req (-> builder (.timeout (Duration/ofSeconds 10)) (.header "Content-Type" "application/json")
                (.method method (if body (HttpRequest$BodyPublishers/ofString body) (HttpRequest$BodyPublishers/noBody))) .build)
        response (.send (-> (HttpClient/newBuilder) (.connectTimeout (Duration/ofSeconds 5)) .build) req (HttpResponse$BodyHandlers/ofString))]
    {:status (.statusCode response) :body (json/parse-string (.body response) true)}))

(deftest real-http-and-postgres
  (let [ctx (system/start-system (main/config))
        title (str "'); DROP TABLE context_example_todos; -- " (random-uuid))]
    (try
      (is (= 400 (:status (request "POST" "{\"title\":42}"))))
      (is (= 400 (:status (request "POST" "{broken"))))
      (let [created (request "POST" (json/generate-string {:title title}))
            listed (request "GET" nil)]
        (is (= 201 (:status created)))
        (is (= title (get-in created [:body :title])))
        (is (= false (get-in created [:body :done])))
        (is (= 200 (:status listed)))
        (is (some #(= (:body created) %) (:body listed))))
      (finally (main/stop! ctx)))))

(defn -main [& _]
  (let [r (run-tests 'todo.integration-test)]
    (shutdown-agents)
    (System/exit (if (zero? (+ (:fail r) (:error r))) 0 1))))
