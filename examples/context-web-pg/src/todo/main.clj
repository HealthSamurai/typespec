(ns todo.main
  (:require [system] [todo.app]))

(defn env {:t/sig '[string? string? :-> string?]} [key fallback] (or (System/getenv key) fallback))

(defn config []
  {:services ["pg" "http" "todo.app"]
   :http {:port (parse-long (env "TODO_HTTP_PORT" "18081")) :max-body 8192}
   :pg {:host (env "TODO_PG_HOST" "127.0.0.1")
        :port (parse-long (env "TODO_PG_PORT" "55439"))
        :database (env "TODO_PG_DATABASE" "context_example")
        :user (env "TODO_PG_USER" "context_example")
        :password (env "TODO_PG_PASSWORD" "local-demo-only")
        :min-idle 1 :max-pool-size 2}})

(defn stop! [ctx]
  ;; Upstream http/stop does not shutdown its worker executor.
  (let [pool (get-in @(:system ctx) [:http :worker-pool])]
    (system/stop-system ctx)
    (when (instance? java.util.concurrent.ExecutorService pool)
      (.shutdownNow ^java.util.concurrent.ExecutorService pool))))

(defn -main [& _args]
  (let [ctx (system/start-system (config))]
    (.addShutdownHook (Runtime/getRuntime) (Thread. #(stop! ctx)))
    @(promise)))
