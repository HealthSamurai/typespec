(ns todo.app
  (:require [system] [http] [pg] [clojure.java.io :as io]
            [todo.handler :as handler]))

(system/defmanifest
  {:description "Disposable todo example" :deps ["pg" "http"]})

(system/defstart [ctx _config]
  ;; One idempotent setup statement, not a versioned migration engine.
  (pg/execute! ctx {:sql [(slurp (io/resource "setup.sql"))]})
  (http/register-endpoint ctx {:method :get :path "/todos" :fn #'handler/list-handler :public true})
  (http/register-endpoint ctx {:method :post :path "/todos" :fn #'handler/create-handler :public true})
  {})

(system/defstop [_ctx _state] nil)
