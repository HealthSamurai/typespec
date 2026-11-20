(ns todo.app
  (:require [system] [http] [pg] [clojure.java.io :as io]
            [clojure.spec.alpha :as s]
            [todo.handler :as handler]))

(s/def ::method #{:get :post})
(s/def ::path string?)
(s/def ::public boolean?)
(s/def ::fn ifn?)
(s/def ::route (s/keys :req-un [::method ::path ::public ::fn]))

(system/defmanifest
  {:description "Disposable todo example" :deps ["pg" "http"]})

(system/defstart [ctx _config]
  ;; One idempotent setup statement, not a versioned migration engine.
  (pg/execute! ctx {:sql [(slurp (io/resource "setup.sql"))]})
  (http/register-endpoint ctx #_(t/satisfies ::route) #_(t/const)
                          {:method :get :path "/todos" :fn (var handler/list-handler) :public true})
  (http/register-endpoint ctx #_(t/satisfies ::route) #_(t/const)
                          {:method :post :path "/todos" :fn (var handler/create-handler) :public true})
  {})

(system/defstop [_ctx _state] nil)
