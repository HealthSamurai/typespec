(ns endpoint-probe
  (:require [clojure.spec.alpha :as s]
            [http :as http]))

(s/fdef accept-endpoint :args (s/cat :endpoint ::http/endpoint) :ret any?)
(defn accept-endpoint [endpoint] endpoint)

(accept-endpoint {:path "/" :method :get :fn (fn [_] nil) :middleware []})
(accept-endpoint {:path "/" :method :post :fn (fn [_] nil) :middleware '()})
(accept-endpoint {:path "/" :method :trace :fn (fn [_] nil)})
(accept-endpoint {:path "/" :method :get :fn (fn [_] nil) :middleware {:auth true}})
