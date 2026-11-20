(ns bad-route
  (:require [todo.app :as app]
            [todo.handler :as handler]))

#_(t/satisfies ::app/route)
{:method :delete :paht "/todos" :public true :fn (var handler/list-handler)}
