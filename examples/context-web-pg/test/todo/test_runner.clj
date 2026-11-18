(ns todo.test-runner (:require [clojure.test :as t] [todo.unit-test]))
(defn -main [& _]
  (let [r (t/run-tests 'todo.unit-test)]
    (shutdown-agents)
    (System/exit (if (zero? (+ (:fail r) (:error r))) 0 1))))
