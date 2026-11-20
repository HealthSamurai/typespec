(ns typespec.regression-test
  "Runs the focused regression suites added with the top-ten improvements."
  (:require [clojure.test :as test]))

(def suites
  '[typespec.types-regression-test
    typespec.incomplete-boundary-test
    typespec.incomplete-overload-test
    typespec.maps-regression-test
    typespec.tuples-and-rows-test
    typespec.apply-test
    typespec.project-contracts-test
    typespec.protocol-contracts-test
    typespec.case-narrowing-test
    typespec.checked-guards-test
    typespec.type-operators-test
    typespec.diagnostic-json-test
    typespec.diagnostics-regression-test
    typespec.externs-regression-test
    typespec.solver-test
    typespec.generic-integration-test
    typespec.contextual-callback-test
    typespec.function-composition-test
    typespec.literal-contract-test
    typespec.generic-extern-test
    typespec.generic-maps-test
    typespec.macro-contracts-test
    typespec.review-fixes-test
    typespec.string-contracts-test
    typespec.java-generic-return-test
    typespec.declaration-resources-test
    typespec.pg-connection-regression-test
    typespec.pg-json-contract-test
    typespec.pg-precision-test
    typespec.extern-resources-test
    typespec.pg-boundary-contract-test
    typespec.metadata-autoload-test
    typespec.metadata-determinism-test
    typespec.var-initialization-test
    typespec.context-web-pg-test
    typespec.http-replace-regression-test])

(defn run-suites []
  ;; Missing namespaces are an error, never a silently skipped test group.
  (doseq [suite suites] (require suite))
  (apply test/run-tests suites))

(defn -main [& _]
  (let [{:keys [fail error]} (run-suites)]
    (shutdown-agents)
    (System/exit (if (zero? (+ fail error)) 0 1))))
