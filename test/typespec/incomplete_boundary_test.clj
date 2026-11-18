(ns typespec.incomplete-boundary-test
  (:require [clojure.test :refer [deftest is]]
            [typespec.check :as c]
            [typespec.types :as t]))

(def ctx {:env {} :file {:file "boundary.clj"} :opts {}})
(def node {:op :sym :val 'x :pos {:row 1 :col 1}})
(def string-t (t/prim :string))
(defn diagnostics [f context actual expected]
  (binding [c/*diags* (atom [])]
    (f context node actual expected)
    @c/*diags*))
(defn check-incomplete [ds reason severity]
  (is (= 1 (count ds)))
  (is (= "TSP210-01" (:code (first ds))))
  (is (= severity (:severity (first ds))))
  (is (= #{reason} (:reasons (first ds))))
  (is (re-find (re-pattern (name reason)) (:message (first ds)))))

(deftest unknown-boundaries
  (doseq [f [#'c/assignable! #'c/check-return!]]
    (check-incomplete (diagnostics f ctx t/unknown string-t) :unknown :warning)
    (check-incomplete (diagnostics f (assoc ctx :opts {:strict true}) t/unknown string-t)
                      :unknown :error)
    (is (empty? (diagnostics f ctx t/any string-t)))
    (is (= :error (:severity (first (diagnostics f ctx (t/prim :int) string-t)))))))

(deftest depth-and-unsupported
  (let [nest #(nth (iterate (fn [x] {:t :coll :of x}) %) 20)]
    (doseq [f [#'c/assignable! #'c/check-return!]]
      (check-incomplete (diagnostics f ctx (nest (t/prim :int)) (nest (t/prim :number)))
                        :depth :warning)))
  (check-incomplete
   (diagnostics #'c/assignable! ctx
                {:t :fn :arities [{:params [] :kwargs {:t :map :req {} :opt {}} :ret string-t}]}
                {:t :fn :arities [{:params [] :ret string-t}]})
   :unsupported-function :warning))

(deftest fresh-map-boundary
  (doseq [f [#'c/assignable! #'c/check-return!]]
    (check-incomplete
     (diagnostics f ctx {:t :map :fresh true :req {:x t/unknown} :opt {}}
                  {:t :map :req {:x string-t} :opt {}})
     :unknown :warning)))

(deftest actual-call-and-overload-isolation
  (binding [c/*diags* (atom [])]
    (with-redefs [c/infer (fn [_ _] t/unknown)]
      (#'c/call-fn ctx node 'f {:t :fn :arities [{:params [string-t] :ret string-t}]} [node]))
    (check-incomplete @c/*diags* :unknown :warning))
  (binding [c/*diags* (atom [])]
    (with-redefs [c/infer (fn [_ _] t/unknown)]
      (#'c/call-fn ctx node 'f {:t :fn :arities [{:params [string-t] :ret string-t}
                                                {:params [t/any] :ret string-t}]} [node]))
    ;; Rejected candidates must not publish warnings during overload search.
    (is (empty? @c/*diags*))))
