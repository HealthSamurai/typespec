(ns typespec.incomplete-overload-test
  (:require [clojure.test :refer [deftest is]]
            [typespec.check :as c]
            [typespec.types :as t]))

(deftest no-proof-ambiguous-result
  (let [node {:op :sym :val 'x :pos {:row 1 :col 1}}
        ctx {:env {} :file {:file "overloads.clj"} :opts {}}]
    (binding [c/*diags* (atom [])]
      (with-redefs [c/infer (fn [_ _] t/unknown)]
        (is (= t/unknown
               (#'c/call-fn ctx node 'f
                {:t :fn :arities [{:params [(t/prim :string)] :ret (t/prim :string)}
                                  {:params [(t/prim :number)] :ret (t/prim :number)}]}
                [node]))))
      (is (= 1 (count @c/*diags*))))))

(deftest builtin-style-application
  (doseq [strict [false true]]
    (binding [c/*diags* (atom [])]
      (#'c/apply-type {:env {} :file {:file "numeric.clj"} :opts {:strict strict}}
       {:t :fn :arities [{:params [(t/prim :number)] :ret (t/prim :number)}]}
       [t/unknown] {:op :sym :val 'x :pos {:row 1 :col 1}} "+")
      (is (= "TSP210-01" (:code (first @c/*diags*))))
      (is (= #{:unknown} (:reasons (first @c/*diags*))))
      (is (= (if strict :error :warning) (:severity (first @c/*diags*)))))))

(deftest builtin-proven-mismatch
  (binding [c/*diags* (atom [])]
    (#'c/apply-type {:env {} :file {:file "numeric.clj"} :opts {}}
     {:t :fn :arities [{:params [(t/prim :number)] :ret (t/prim :number)}]}
     [(t/prim :string)] {:op :sym :val 'x :pos {:row 1 :col 1}} "+")
    (is (= "TSP2345" (:code (first @c/*diags*))))
    (is (= :error (:severity (first @c/*diags*))))))
