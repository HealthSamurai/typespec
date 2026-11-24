(ns typespec.java-overloads-test
  (:require [clojure.test :refer [deftest is]]
            [typespec.check :as check]
            [typespec.parse :as parse]))

(deftest java-overloads-check-the-whole-argument-list
  (let [source "(ns example.overloads)
(java.util.Arrays/equals (byte-array [1]) (byte-array [1]))
(java.util.Arrays/equals (int-array [1]) (int-array [1]))
(java.util.Arrays/equals (byte-array [1]) (int-array [1]))
(java.util.Arrays/equals (byte-array [1]) :bad)
(defn same-array [flag]
  (let [values (if flag (byte-array [1]) (int-array [1]))]
    (java.util.Arrays/equals values values)))
(defn format-or-fail [flag]
  (String/format \"%s\" (if flag (object-array [\"ok\"]) [\"bad\"])))"
        result (check/check-files [(parse/parse-string "overloads.clj" source)] {})
        diagnostics (:diagnostics result)]
    (is (= [[4 1 "TSP2345"] [5 43 "TSP2345"] [10 23 "TSP2345"]]
           (mapv (juxt (comp :row :pos) (comp :col :pos) :code) diagnostics))
        (pr-str diagnostics))
    (is (= (str "No overload of java.util.Arrays.equals accepts these arguments. "
                "(byte[], byte[]) rejects argument 2. (int[], int[]) rejects argument 1")
           (:message (first diagnostics))))))

(deftest java-overloads-check-destructured-array-specs-across-namespaces
  (let [files [(parse/parse-string
                "provider.clj"
                "(ns provider (:require [clojure.spec.alpha :as s]))
(s/fdef arrays :args (s/cat) :ret (s/tuple #(instance? byte/1 %) #(instance? int/1 %)))
(defn arrays [] [(byte-array [1]) (int-array [1])])")
               (parse/parse-string
                "consumer.clj"
                "(ns consumer (:require [provider :as p]))
(let [[bytes ints] (p/arrays)]
  (java.util.Arrays/equals bytes bytes)
  (java.util.Arrays/equals ints ints)
  (java.util.Arrays/equals bytes ints))")]
        result (check/check-files files {})]
    (is (= [["consumer.clj" 5 3 "TSP2345"]]
           (mapv (juxt :file (comp :row :pos) (comp :col :pos) :code) (:diagnostics result)))
        (pr-str (:diagnostics result)))))
