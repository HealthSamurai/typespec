(ns typespec.java-varargs-test
  (:require [clojure.test :refer [deftest is]]
            [typespec.check :as check]
            [typespec.main :as main]
            [typespec.parse :as parse]))

(deftest java-varargs-use-the-jvm-array-parameter
  (let [result (check/check-files
                [(parse/parse-string
                  "format.clj"
                  "(ns example.format)
(String/format \"%s\" (into-array String [\"ok\"]))
(String/format \"%s\" [\"bad\"])
(String/format \"%s\" \"bad\")
(String/format \"%s\" (int-array [1]))
(String/format \"no args\")
(String/format \"no args\" nil)
(String/format java.util.Locale/ROOT \"%s\" (object-array [\"ok\"]))
(.format (java.util.Formatter.) \"%s\" (object-array [\"ok\"]))
(.format (java.util.Formatter.) \"%s\" [\"bad\"])
(java.util.List/of \"a\" \"b\")")]
                {})]
    (is (= [[3 "TSP2345"] [4 "TSP2345"] [5 "TSP2345"] [6 "TSP2554"] [10 "TSP2345"]]
           (mapv (juxt (comp :row :pos) :code) (:diagnostics result)))
        (pr-str (:diagnostics result)))
    (is (= ["String" "String" "String" "java.util.Formatter" "java.util.List"]
           (mapv #(main/type-at result "format.clj" % 1) [2 7 8 9 11])))))

(deftest java-varargs-check-array-specs-across-namespaces
  (let [files [(parse/parse-string
                "provider.clj"
                "(ns provider (:require [clojure.spec.alpha :as s]))
(s/fdef arguments :args (s/cat :values (s/tuple string?)) :ret #(instance? String/1 %))
(defn arguments [values] (into-array String values))
(s/fdef primitive :args (s/cat) :ret #(instance? int/1 %))
(defn primitive [] (int-array [42]))")
               (parse/parse-string
                "consumer.clj"
                "(ns consumer (:require [provider :as p]))
(String/format \"%s\" (p/arguments [\"ok\"]))
(String/format \"%s\" (p/primitive))")]
        result (check/check-files files {})]
    (is (= [["consumer.clj" 3 21 "TSP2345"]]
           (mapv (juxt :file (comp :row :pos) (comp :col :pos) :code) (:diagnostics result)))
        (pr-str (:diagnostics result)))
    (is (= "String" (main/type-at result "consumer.clj" 2 1)))))
