(ns typespec.maps-regression-test
  (:require [clojure.test :refer [deftest is testing]]
            [typespec.check :as check]
            [typespec.parse :as parse]
            [typespec.types :as t]))

(defn infer-type
  ([source] (infer-type source {}))
  ([source locals]
   (let [file (parse/parse-string "maps-regression.clj" source)]
     (binding [check/*diags* (atom [])]
       (check/infer {:env {} :file file :locals locals :opts {:strict-nil true}}
                    (first (:forms file)))))))

(defn alternatives [ty] (set (t/members {} ty)))
(defn map-type [req opt] {:t :map :req req :opt opt})

(deftest defaults-replace-absence-not-nil
  (doseq [source ["(get m :x 42)" "(:x m 42)"]]
    (testing source
      (is (= t/nil-t (infer-type source {'m (map-type {:x t/nil-t} {})})))
      (is (= #{t/nil-t (t/prim :string) (t/lit 42)}
             (alternatives (infer-type source {'m (map-type {} {:x (t/union t/nil-t (t/prim :string))})}))))
      (is (= (t/lit 42) (infer-type source {'m t/nil-t})))
      (is (= (t/prim :string) (infer-type source {'m (map-type {:x (t/prim :string)} {})})))
      (is (= #{t/nil-t (t/lit 42)}
             (alternatives (infer-type source {'m (t/union t/nil-t (map-type {:x t/nil-t} {}))}))))))
  (is (= t/nil-t (infer-type "(get {:x nil} :x 42)")))
  (is (= t/nil-t (infer-type "(:x {:x nil} 42)"))))

(deftest merge-required-left-optional-right
  (let [a (map-type {:x (t/prim :string)} {:z (t/prim :string)})
        b (map-type {} {:x (t/prim :int) :z t/nil-t})
        merged (infer-type "(merge a b)" {'a a 'b b})]
    (is (= #{(t/prim :string) (t/prim :int)} (alternatives (get-in merged [:req :x]))))
    (is (not (contains? (:opt merged) :x)))
    (is (= #{(t/prim :string) t/nil-t} (alternatives (get-in merged [:opt :z]))))
    (is (= (t/prim :string) (get-in (infer-type "(merge b a)" {'a a 'b b}) [:req :x])))
    (is (= t/nil-t (get-in (infer-type "(merge a b c)" {'a a 'b b 'c (map-type {:x t/nil-t} {})}) [:req :x])))))

(deftest clojure-truthiness
  (doseq [[source expected]
          [["(if false :yes :no)" :no]
           ["(if nil :yes :no)" :no]
           ["(if 0 :yes :no)" :yes]
           ["(if \"\" :yes :no)" :yes]
           ["(if (some? false) :yes :no)" :yes]
           ["(if-not false :yes :no)" :yes]
           ["(if-let [x false] :yes :no)" :no]
           ["(if-some [x false] :yes :no)" :yes]]]
    (is (= (t/lit expected) (infer-type source)) source))
  (is (= (t/lit true) (infer-type "(some? false)")))
  (is (= (t/lit false) (infer-type "(nil? false)")))
  (is (= (t/prim :boolean) (infer-type "(some? x)" {'x t/unknown})))
  (is (= #{(t/lit :yes) (t/lit :no)}
         (alternatives (infer-type "(if (some? x) :yes :no)" {'x t/unknown}))))
  (is (= t/nil-t (infer-type "(when false 1)")))
  (is (= (t/lit 1) (infer-type "(when-not false 1)")))
  (is (= (t/lit false) (infer-type "(and false 1)")))
  (is (= (t/lit 1) (infer-type "(or false 1)")))
  (is (= t/nil-t (infer-type "(when-let [x false] x)")))
  (is (= (t/lit false) (infer-type "(when-some [x false] x)"))))

(deftest branches-narrow-false-and-nil-separately
  (let [locals {'x (t/union t/nil-t (t/prim :boolean) (t/prim :string))}]
    (is (= #{(t/lit true) (t/prim :string) (t/lit :no)}
           (alternatives (infer-type "(if x x :no)" locals))))
    (is (= #{t/nil-t (t/lit false) (t/lit :yes)}
           (alternatives (infer-type "(if x :yes x)" locals))))
    (is (= #{(t/prim :boolean) (t/prim :string) (t/lit :no)}
           (alternatives (infer-type "(if (some? x) x :no)" locals))))
    (is (= #{t/nil-t (t/lit :yes)}
           (alternatives (infer-type "(if (some? x) :yes x)" locals))))
    (is (= #{t/nil-t (t/lit :no)}
           (alternatives (infer-type "(if (nil? x) x :no)" locals))))))
