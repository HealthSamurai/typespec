(ns typespec.spec-conform
  "Static output types of tagged Spec conformation."
  (:require [clojure.string :as str]
            [typespec.types :as t]))

(def invalid-key :clojure.spec.alpha/invalid)

(defn- tuple [items]
  (with-meta (t/tuple items) {::t/tuple-contract true}))

(defn tagged-result
  "Type of conform when the final result is tagged by a known s/or, including failure.
  Other conformers may transform values, so their output is unknown."
  [env context form]
  (let [budget (volatile! 256)]
    (letfn [(output [context form active]
              (if (neg? (vswap! budget dec))
                t/unknown
                (cond
                  (qualified-keyword? form)
                  (let [{:keys [spec-form spec-context]} (get-in env [:specs form])]
                    (if (and spec-context (not (active form)))
                      (output spec-context spec-form (conj active form))
                      t/unknown))

                  (symbol? form)
                  (let [context (assoc context :defined-symbols (get-in env [:ns-defs (:ns context)]))
                        predicate (t/core-name context form)]
                    (if (and predicate (str/ends-with? (name predicate) "?"))
                      (t/parse-type (assoc context :fail (constantly nil) :spec? true) form)
                      t/unknown))

                  (set? form) (t/parse-type (assoc context :fail (constantly nil) :spec? true) form)
                  (seq? form)
                  (let [args (rest form)
                        child #(output context % active)]
                    (case (t/type-form-op context (first form))
                      :s/or (let [pairs (partition 2 args)]
                              (if (and (seq pairs) (even? (count args)) (every? (comp keyword? first) pairs))
                                (vary-meta (apply t/union
                                                  (for [[tag spec] pairs]
                                                    (assoc (tuple [(t/lit tag) (child spec)]) :map-entry? true)))
                                           assoc ::tagged true)
                                t/unknown))
                      :s/spec (child (first args))
                      :s/with-gen (if (= 2 (count args)) (child (first args)) t/unknown)
                      :s/and (child (last args))
                      (:fn :fn*) (or (t/predicate-spec-type context form) t/unknown)
                      :s/nilable (t/union t/nil-t (child (first args)))
                      :s/tuple (tuple (mapv child args))
                      :s/keys (let [shapes (t/members env (t/parse-type (assoc context :fail (constantly nil)) form))
                                    fields (fn [row]
                                             (update-vals row #(child (:name %))))]
                                (if (every? #(= :map (:t %)) shapes)
                                  (apply t/union (map #(-> % (update :req fields) (update :opt fields)) shapes))
                                  t/unknown))
                      t/unknown))
                  :else t/unknown)))]
      (let [result (output context form #{})]
        (when (::tagged (meta result))
          (t/union result (t/lit invalid-key)))))))
