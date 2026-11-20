(ns typespec.literals
  "Evidence checks for a value that claims to satisfy a type without an opt-out."
  (:require [typespec.env :as env]
            [typespec.type-operators :as operators]
            [typespec.types :as t]))

(defn- type-issue [env ty contract?]
  (let [budget (volatile! 1024)]
    (letfn [(visit [value active]
              (cond
                (and (map? value) (keyword? (:t value)))
                (let [tag (:t value)
                      shape (::t/shape-form (meta value))]
                  (or (when (neg? (vswap! budget dec)) :analysis-limit)
                      (when-not contract? (or (t/assumption value)
                                              (when (:generic-failure value) :unknown)))
                      (when (and contract?
                                 (or (and shape (not (#{"nilable" "or"} (name shape))))
                                     (::t/unchecked-contract (meta value))
                                     (::t/shape-conflicts (meta value))
                                     (#{:all :map-of :merge} tag)))
                        :unsupported-contract)
                      (case tag
                        :ref (let [k (:name value)]
                               (cond (active k) :recursive-type
                                     (get-in env [:specs k :type])
                                     (visit (get-in env [:specs k :type]) (conj active k))
                                     :else :unresolved-type))
                        :class (when-not (t/load-class (:name value)) :unresolved-class)
                        :any (when-not contract? :any)
                        :unknown (cond (not contract?) :unknown (seq active) :opaque-contract)
                        (:var :ivar :rigid :all :operator) :unresolved-type
                        (some #(visit % active) (vals value)))))
                (coll? value) (some #(visit % active) value)))]
      (visit ty #{}))))

(defn parse-contract [env file form]
  (let [invalid? (volatile! false)
        fail! (fn [& _] (vreset! invalid? true))
        ty (try
             (operators/normalize env
                                  (t/parse-type (assoc (env/type-ctx file fail!) :operator-error fail!) form)
                                  fail!)
             (catch IllegalArgumentException _ (fail!))
             (catch IndexOutOfBoundsException _ (fail!)))]
    (if (or @invalid? (operators/invalid? ty))
      {:reason :invalid-contract}
      (if-let [issue (type-issue env ty true)]
        {:reason issue}
        {:type ty}))))

(defn evidence-issue [env ty]
  (type-issue env ty false))
