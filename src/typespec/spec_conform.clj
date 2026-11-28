(ns typespec.spec-conform
  "Static output types of supported Spec conformation."
  (:require [clojure.string :as str]
            [typespec.types :as t]))

(def invalid-key :clojure.spec.alpha/invalid)

(defn- tuple [items]
  (with-meta (t/tuple items) {::t/tuple-contract true}))

(defn- unchanged-value? [type]
  (let [facts (meta type)]
    (or (::predicate-result facts) (::original-value facts))))

(defn- conformed-entry [key value conform-keys?]
  (cond
    (or (and conform-keys? (::tagged (meta key))) (::tagged (meta value)))
    (assoc (tuple [key value]) :jvm-class "clojure.lang.PersistentVector")

    (and (or (not conform-keys?) (unchanged-value? key)) (unchanged-value? value))
    (t/map-entry-type key value)

    :else (tuple [key value])))

(defn- conform-result [env context form supported?]
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
                      (vary-meta (t/parse-type (assoc context :fail (constantly nil) :spec? true) form)
                                 assoc ::predicate-result true)
                      t/unknown))

                  (set? form)
                  (vary-meta (t/parse-type (assoc context :fail (constantly nil) :spec? true) form)
                             assoc ::predicate-result true)
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
                      :s/and (reduce (fn [previous spec]
                                       (let [result (child spec)]
                                         (if (and (::predicate-result (meta result))
                                                  (not (#{:unknown :any} (:t previous)))
                                                  (t/sub? env previous result))
                                           previous
                                           result)))
                                     t/unknown args)
                      :s/nonconforming (if (= 1 (count args))
                                        (vary-meta (t/parse-type (assoc context :fail (constantly nil) :spec? true)
                                                                 (first args))
                                                   assoc ::original-value true)
                                        t/unknown)
                      (:fn :fn*) (if-let [result (t/predicate-spec-type context form)]
                                   (vary-meta result assoc ::predicate-result true)
                                   t/unknown)
                      :s/nilable (if (= 1 (count args))
                                   (vary-meta (t/union t/nil-t (child (first args)))
                                              assoc ::structured true)
                                   t/unknown)
                      :s/tuple (vary-meta (tuple (mapv child args)) assoc ::structured true)
                      :s/keys (let [shapes (t/members env (t/parse-type (assoc context :fail (constantly nil)) form))
                                    fields (fn [row]
                                             (update-vals row #(child (:name %))))]
                                (if (every? #(= :map (:t %)) shapes)
                                  (vary-meta
                                   (apply t/union (map #(-> % (update :req fields) (update :opt fields)) shapes))
                                   assoc ::structured true)
                                  t/unknown))
                      :s/map-of
                      (let [[key value & options] args
                            {:keys [conform-keys into]} (when (even? (count options)) (apply hash-map options))
                            input (t/parse-type (assoc context :fail (constantly nil) :spec? true) form)
                            vector-output? (and (vector? into) (empty? into))]
                        (if (and (= :map-of (:t input)) (or (nil? into) (= {} into) vector-output?)
                                 (contains? #{nil false true} conform-keys))
                          (let [key-result (if conform-keys (child key) (:k input))
                                value-result (child value)
                                result (if vector-output?
                                         (-> input
                                             (dissoc :k :v)
                                             (assoc :t :coll :kind :vector
                                                    :of (conformed-entry key-result value-result conform-keys)))
                                         (cond-> (assoc input :k key-result :v value-result)
                                           (or conform-keys into) (dissoc :min-count)))]
                            (vary-meta result assoc ::structured true))
                          t/unknown))
                      :s/coll-of
                      (let [[item & options] args
                            {:keys [into]} (when (even? (count options)) (apply hash-map options))
                            input (t/parse-type (assoc context :fail (constantly nil) :spec? true) form)
                            kind (cond
                                   (nil? into) (:kind input)
                                   (and (vector? into) (empty? into)) :vector
                                   (and (set? into) (empty? into)) :set)]
                        (if (and (= :coll (:t input)) (#{:vector :set :seq} kind))
                          (vary-meta (cond-> (assoc (dissoc input :distinct? :counted-max) :kind kind :of (child item))
                                       (some? into) (dissoc :jvm-class :list-kind?)
                                       (= :set kind) (dissoc :min-count))
                                     assoc ::structured true)
                          t/unknown))
                      t/unknown))
                  :else t/unknown)))]
      (let [result (output context form #{})]
        (when (and (not= t/unknown result) (supported? (meta result)))
          (t/union result (t/lit invalid-key)))))))

(defn tagged-result
  "Type of conform when the final result is tagged by a known s/or, including failure."
  [env context form]
  (conform-result env context form ::tagged))

(defn value-result
  "Output type of supported tagged, structured and nonconforming specs, including failure."
  [env context form]
  (conform-result env context form #(or (::tagged %) (::structured %) (::original-value %))))
