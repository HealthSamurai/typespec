(ns typespec.type-operators
  "Reduce map type operators against the complete declaration environment."
  (:require [clojure.string :as str]
            [clojure.walk :as walk]
            [typespec.types :as t]))

(defn- fail [node code message]
  (throw (ex-info message {::failure true :code code :form (::t/operator-form (meta node))})))

(declare evaluate)

(defn- map-shape [env origin input seen budget]
  (when (neg? (vswap! budget dec))
    (fail origin "TSP270-03" "type operator exceeded the map resolution limit"))
  (let [{::t/keys [invalid-operator shape-form shape-conflicts]} (meta input)]
    (cond
      invalid-operator (fail origin nil "invalid operand")
      shape-form (fail origin "TSP270-03" (str "expected a single s/keys shape, got " shape-form))
      (seq shape-conflicts)
      (fail origin "TSP270-03" (str "conflicting declarations for " (str/join ", " shape-conflicts)))
      :else
      (case (:t input)
        :map input
        :operator (recur env origin (evaluate env input seen budget) seen budget)
        :ref (let [name (:name input)
                   target (get-in env [:specs name :type])]
               (cond
                 (seen name) (fail origin "TSP270-03" (str "cyclic map reference " name))
                 target (recur env origin target (conj seen name) budget)
                 :else (fail origin "TSP270-03" (str "undefined map type " name))))
        (fail origin "TSP270-03" (str "expected a single s/keys shape, got " (t/show input)))))))

(defn- evaluate [env {:keys [operator input keys] :as node} seen budget]
  (let [{:keys [req opt] :as source} (map-shape env node input seen budget)
        fields (merge req opt)
        missing (remove #(contains? fields %) keys)
        ordered (sort-by pr-str (clojure.core/keys fields))]
    (when (seq missing)
      (fail node "TSP270-02" (str operator " refers to undeclared "
                                  (str/join ", " missing) "; declared keys: " (pr-str (vec ordered)))))
    (let [result (case operator
                   t/pick (assoc source :req (select-keys req keys) :opt (select-keys opt keys))
                   t/omit (assoc source :req (apply dissoc req keys) :opt (apply dissoc opt keys))
                   t/partial (assoc source :req {} :opt (merge opt req))
                   t/keys-of (apply t/union (map t/lit ordered))
                   t/get (let [key (first keys)]
                           (or (get req key) (t/union (get opt key) t/nil-t))))]
      (vary-meta result merge (meta node)))))

(defn normalize
  "Resolve operators in a type. Other references, including recursive field types, stay symbolic.
  report! receives a diagnostic code, the original form, and a message."
  [env ty report!]
  (walk/postwalk
   (fn [node]
     (case (when (map? node) (:t node))
       :operator
       (try
         (evaluate env node #{} (volatile! 256))
         (catch clojure.lang.ExceptionInfo e
           (let [{::keys [failure] :keys [code form]} (ex-data e)]
             (when-not failure (throw e))
             (when code (report! code form (ex-message e)))
             (with-meta t/unknown {::t/invalid-operator true}))))
       :union (let [alts (:types node)
                    reduced (vary-meta (apply t/union alts) merge (meta node))]
                (cond-> reduced
                  (some #(-> % meta ::t/invalid-operator) alts) (vary-meta assoc ::t/invalid-operator true)))
       node))
   ty))

(defn invalid?
  "Whether a normalized type contains a failed operator recovery."
  [ty]
  (boolean (some #(-> % meta ::t/invalid-operator) (tree-seq coll? seq ty))))
