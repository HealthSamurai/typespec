(ns typespec.macro-contracts
  "Opt-in, trusted binding-body macro assumptions, NOT verified macro expansions.
  No macros are loaded, expanded or executed. Contracts describe only body locals
  and its expected return, not the macro expression's value or generated vars.
  :macro-contracts {qualified/macro {:shape :binding-body
                                   :bindings [type-at-index-0 ...] :ret type}}.
  Only a first argument vector of distinct simple symbols is supported; remaining
  arguments are body forms. Destructuring, rest bindings and other shapes fail closed."
  (:require [clojure.walk :as walk]
            [typespec.types :as t]))

(defn compile-contracts
  "Validate inert contract data against collected specs. Invalid entries are never
  installed. Type references must be qualified and present in env; no source
  namespace aliases apply to config types. A key that names a function known to
  env, or a form with a built-in checker rule (built-in? sym), is rejected: a
  contract would silently replace the real check. Returns {:contracts :diagnostics}."
  ([config env] (compile-contracts config env (constantly false)))
  ([config env built-in?]
  (let [diags (atom [])
        error! (fn [message]
                 (swap! diags conj {:code "TSP9004" :severity :error
                                   :file "typespec.edn"
                                   :pos {:row 1 :col 1 :end-row 1 :end-col 1}
                                   :message message}))
        parse (fn [form]
                (let [ty (t/parse-type {:aliases {} :imports {}
                                        :fail #(throw (ex-info (str "not a type: " (pr-str %)) {}))} form)]
                  (walk/postwalk
                   (fn [x]
                     (when (and (map? x) (= :ref (:t x))
                                (nil? (get-in env [:specs (:name x)])))
                       (throw (ex-info (str "unknown type reference " (pr-str x)) {})))
                     x) ty)
                  ty))]
    {:contracts
     (if-not (map? config)
       (do (error! ":macro-contracts must be a map") {})
       (reduce-kv
        (fn [acc sym contract]
          (try
            (when-not (and (qualified-symbol? sym) (not= "clojure.core" (namespace sym)))
              (throw (ex-info "macro key must be a qualified non-core symbol" {})))
            (when (and (get-in env [:vars sym]) (not (contains? (:macros env) sym)))
              (throw (ex-info "names a function, not a macro; its calls are checked by its signature" {})))
            (when (built-in? sym)
              (throw (ex-info "this form already has a built-in rule" {})))
            (when-not (and (map? contract)
                           (= #{:shape :bindings :ret} (set (keys contract)))
                           (= :binding-body (:shape contract))
                           (vector? (:bindings contract)))
              (throw (ex-info "expected exactly {:shape :binding-body :bindings [types...] :ret type}" {})))
            (assoc acc sym {:bindings (mapv parse (:bindings contract))
                            :ret (parse (:ret contract))})
            (catch Exception e
              (error! (str "macro contract " (pr-str sym) ": " (.getMessage e)))
              acc))) {} config))
     :diagnostics @diags})))

(defn valid-bindings?
  "Whether a parsed invocation matches the fixed, simple-symbol binder vector."
  [node contract]
  (let [bindings (second (:children node))
        names (map :val (:children bindings))
        ;; _ may repeat in Clojure binding vectors: it is never read.
        used (remove #{'_} names)]
    (and (= :vector (:op bindings))
         (= (count (:bindings contract)) (count names))
         (every? #(and (= :sym (:op %)) (simple-symbol? (:val %))
                       (not= '& (:val %))) (:children bindings))
         (= (count used) (count (distinct used))))))
