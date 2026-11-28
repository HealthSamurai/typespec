(ns typespec.macro-contracts
  "Source-derived Spec checks and opt-in, trusted binding-body assumptions.
  No macros are loaded, expanded or executed. Contracts describe only body locals
  and its expected return, or the shape of a function declaration.
  :macro-contracts {qualified/macro {:shape :binding-body
                                   :bindings [type-at-index-0 ...] :ret type}}.
  Only a first argument vector of distinct simple symbols is supported; remaining
  arguments are body forms. Destructuring, rest bindings and other shapes fail closed."
  (:require [clojure.string :as str]
            [clojure.walk :as walk]
            [typespec.env :as env]
            [typespec.types :as t]))

(defn- malli-definition? [contract]
  (and (map? contract)
       (= :malli-defn (:shape contract))
       (every? #{:shape :private?} (keys contract))
       (boolean? (get contract :private? false))))

(declare ^:private strip-malli-bindings)

(defn- strip-malli-pattern [node]
  (case (:op node)
    :sym (when (simple-symbol? (:val node)) node)
    :vector (strip-malli-bindings node)
    :map (let [pairs (for [[key value] (partition 2 (:children node))]
                       [(if (#{:vector :map} (:op key)) (strip-malli-pattern key) key) value])]
           (when (every? first pairs)
             (assoc node :children (vec (mapcat identity pairs)))))
    :kw (when (= :as (:val node)) node)
    nil))

(defn- strip-malli-bindings [params]
  (loop [[binding & more] (:children params) result []]
    (if binding
      (let [[separator schema & tail] more
            annotated? (= :- (:val separator))
            pattern (strip-malli-pattern binding)]
        (when (and pattern (or (not annotated?) (and schema (not (#{'& :as} (:val pattern))))))
          (recur (if annotated? tail more) (conj result pattern))))
      (assoc params :children result))))

(defn- malli-arity [[params & body]]
  (when (= :vector (:op params))
    (when-let [params (strip-malli-bindings params)]
      (into [params] body))))

(defn- malli-declaration [form private?]
  (let [[head name & tail] (:children form)
        tail (if (= :- (:val (first tail))) (drop 2 tail) tail)
        [doc tail] (if (= :str (:op (first tail))) [(first tail) (rest tail)] [nil tail])
        [attr tail] (if (= :map (:op (first tail))) [(first tail) (rest tail)] [nil tail])
        arities (cond
                  (= :vector (:op (first tail))) (malli-arity tail)
                  (seq tail) (let [trailing (when (= :map (:op (last tail))) (last tail))
                                   forms (if trailing (butlast tail) tail)
                                   bodies (mapv (fn [arity]
                                                  (when (= :list (:op arity))
                                                    (when-let [body (malli-arity (:children arity))]
                                                      (assoc arity :children body))))
                                                forms)]
                               (when (and (seq forms) (every? some? bodies))
                                 (cond-> bodies trailing (conj trailing)))))]
    (when (and (= :sym (:op name)) (simple-symbol? (:val name)) arities)
      (let [head (assoc head :val (if private? 'clojure.core/defn- 'clojure.core/defn))
            prefix (cond-> [head name] doc (conj doc) attr (conj attr))]
        (assoc form :children (into prefix arities))))))

(defn normalize-definitions
  "Apply inert declaration shapes to top-level forms before building the Var index."
  [files config]
  (let [definitions (when (map? config)
                      (into {} (filter (fn [[sym contract]]
                                         (and (qualified-symbol? sym)
                                              (not= "clojure.core" (namespace sym))
                                              (malli-definition? contract))))
                            config))]
    (if (seq definitions)
      (mapv (fn [file]
              (update file :forms
                      (fn [forms]
                        (mapv (fn [form]
                                (if-let [contract (get definitions (env/head file form))]
                                  (or (malli-declaration form (:private? contract)) form)
                                  form))
                              forms))))
            files)
      files)))

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
            (cond
              (malli-definition? contract) (assoc acc sym contract)
              (and (map? contract)
                   (= #{:shape :bindings :ret} (set (keys contract)))
                   (= :binding-body (:shape contract))
                   (vector? (:bindings contract)))
              (assoc acc sym {:shape :binding-body
                              :bindings (mapv parse (:bindings contract))
                              :ret (parse (:ret contract))})
              :else
              (throw (ex-info (str "expected {:shape :binding-body :bindings [types...] :ret type}"
                                   " or {:shape :malli-defn :private? boolean} (private? is optional)") {})))
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

(defn- binding-name [node]
  (let [sym (:val node)]
    (when (and (= :sym (:op node)) (simple-symbol? sym)
               (not (contains? '#{& &form &env} sym)) (empty? (:meta node)))
      sym)))

(defn- parameter-locals [node]
  (if-let [sym (binding-name node)]
    [sym]
    (when (and (= :map (:op node)) (even? (count (:children node))) (empty? (:meta node)))
      (let [pairs (partition 2 (:children node))
            aliases (filter #(= :as (:val (first %))) pairs)
            fields (remove #(= :as (:val (first %))) pairs)
            alias (binding-name (second (first aliases)))]
        (when (and (= 1 (count aliases)) alias
                   (every? (fn [[local key]] (and (binding-name local) (= :kw (:op key)))) fields))
          (into [alias] (map (comp binding-name first)) fields))))))

(defn- call-head [form arity]
  (let [head (first (:children form))]
    (when (and (= :list (:op form)) (= :sym (:op head))
               (= (inc arity) (count (:children form))))
      (:val head))))

(defn- core-call? [ctx name arity form]
  (= name (t/core-name ctx (call-head form arity))))

(defn- spec-call? [ctx file name arity form]
  (when-let [head (call-head form arity)]
    (and (not (contains? (:defined-symbols ctx) head))
         (= name (env/resolve-sym file head)))))

(defn- unquoted-node [node]
  (when (and (= :unquote (:op node)) (= 1 (count (:children node))))
    (first (:children node))))

(defn- spec-key [node]
  (let [key (if (= :kw (:op node)) node (unquoted-node node))]
    (when (and (= :kw (:op key)) (qualified-keyword? (:val key)))
      (:val key))))

(defn- invalid-test? [ctx file test result]
  (let [[_ a b] (:children test)]
    (or (and (core-call? ctx '= 2 test)
             (= :kw (:op a)) (= :clojure.spec.alpha/invalid (:val a))
             (= :sym (:op b)) (= result (:val b)))
        (and (spec-call? ctx file 'clojure.spec.alpha/invalid? 1 test)
             (= :sym (:op a)) (= result (:val a))))))

(defn- rejection? [ctx file branch spec parameter]
  (let [[_ exception] (:children branch)
        [_ message explanation] (:children exception)
        [_ key arg] (:children explanation)]
    (and (core-call? ctx 'throw 1 branch)
         (core-call? ctx 'ex-info 2 exception)
         (= :str (:op message))
         (spec-call? ctx file 'clojure.spec.alpha/explain-data 2 explanation)
         (= spec (spec-key key))
         (= parameter (binding-name (unquoted-node arg))))))

(defn- direct-forwarding? [env file form parameters]
  (when-let [head (call-head form (count parameters))]
    (let [resolved (env/resolve-sym file head)
          target (if (qualified-symbol? resolved) resolved (symbol (str (:ns file)) (name resolved)))]
      (and (get-in env [:vars target :parts])
           (= 1 (get-in env [:root-definitions target]))
           (not (contains? (:macros env) target))
           (= parameters (mapv (comp binding-name unquoted-node) (rest (:children form))))))))

(defn- spec-template-contract [env file form]
  (let [{[{params :params [quoted & body] :body} & more] :arities} (env/defn-parts form)
        ctx (assoc (env/type-ctx file nil) :defined-symbols (get-in env [:ns-defs (:ns file)]))
        locals (mapv parameter-locals (:children params))
        parameters (mapv first locals)
        names (mapcat identity locals)
        template (:syntax-template quoted)
        [_ bindings conditional] (:children template)
        [binding conform] (:children bindings)
        result (binding-name binding)
        [_ spec-node argument] (:children conform)
        spec (spec-key spec-node)
        parameter (binding-name (unquoted-node argument))
        index (get (zipmap parameters (range)) parameter)
        [_ test failure success] (:children conditional)]
    (when (and (= 'defmacro (t/core-name ctx (get-in form [:children 0 :val])))
               (empty? more) (empty? body) (empty? (:meta params))
               (seq locals) (every? some? locals) (apply distinct? names)
               (core-call? ctx 'let 2 template)
               (= :vector (:op bindings)) (= 2 (count (:children bindings)))
               result (str/ends-with? (name result) "#")
               (spec-call? ctx file 'clojure.spec.alpha/conform 2 conform)
               (some? index)
               spec (get-in env [:specs spec])
               (not= t/unknown (t/resolve-t env (t/ref-t spec)))
               (core-call? ctx 'if 3 conditional)
               (invalid-test? ctx file test result)
               (rejection? ctx file failure spec parameter)
               (direct-forwarding? env file success parameters))
      {:spec spec :index index :arity (count parameters)})))

(defn infer-spec-contracts
  "Read a fixed syntax-quoted Spec guard and direct function call as an argument check."
  [env files]
  (when (seq (:macros env))
    (let [definitions (for [file files form (:forms file)
                            :when (= 'defmacro (env/head file form))
                            :let [nm (:val (second (:children form)))]
                            :when (simple-symbol? nm)]
                        [(symbol (str (:ns file)) (name nm)) file form])]
      (into {}
            (for [[sym [[_ file form] & more]] (group-by first definitions)
                  :when (and (empty? more) (not (get-in env [:root-definitions sym])))
                  :let [contract (spec-template-contract env file form)]
                  :when contract]
              [sym contract])))))
