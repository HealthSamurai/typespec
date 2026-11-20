(ns typespec.check
  "The checker: walks forms, infers types, reports diagnostics."
  (:require [clojure.string :as str]
            [clojure.walk :as walk]
            [typespec.parse :as parse]
            [typespec.types :as t]
            [typespec.solver :as solver]
            [typespec.env :as env]
            [typespec.java :as java]
            [typespec.core-types :as core]
            [typespec.macro-contracts :as macros]))

(def ^:dynamic *diags* nil)
(def ^:dynamic *types* nil)
(def ^:dynamic *ignores* nil)
(def ^:dynamic *trust* nil)
(def ^:dynamic *rets* nil)

(declare infer infer-body bind-pattern apply-fn-node why-of var-type infer-fn narrow)

(defn- report! [ctx node code msg & {:as extra}]
  (when-not (:quiet ctx)
    (swap! *diags* conj (merge {:code code :severity :error :file (:file (:file ctx)) :pos (:pos node) :message msg} extra)))
  nil)

(defn- eff
  "With :strict-nil, a platform type (from a hint or from Java) may be nil."
  [ctx ty]
  (if (and (:strict-nil (:opts ctx)) (:platform ty)) (t/union (dissoc ty :platform) t/nil-t) ty))

(defn- show-ref [ctx ty]
  (let [s (t/show ty)]
    (if (= :ref (:t ty))
      (let [r (t/resolve-t (:env ctx) ty)]
        (if (#{:map :merge :unknown} (:t r)) s (str s " (" (t/show r) ")")))
      s)))

;; ---- did you mean ----

(defn- edit-distance [^String a ^String b]
  (let [prev (int-array (range (inc (count b))))]
    (dotimes [i (count a)]
      (let [cur (int-array (inc (count b)))]
        (aset cur 0 (inc i))
        (dotimes [j (count b)]
          (aset cur (inc j) (int (min (inc (aget cur j)) (inc (aget prev (inc j)))
                                      (+ (aget prev j) (if (= (.charAt a i) (.charAt b j)) 0 1))))))
        (System/arraycopy cur 0 prev 0 (alength cur))))
    (aget prev (count b))))

(defn- similar [x candidates]
  (let [norm #(str/replace (str/lower-case (str %)) #"[-_]" "")
        sx (str x)]
    (->> candidates
         (remove #(= (str %) sx))
         (map (fn [c] [(if (= (norm c) (norm sx)) 0 (edit-distance sx (str c))) c]))
         (filter (fn [[d c]] (<= d (max 1 (min 2 (quot (count (name c)) 2))))))
         (sort-by first) first second)))

(defn- hint-suffix [x candidates]
  (if-let [c (similar x candidates)] (str ". Did you mean " c "?") ""))

;; ---- maps and keys ----

(defn- map-members [ctx ty]
  (filter #(= :map (:t %)) (t/members (:env ctx) ty)))

(defn- lookup-key [ctx ty k]
  (let [env (:env ctx)
        ms (remove #(= :nil (:t %)) (t/members env ty))
        spec (when (and (qualified-keyword? k) (get-in env [:specs k])) (t/ref-t k))
        loose [:unknown (or spec t/unknown)]
        one (fn [m]
              (case (:t m)
                :map (cond (contains? (:req m) k) [:req (get (:req m) k)]
                           (contains? (:opt m) k) [:opt (get (:opt m) k)]
                           spec [:opt spec]
                           ;; a map whose type was inferred (a literal, a pattern) may hold more keys than we saw
                           (:inferred m) loose
                           :else [:missing nil])
                :map-of [:opt (:v m)]
                (:unknown :any :class) loose
                :prim (if (= :map (:name m)) loose [:not-map nil])
                [:not-map nil]))
        rs (map one ms)
        ty-of #(apply t/union (keep second rs))]
    (cond (empty? ms) {:status :nil :type t/nil-t}
          (some #(= :not-map (first %)) rs) {:status :not-map}
          (some #(= :missing (first %)) rs) {:status :missing}
          (every? #(= :req (first %)) rs) {:status :req :type (ty-of)}
          (every? #(= :unknown (first %)) rs) {:status :unknown :type (ty-of)}
          :else {:status :opt :type (ty-of)})))

(defn- key-info
  "{:type :optional? :declared?} for reading key k from a value of type ty; reports unknown keys at node."
  [ctx ty k node]
  (let [env (:env ctx)
        {:keys [status type]} (lookup-key ctx ty k)
        spec-keys (keys (:specs env))]
    (case status
      :req {:type type :optional? (t/nilable? env ty) :declared? true :status :req}
      :opt {:type type :optional? true :declared? true}
      :nil {:type t/nil-t :optional? true :declared? true}
      :unknown (do (when (and (qualified-keyword? k) (not (get-in env [:specs k]))
                              (contains? (:key-nss env) (namespace k))
                              (or (:strict (:opts ctx)) (similar k spec-keys)))
                     (report! ctx node "TSP2304" (str "no spec for " k (hint-suffix k spec-keys))))
                   ;; nothing is known about the map: nil is not checked, as for other platform types
                   {:type (if (#{:unknown :any} (:t type)) type (assoc type :platform true))
                    :optional? false :declared? false})
      :missing (do (report! ctx node "TSP2339"
                            (str k " is not a key of " (t/show (t/remove-nil env ty))
                                 (hint-suffix k (concat (mapcat #(concat (keys (:req %)) (keys (:opt %))) (map-members ctx ty))
                                                        (when (qualified-keyword? k) spec-keys)))))
                   {:type t/unknown :optional? false :declared? false})
      :not-map (do (report! ctx node "TSP2339" (str "cannot read " k " from " (t/show ty)))
                   {:type t/unknown :optional? false :declared? false}))))

(defn- read-key [ctx ty k node]
  (let [{:keys [type optional?]} (key-info ctx ty k node)]
    (if optional? (t/union type t/nil-t) type)))

(defn- read-key-default [ctx ty k node default-type]
  (let [{:keys [type optional?]} (key-info ctx ty k node)]
    ;; A default replaces absence, never a nil value stored at the key.
    (if optional?
      (if (every? #(= :nil (:t %)) (t/members (:env ctx) ty))
        default-type
        (t/union type default-type))
      type)))

;; ---- assignability ----

(defn- incomplete-problem [ctx actual expected result]
  ["TSP210-01"
   (str "compatibility not checked: " (show-ref ctx actual) " to " (show-ref ctx expected)
        " (" (str/join ", " (sort (map name (:reasons result)))) ")")
   {:severity (if (:strict (:opts ctx)) :error :warning)
    :reasons (:reasons result)}])

(defn- fresh-problems [ctx actual expected]
  (let [env (:env ctx)
        ms (remove #(= :nil (:t %)) (t/members env expected))]
    (when (and (:fresh actual) (= 1 (count ms)) (= :map (:t (first ms))))
      (let [m (first ms)
            known (concat (keys (:req m)) (keys (:opt m)))]
        (concat
         (for [k (keys (:req m)) :when (not (contains? (:req actual) k))]
           ["TSP2741" (str "required key " k " of " (t/show expected) " is missing")])
         (for [[k v] (:req actual)
               :let [exp (or (get (:req m) k) (get (:opt m) k))
                     result (when exp (t/sub-result env v exp))]
               :when (and exp (not= :proved (:status result)))]
           (if (= :inconclusive (:status result))
             (incomplete-problem ctx v exp result)
             ["TSP2322" (str "value of " k ": " (t/show v) " is not assignable to " (show-ref ctx exp))]))
         (for [k (keys (:req actual))
               :when (and (not (some #{k} known))
                          (not (and (qualified-keyword? k) (get-in env [:specs k])))
                          ;; a qualified key with no spec is already reported as TSP2304
                          (not (and (qualified-keyword? k) (contains? (:key-nss env) (namespace k))))
                          (or (qualified-keyword? k) (similar k known)))]
           ["TSP2353" (str "unknown key " k " in a literal of " (t/show expected) (hint-suffix k known))]))))))

(defn- missing-req
  "[key optional-there?] when `actual` is a map type that lacks a required key of `expected`."
  [ctx actual expected]
  (let [env (:env ctx)
        am (remove #(= :nil (:t %)) (t/members env actual))
        em (remove #(= :nil (:t %)) (t/members env expected))]
    (when (and (= 1 (count am) (count em)) (= :map (:t (first am)) (:t (first em))) (not (t/nilable? env actual)))
      (when-let [k (first (remove #(contains? (:req (first am)) %) (keys (:req (first em)))))]
        [k (contains? (:opt (first am)) k)]))))

(defn- problems
  "Why a value of type `actual` (written as `node`) cannot be used as `expected`. Empty when it can."
  [ctx node actual expected]
  (let [env (:env ctx) actual (eff ctx actual)
        result (t/sub-result env actual expected)]
    (cond
      (:generic-failure actual) []
      (= :proved (:status result)) (if (:fresh actual) (filter #(= "TSP2353" (first %)) (fresh-problems ctx actual expected)) [])
      (seq (fresh-problems ctx actual expected)) (fresh-problems ctx actual expected)
      (= :inconclusive (:status result)) [(incomplete-problem ctx actual expected result)]
      (and (not (#{:nil :map} (:op node))) (t/nilable? env actual) (not (t/nilable? env expected))
           (t/sub? env (t/remove-nil env actual) expected))
      [["TSP2531" (str (or (:display node) (parse/unparse node)) " may be nil"
                       (when-let [w (if (= :sym (:op node))
                                      (get-in ctx [:why (:val node)])
                                      (why-of ctx node actual))]
                         (str ": " w)))]]
      :else
      (if-let [[k opt?] (missing-req ctx actual expected)]
        [["TSP2741" (str "required key " k " is missing in " (t/show actual) (when opt? " (it is optional there)"))]]
        [["TSP2345" (str (show-ref ctx actual) " is not assignable to " (show-ref ctx expected))]]))))

(defn- assignable! [ctx node actual expected]
  (doseq [[code msg extra] (problems ctx node actual expected)]
    (apply report! ctx node code msg (mapcat identity extra))))

;; ---- locals and narrowing ----

(defn- bind [ctx sym ty & {:keys [why hinted param]}]
  (-> ctx
      (assoc-in [:locals sym] ty)
      (update :why #(if why (assoc % sym why) (dissoc % sym)))
      (update :hinted #(if hinted (conj (or % #{}) sym) (disj (or % #{}) sym)))
      (update :params #(if param (conj (or % #{}) sym) (disj (or % #{}) sym)))
      (assoc-in [:binding-ids sym] (Object.))
      (update :projections dissoc sym)
      (update :conditions dissoc sym)
      (update :missing-of dissoc sym)))

(defn- plain-local [ctx node]
  (when (and (= :sym (:op node)) (empty? (:directives node))
             (contains? (:locals ctx) (:val node)))
    (:val node)))

(defn- builtin-head [ctx node]
  (let [head (env/head (:file ctx) node)
        raw (:val (first (:children node)))
        ns-sym (:ns (:file ctx))
        core-sym (when (and head (not (namespace head))) (symbol "clojure.core" (name head)))]
    (when (and head (empty? (:directives node)) (not (env/tag-node node))
               (not (contains? (:locals ctx) raw))
               (not (and core-sym (contains? (get-in ctx [:env :ns-defs ns-sym]) head)))
               (not= :extern (get-in ctx [:env :vars core-sym :source])))
      head)))

(defn- remember-projection [ctx sym origin path]
  (assoc-in ctx [:projections sym]
            {:origin origin :path path :type (get-in ctx [:locals sym])}))

(defn- stable-path [ctx node]
  (when (and (empty? (:directives node)) (not (env/tag-node node)))
    (if-let [sym (plain-local ctx node)]
      (let [ty (get-in ctx [:locals sym])
            {:keys [origin path type]} (get-in ctx [:projections sym])]
        (if (and origin (= type ty))
          {:origin origin :path path :type (get-in ctx [:origins origin])}
          {:local sym :path [] :type ty}))
      (when (= :list (:op node))
        (let [[head a b & more] (:children node)
              h (builtin-head ctx node)
              [base path] (cond
                            (and (= :kw (:op head)) a (nil? b)) [a [(:val head)]]
                            (and (= 'get h) (= :kw (:op b)) (empty? more)) [a [(:val b)]]
                            (and (= 'get-in h) (= :vector (:op b)) (seq (:children b))
                                 (every? #(= :kw (:op %)) (:children b)) (empty? more))
                            [a (mapv :val (:children b))])]
          (when-let [p (and base (stable-path ctx base))]
            (update p :path into path)))))))

(defn- track-path [ctx {:keys [local origin path type]}]
  (if origin
    [ctx origin path]
    (let [origin (Object.)
          ctx (assoc-in ctx [:origins origin] type)]
      [(cond-> ctx local (remember-projection local origin [])) origin path])))

(defn- projected-type [ctx ty path]
  (reduce (fn [current key]
            (let [{:keys [type status]} (lookup-key ctx current key)]
              (case status
                :req (if (t/nilable? (:env ctx) current) (t/union type t/nil-t) type)
                :opt (t/union type t/nil-t)
                :nil t/nil-t
                (reduced nil))))
          ty path))

(defn- refine-origin [ctx {:keys [local origin]} ty]
  (when-not (= :never (:t ty))
    (if origin
      (reduce-kv (fn [c sym {:keys [path type] :as p}]
                   (if (and (= origin (:origin p)) (= type (get-in ctx [:locals sym])))
                     (if-let [narrowed (projected-type c ty path)]
                       (-> c (assoc-in [:locals sym] narrowed) (remember-projection sym origin path))
                       c)
                     c))
                 (assoc-in ctx [:origins origin] ty) (:projections ctx))
      (assoc-in ctx [:locals local] ty))))

(defn- path-alternatives
  "Pairs of [leaf type, rebuild]. Each rebuild retains its enclosing map alternatives."
  ([ctx ty path] (path-alternatives ctx ty path false))
  ([ctx ty path presence?]
   (let [budget (volatile! 256)
         env (:env ctx)]
     (letfn [(walk [ty path]
               (let [members (vec (take (inc @budget) (t/members env (eff ctx ty))))]
                 (when (and (seq members) (not (neg? (vswap! budget - (count members)))))
                   (if-let [[key & more] (seq path)]
                     (let [required (fn [m field]
                                      (when-let [children (walk field more)]
                                        (for [[leaf rebuild] children]
                                          [leaf #(assoc-in m [:req key] (rebuild %))])))
                           paths (for [m members]
                                   (cond
                                     (get-in m [:req key]) (required m (get-in m [:req key]))
                                     (and presence? (get-in m [:opt key]))
                                     (let [field (get-in m [:opt key])
                                           absent [t/nil-t (constantly (assoc-in m [:opt key] t/never))]
                                           present (required (update m :opt dissoc key) field)]
                                       (when present (cons absent present)))
                                     (and presence? (= :nil (:t m))) [[t/nil-t (constantly m)]]))]
                       (when (every? some? paths)
                         (mapcat identity paths)))
                     (map (fn [member] [member identity]) members)))))]
       (walk ty path)))))

(def ^:private primitive-predicates
  {'string? [:string string?] 'int? [:int int?] 'number? [:number number?]
   'keyword? [:keyword keyword?] 'map? [:map map?] 'vector? [:vector vector?]
   'boolean? [:boolean boolean?]})

(defn- truth-part [env ty truthy?]
  (if (and truthy?
           (not-any? #(or (= % (t/lit false)) (= % (t/prim :boolean))
                          (#{:unknown :any} (:t %))) (t/members env ty)))
    (t/remove-nil env ty)
    (apply t/union
         (for [m (t/members env ty)
               :let [falsy? (or (= :nil (:t m)) (= m (t/lit false)))
                     boolean? (= m (t/prim :boolean))]
               :when (or (#{:unknown :any} (:t m)) boolean? (= truthy? (not falsy?)))]
           (if boolean? (t/lit truthy?) m)))))

(defn- nil-part [env ty]
  (if (some #(#{:nil :unknown :any} (:t %)) (t/members env ty)) t/nil-t t/never))

(defn- refine-alternatives [ctx path alternatives select]
  (refine-origin ctx path
                 (apply t/union (keep (fn [[leaf rebuild]]
                                        (when-let [narrowed (select leaf)]
                                          (rebuild narrowed)))
                                      alternatives))))

(defn- literal-node? [node]
  (and (#{:str :kw :num :bool :nil} (:op node)) (empty? (:directives node))
       (not (env/tag-node node))))

(defn- literal-type [v]
  (if (nil? v) t/nil-t (t/lit v)))

(defn- finite-value? [ty]
  (#{:lit :nil} (:t ty)))

(defn- boolean-type? [ty]
  (= (t/prim :boolean) (t/strip ty)))

(defn- expand-boolean [ty]
  (if (boolean-type? ty) [(t/lit false) (t/lit true)] [ty]))

(defn- refine-path [ctx node split]
  (let [subject (stable-path ctx node)
        alternatives (when subject (path-alternatives ctx (:type subject) (:path subject) true))
        local (when-not alternatives (plain-local ctx node))
        subject (if local {:local local} subject)
        alternatives (or alternatives (when local (path-alternatives ctx (get-in ctx [:locals local]) [])))]
    (when alternatives
      (let [parts (map (fn [[leaf rebuild]] [(split leaf) rebuild]) alternatives)
            branch (fn [i]
                     (refine-alternatives ctx subject parts #(nth % i)))]
        [(branch 0) (branch 1)]))))

(defn- equality-parts [ctx value leaf]
  (let [literal (literal-type value)]
    (cond
      (finite-value? leaf) (if (= value (:val leaf)) [leaf nil] [nil leaf])
      (= :never (:t leaf)) [nil nil]
      (boolean-type? leaf)
      (if (boolean? value) [literal (t/lit (not value))] [nil leaf])
      (and (= :prim (:t leaf)) (not (t/sub? (:env ctx) literal leaf))) [nil leaf]
      :else [literal leaf])))

(defn- equality-contexts [ctx expr value]
  (refine-path ctx expr #(equality-parts ctx value %)))

(defn- presence-parts [key leaf]
  (case (:t leaf)
    :nil [nil leaf]
    :never [nil nil]
    :map (cond
           (contains? (:req leaf) key) [leaf nil]
           (contains? (:opt leaf) key)
           (let [field (get-in leaf [:opt key])
                 absent (assoc-in leaf [:opt key] t/never)]
             [(when-not (= :never (:t field))
                (-> leaf (assoc-in [:req key] field) (update :opt dissoc key)))
              absent])
           :else [leaf leaf])
    [leaf leaf]))

(def ^:private scalar-primitives #{:int :double :number :string :keyword :boolean :symbol})

(defn- predicate-parts [ctx head target leaf]
  (let [tag (:t leaf)
        env (:env ctx)
        pred (second (primitive-predicates head))]
    (cond
      (= :never tag) [nil nil]
      (#{:unknown :any} tag) [target leaf]
      ;; Specs compare numeric literals by value; int? and instance? can
      ;; distinguish representations of the same number.
      (and (= 'int? head)
           (or (and (= :lit tag) (integer? (:val leaf))) (= :int (:name leaf))))
      [leaf leaf]
      (and (finite-value? leaf) pred)
      (if (pred (:val leaf)) [leaf nil] [nil leaf])
      (= :nil tag) [nil leaf]
      (t/sub? env leaf target) [leaf nil]
      (t/sub? env target leaf) [target leaf]
      (and (= :prim tag) (= :prim (:t target))
           (or (scalar-primitives (:name leaf)) (scalar-primitives (:name target))))
      [nil leaf]
      :else [target leaf])))

(defn- instance-parts [ctx class-name leaf]
  (if (= "java.lang.Object" class-name)
    [(when-not (#{:nil :never} (:t leaf)) leaf)
     (when (#{:nil :unknown :any} (:t leaf)) t/nil-t)]
    (let [target (t/class-type class-name)
          [positive negative] (predicate-parts ctx 'instance? target leaf)]
      (if (#{:int :double} (:name target))
        [positive (when-not (= :never (:t leaf)) leaf)]
        [positive negative]))))

(defn- join-contexts [base paths]
  (let [paths (vec (remove nil? paths))]
    (case (count paths)
      0 nil
      1 (first paths)
      (let [locals (apply merge-with t/union (map :locals paths))]
        (reduce-kv (fn [c sym {:keys [origin path]}]
                     (if (every? #(= (get-in % [:locals sym]) (get-in % [:projections sym :type])) paths)
                       (remember-projection c sym origin path)
                       c))
                   (assoc base :locals locals :origins (apply merge-with t/union (map :origins paths)))
                   (:projections base))))))

(defn- logical-contexts [ctx args and?]
  (let [proceed (if and? first second)
        stop (if and? second first)
        [last-path stopped] (reduce (fn [[active stopped] arg]
                                      (if active
                                        (let [branches (narrow active arg)]
                                          [(proceed branches) (conj stopped (stop branches))])
                                        (reduced [nil stopped])))
                                    [ctx []] args)
        stopped (join-contexts ctx stopped)]
    (if and? [last-path stopped] [stopped last-path])))

(defn- condition-capture [ctx node]
  (let [head (builtin-head ctx node)]
    (when (or (primitive-predicates head)
              (#{'some? 'nil? 'not 'and 'or '= 'not= 'contains? 'instance? 'clojure.spec.alpha/valid?} head))
      {:test node
       :bindings (into {} (for [n (tree-seq (comp seq :children) :children node) :when (= :sym (:op n))]
                            [(:val n) (get-in ctx [:binding-ids (:val n)])]))})))

(defn- truth-contexts [ctx node]
  (refine-path ctx node (fn [leaf]
                         (for [truthy? [true false]
                               :let [ty (truth-part (:env ctx) leaf truthy?)]]
                           (when-not (= :never (:t ty)) ty)))))

(defn- narrow
  "Truth and falsity contexts. A nil context denotes an unreachable path."
  [ctx test]
  (let [env (:env ctx)
        h (builtin-head ctx test)
        [a b :as args] (vec (rest (:children test)))
        local (plain-local ctx test)
        {captured-test :test bindings :bindings} (get-in ctx [:conditions local])]
    (cond
      (and captured-test (every? (fn [[sym id]] (= id (get-in ctx [:binding-ids sym]))) bindings))
      (narrow ctx captured-test)

      (literal-node? test)
      (if (:val test) [ctx nil] [nil ctx])

      local
      (let [ty (eff ctx (get-in ctx [:locals local]))
            positive (truth-part env ty true)
            negative (truth-part env ty false)
            update-type (fn [ty] (when-not (= :never (:t ty)) (assoc-in ctx [:locals local] ty)))
            fc (update-type negative)]
        (or (when (seq (get-in ctx [:projections local :path])) (truth-contexts ctx test))
            [(update-type positive)
             (if (= t/nil-t negative)
               (reduce (fn [c s] (update-in c [:locals s] #(t/remove-nil env %))) fc (get-in ctx [:missing-of local]))
               fc)]))

      (#{'some? 'nil?} h)
      (or (when (= 1 (count args))
            (refine-path ctx a (fn [leaf]
                                 (let [nil? (= :nil (:t leaf))
                                       vague? (#{:unknown :any} (:t leaf))
                                       present (when-not (or nil? (= :never (:t leaf))) leaf)
                                       absent (when (or nil? vague?) t/nil-t)]
                                   (if (= 'some? h) [present absent] [absent present])))))
          [ctx ctx])

      (and (= 'not h) (= 1 (count args))) (vec (reverse (narrow ctx a)))
      (= 'and h) (logical-contexts ctx args true)
      (= 'or h) (logical-contexts ctx args false)

      (and (#{'= 'not=} h) (= 2 (count args)))
      (let [branches (or (cond (and (literal-node? a) (literal-node? b))
                               (if (= (:val a) (:val b)) [ctx nil] [nil ctx])
                               (literal-node? b) (equality-contexts ctx a (:val b))
                               (literal-node? a) (equality-contexts ctx b (:val a)))
                         [ctx ctx])]
        (if (= '= h) branches (vec (reverse branches))))

      (and (= 'contains? h) (= 2 (count args)) (= :kw (:op b)) (literal-node? b))
      (or (refine-path ctx a #(presence-parts (:val b) %)) [ctx ctx])

      (and (= 'instance? h) (= 2 (count args)) (= :sym (:op a)))
      (or (when-let [c (t/resolve-class (:imports (:file ctx)) (:val a))]
            (refine-path ctx b #(instance-parts ctx c %)))
          [ctx ctx])

      (and (= 'clojure.spec.alpha/valid? h) (= 2 (count args)) (= :kw (:op a)) (plain-local ctx b))
      [(assoc-in ctx [:locals (plain-local ctx b)] (t/ref-t (:val a))) ctx]

      (and (primitive-predicates h) (= 1 (count args)))
      (let [target (t/prim (first (primitive-predicates h)))]
        (or (refine-path ctx a #(predicate-parts ctx h target %)) [ctx ctx]))

      :else (or (truth-contexts ctx test) [ctx ctx]))))

(defn- condition-contexts [ctx test]
  (let [ty (eff ctx (infer ctx test))
        [tc fc] (narrow ctx test)]
    [(when-not (= :never (:t (truth-part (:env ctx) ty true))) tc)
     (when-not (= :never (:t (truth-part (:env ctx) ty false))) fc)]))

;; ---- destructuring ----

(defn- keys-directive [k]
  (when (and (= :kw (:op k)) (#{"keys" "keys!"} (name (:val k))))
    {:ns (namespace (:val k)) :bang? (= "keys!" (name (:val k)))}))

(defn- has-bang? [pattern]
  (and (= :map (:op pattern))
       (some (fn [[k _]] (or (:bang? (keys-directive k)) (has-bang? k))) (partition 2 (:children pattern)))))

(defn- destructure-map [ctx pattern ty outer-why projection]
  (let [env (:env ctx)
        pairs (partition 2 (:children pattern))
        special (into {} (for [[k v] pairs :when (and (= :kw (:op k)) (nil? (namespace (:val k))))] [(:val k) v]))
        defaults (into {} (for [[k v] (partition 2 (:children (:or special)))] [(:val k) v]))
        missing-sym (some-> (:missing special) :val)
        tname (t/show (t/remove-nil env ty))
        ;; Destructuring evaluates its input once. Its whole value and plain
        ;; field bindings share this identity, even without :as.
        [ctx origin path] (track-path ctx (or projection {:path [] :type ty}))
        ctx (cond-> ctx missing-sym (bind missing-sym (t/union (t/prim :map) t/nil-t)))
        ctx (reduce
             (fn [ctx [k v]]
               (let [kd (keys-directive k)]
                 (cond
                   (and kd (= :vector (:op v)))
                   (reduce
                    (fn [ctx s]
                      (if-not (#{:sym :kw} (:op s))
                        ctx
                        (let [key (keyword (or (:ns kd) (namespace (:val s))) (name (:val s)))
                              sym (symbol (name (:val s)))
                              {:keys [type optional? declared? status]} (key-info ctx ty key s)
                              default (get defaults sym)]
                          (cond
                            (and (:bang? kd) missing-sym)
                            (-> (bind ctx sym (t/union type t/nil-t) :why (str key " is missing when " missing-sym " is not nil"))
                                (update-in [:missing-of missing-sym] (fnil conj []) sym))
                            (:bang? kd)
                            (do (when (and optional? declared?)
                                  (report! ctx s "TSP2741" (str ":keys! requires " key ", but it is optional in " tname)))
                                (bind ctx sym type))
                            default
                            (bind ctx sym (if optional? (t/union type (infer ctx default)) type)
                                  :why (when (t/nilable? env type)
                                         (str key " is " (t/show (t/resolve-t env type))
                                              ", and :or applies only when the key is absent")))
                            optional?
                            (-> (bind ctx sym (t/union type t/nil-t)
                                      :why (cond (= :req status) (or outer-why "the destructured value may be nil")
                                                 declared? (str key " is optional in " tname)
                                                 :else (str key " may be absent")))
                                (remember-projection sym origin (conj path key)))
                            :else (-> (bind ctx sym type)
                                      (remember-projection sym origin (conj path key)))))))
                    ctx (:children v))

                   (and (= :kw (:op k)) (= :as (:val k)))
                   (-> (bind ctx (:val v) ty)
                       (remember-projection (:val v) origin path))
                   (and (= :kw (:op k)) (= :missing (:val k))) ctx
                   (and (= :kw (:op k)) (#{:select :excess :all} (:val k))) (bind ctx (:val v) t/unknown)
                   (and (= :kw (:op k)) (#{:strs :syms :strs! :syms!} (:val k)))
                   (reduce #(bind %1 (:val %2) t/unknown) ctx (filter #(= :sym (:op %)) (:children v)))

                   (= :kw (:op v))
                   (let [{:keys [type optional? declared?]} (key-info ctx ty (:val v) v)
                         bang? (has-bang? k)]
                     (when (and bang? declared? (or optional? (t/nilable? env type)))
                       (report! ctx v "TSP2741" (str ":keys! below requires " (:val v) ", but it is optional in " tname)))
                     (bind-pattern ctx k (cond bang? (t/remove-nil env type)
                                               optional? (t/union type t/nil-t)
                                               :else type)
                                   (when (and optional? declared?) (str (:val v) " is optional in " tname))
                                   (when (and (not bang?) (not (contains? defaults (:val k))))
                                     {:origin origin :path (conj path (:val v))})))
                   :else ctx)))
             ctx pairs)]
    ctx))

(defn- elem-type [ctx ty]
  (apply t/union
         (for [m (t/members (:env ctx) ty)]
           (case (:t m) :coll (:of m) :nil t/never :map-of (t/prim :vector) t/unknown))))

(defn- indexed
  "Element access is not checked for emptiness (as in TypeScript by default); :strict-nil makes it T | nil."
  [el]
  (if (#{:unknown :any :never :nil} (:t el)) el (assoc el :platform true)))

(defn- check-hint!
  "A hint that cannot be true for the value it is put on."
  [ctx node actual what]
  (when-let [h (env/hint-type (:file ctx) node)]
    (let [env (:env ctx) a (t/remove-nil env (eff ctx actual))]
      (when (and (not (#{:never :unknown :any} (:t (t/resolve-t env a))))
                 (not (t/sub? env a h)) (not (t/sub? env h a)))
        (report! ctx (env/tag-node node) "TSP2322"
                 (str "hint " (t/show h) " contradicts the " what ": " (t/show (dissoc actual :platform))))))
    h))

(defn bind-pattern
  ([ctx pattern ty] (bind-pattern ctx pattern ty nil))
  ([ctx pattern ty why] (bind-pattern ctx pattern ty why nil))
  ([ctx pattern ty why projection]
   (case (:op pattern)
     :sym (let [h (check-hint! ctx pattern ty "type")
                env (:env ctx)
                ;; the compiler believes the hint; keep the inferred type only when it agrees and says more
                use-hint? (and h (or (#{:unknown :any} (:t (t/resolve-t env ty)))
                                     (not (t/sub? env (t/remove-nil env (eff ctx ty)) h))))
                [c origin path] (if (and projection (not use-hint?)) (track-path ctx projection) [ctx])]
            (cond-> (bind c (:val pattern) (if use-hint? h ty) :why (when-not use-hint? why) :hinted (boolean h))
              origin (remember-projection (:val pattern) origin path)))
     :map (destructure-map ctx pattern ty why projection)
     :vector (let [el (elem-type ctx ty)]
               (loop [ctx ctx [p & more] (:children pattern)]
                 (cond (nil? p) ctx
                       (and (= :sym (:op p)) (= '& (:val p))) (bind-pattern ctx (first more) {:t :coll :of el})
                       (and (= :kw (:op p)) (= :as (:val p))) (bind-pattern ctx (first more) ty)
                       :else (recur (bind-pattern ctx p (if (#{:unknown :any :never :nil} (:t el)) el (assoc el :platform true))) more))))
     ctx)))

;; ---- function calls ----

(defn- arity-fits? [a n]
  (let [k (count (:params a))]
    (or (= n k) (and (> n k) (or (:rest a) (:kwargs a))))))

(defn- arg-problems [ctx a arg-nodes arg-types call-node]
  (let [k (count (:params a))
        fixed (mapcat (fn [n at pt] (map #(vector n %) (problems ctx n at pt))) arg-nodes arg-types (:params a))
        extra (drop k (map vector arg-nodes arg-types))]
    (concat
     fixed
     (when (:rest a) (mapcat (fn [[n at]] (map #(vector n %) (problems ctx n at (:rest a)))) extra))
     (when-let [kw (:kwargs a)]
       (let [given (into {} (for [[[kn _] [vn vt]] (partition 2 extra) :when (= :kw (:op kn))] [(:val kn) [vn vt]]))
             m (first (map-members ctx kw))]
         (concat
          (for [rk (keys (:req m)) :when (not (contains? given (keyword (name rk))))]
            [call-node ["TSP2741" (str "required key " (keyword (name rk)) " is missing")]])
          (for [[gk [vn vt]] given
                :let [exp (some (fn [[dk dv]] (when (= (name dk) (name gk)) dv)) (concat (:req m) (:opt m)))]
                :when exp
                p (problems ctx vn vt exp)]
            [vn p])))))))

(defn- unresolved-type?
  "Contains unknown/any or an unsolved variable. A rigid type parameter is a
  known type: its (possibly unknown) bound is not part of the value's type."
  [ty]
  (boolean (some #(and (map? %) (#{:ivar :var :unknown :any :all} (:t %)))
                 (tree-seq #(and (coll? %) (not (and (map? %) (= :rigid (:t %))))) seq ty))))

(defn- trusted-substitution
  "Candidates that may be used as types: not the ones the solver marks uncertain
  (an unknown lower bound or evidence it could not decompose)."
  [solved]
  (apply dissoc (:substitution solved) (:uncertain solved)))

(defn- generic-result [ctx node label state]
  (let [{:keys [status issues] :as solved} (solver/solve (:env ctx) state)
        issue (or (first (filter #(= :mismatch (:status %)) issues)) (first issues))
        ret (solver/substitute (:ret (first (:arities (:type state)))) (trusted-substitution solved))
        ;; Only the solver snapshot keeps unsolved ivars. Do not leak them, or an
        ;; incomplete candidate, into ordinary inference after the diagnostic.
        ret (solver/substitute ret (into {} (for [v (:vars state)] [(:id v) t/unknown])))]
    ;; A solve snapshot is pure. Only this final solve emits, once per call.
    (when issue
      (if (= :mismatch status)
        (report! ctx node "TSP2345"
                 (str label ": " (t/show (:left issue)) " is not assignable to " (t/show (:right issue))))
        (report! ctx node "TSP210-01"
                 (str label ": generic call not checked ("
                      (str/join ", " (sort (map name (into #{} (mapcat :reasons issues))))) ")")
                 :severity (if (:strict (:opts ctx)) :error :warning)
                 :reasons (into #{} (mapcat :reasons issues)))))
    ;; The flag silences follow-up checks of a result that means nothing: after
    ;; a mismatch, or when the result itself is not known. A fully known result
    ;; of a merely unchecked call is still checked where it is used.
    (cond-> (or ret t/unknown)
      (or (= :mismatch status) (and (not= :proved status) (unresolved-type? ret)))
      (assoc :generic-failure true))))

(defn- ivar-ids [ty]
  (into #{} (keep #(when (and (map? %) (= :ivar (:t %))) (:id %))) (tree-seq coll? seq ty)))

(defn- callback-substitution
  "The partial solution as callbacks may see it. A variable that some callback
  returns can still grow from that callback's result, so its seed is only a
  start: a literal seed 0 means int?, and an empty collection says nothing."
  [partial callback-types]
  (let [growing (into #{} (mapcat #(ivar-ids (:ret (first (:arities %)))) callback-types))
        widen-all (fn [ty] (walk/postwalk
                            (fn [x] (cond (and (map? x) (= :lit (:t x))) (t/lit-prim (:val x))
                                          (and (map? x) (= :union (:t x))) (apply t/union (:types x))
                                          :else x)) ty))
        empty-seed? (fn [ty] (some #(and (map? %) (= :never (:t %))) (tree-seq coll? seq ty)))]
    (reduce (fn [sub id]
              (cond (not (contains? sub id)) sub
                    (empty-seed? (get sub id)) (dissoc sub id)
                    :else (update sub id widen-all)))
            partial growing)))

(defn- callback-context
  "Parameter types a callback may assume, or nil when they are not fully known."
  [expected partial]
  (let [context (solver/substitute expected partial)
        ca (first (:arities context))]
    (when (and (= :fn (:t context)) (= 1 (count (:arities context)))
               (not (:rest ca)) (not (:kwargs ca))
               (not-any? unresolved-type? (:params ca)))
      (:params ca))))

(defn- fn-lit-arity
  "Number of positional parameters of #(...), or nil when it uses %&."
  [node]
  (let [syms (keep #(when (= :sym (:op %)) (name (:val %))) (tree-seq :children :children node))]
    (when-not (some #{"%&"} syms)
      (reduce max 0 (keep #(cond (= "%" %) 1
                                 (re-matches #"%[1-9][0-9]*" %) (parse-long (subs % 1)))
                          syms)))))

(defn- infer-fn-lit
  "Type of #(...) when the context gives its parameter types."
  [ctx node arg-types]
  (let [n (fn-lit-arity node)
        ptypes (mapv #(nth arg-types % t/unknown) (range n))
        ctx' (reduce (fn [c [i ty]] (cond-> (bind c (symbol (str "%" (inc i))) ty)
                                      (zero? i) (bind '% ty)))
                     (dissoc ctx :recur-types) (map-indexed vector ptypes))]
    {:t :fn :arities [{:params ptypes :rest nil
                       :ret (infer ctx' {:op :list :children (:children node) :pos (:pos node)})}]}))

(defn- call-generic
  "Rank-1, single fixed arity: infer ordinary arguments before contextual callbacks.
  Typed arguments are used by apply-type; node arguments are visited at most once."
  [ctx node label ftype arg-nodes typed-args]
  (let [state (solver/instantiate ftype)
        ars (:arities (:type state)) ar (first ars)
        n (count (or typed-args arg-nodes))
        inline? (fn [nd] (or (= :fn-lit (:op nd))
                             (and (= :list (:op nd))
                                  (#{'fn 'fn* 'clojure.core/fn} (:val (first (:children nd)))))))]
    (cond
      (or (not= 1 (count ars)) (:rest ar) (:kwargs ar))
      (do (when-not typed-args (run! #(infer ctx %) arg-nodes))
          (report! ctx node "TSP210-01" "generic overloads and variadic contracts are unsupported"
                   :severity (if (:strict (:opts ctx)) :error :warning))
          (assoc t/unknown :generic-failure true))
      (not= n (count (:params ar)))
      (do (when-not typed-args (run! #(infer ctx %) arg-nodes))
          (report! ctx node "TSP2554" (str label " expects " (count (:params ar)) " argument(s), got " n))
          (assoc t/unknown :generic-failure true))
      :else
      (let [entries (mapv (fn [i pt] {:i i :expected pt :node (nth arg-nodes i nil)})
                          (range n) (:params ar))
            callbacks (if typed-args [] (filter #(inline? (:node %)) entries))
            ordinary (if typed-args entries (remove #(inline? (:node %)) entries))
            state (reduce (fn [st {:keys [i expected node]}]
                            (solver/constrain st (eff ctx (if typed-args (nth typed-args i) (infer ctx node)))
                                              expected {:argument i})) state ordinary)
            partial (callback-substitution (trusted-substitution (solver/solve (:env ctx) state))
                                           (map :expected callbacks))
            state (reduce
                   (fn [st {:keys [i expected node]}]
                     (let [params (callback-context expected partial)
                           actual (cond
                                    (= :fn-lit (:op node))
                                    (if (and params (fn-lit-arity node))
                                      (infer-fn-lit ctx node params)
                                      (do (infer ctx node) t/unknown))
                                    params (infer-fn ctx node params)
                                    :else (do (infer-fn ctx node nil) t/unknown))]
                       ;; Keep the original expected return ivar: the callback's result solves it.
                       (solver/constrain st actual expected {:argument i :callback true})))
                   state callbacks)]
        (generic-result ctx node label state)))))

(defn- call-fn [ctx node fname ftype arg-nodes]
  (if (= :all (:t ftype))
    (call-generic ctx node fname ftype arg-nodes nil)
    (let [n (count arg-nodes)
        arg-types (mapv #(infer ctx %) arg-nodes)
        ars (:arities ftype)
        fits (filter #(arity-fits? % n) ars)]
    (cond
      (empty? ars) (or (:ret ftype) t/unknown)
      (empty? fits) (do (report! ctx node "TSP2554"
                                 (str fname " expects " (str/join " or " (distinct (map #(str (count (:params %)) (when (or (:rest %) (:kwargs %)) "+")) ars)))
                                      " argument(s), got " n))
                        (apply t/union (map :ret ars)))
      :else (let [all (map #(arg-problems ctx % arg-nodes arg-types node) fits)
                  passing (keep-indexed (fn [i ps] (when (empty? ps) i)) all)
                  vague? (some #(#{:unknown :any} (:t (t/resolve-t (:env ctx) %))) arg-types)
                  ;; Incomplete evidence is not a mismatch. Prefer a candidate
                  ;; whose known arguments fit, but still emit its uncertainty.
                  incomplete (keep-indexed
                               (fn [i ps]
                                 (when (and (seq ps)
                                            (every? #(= "TSP210-01" (first (second %))) ps)) i)) all)
                  ;; overloads go from narrow to general: with an unknown argument only the general one is safe
                  ;; nothing fits: if the only trouble is a possible nil, keep the overload that fits otherwise
                  non-nil (mapv #(t/remove-nil (:env ctx) (eff ctx %)) arg-types)
                  non-nil-problems (map #(arg-problems (assoc ctx :quiet true) % arg-nodes non-nil node) fits)
                  nil-only (first (keep-indexed (fn [i ps] (when (empty? ps) i)) non-nil-problems))
                  ;; Return evidence must include every candidate that could fit
                  ;; after removing nil, even if another argument is still unknown.
                  nil-compatible (keep-indexed
                                   (fn [i ps]
                                     (when (every? #(= "TSP210-01" (first (second %))) ps) i))
                                   non-nil-problems)
                  unchecked? (and (empty? passing)
                                  (some #(= "TSP210-01" (first (second %))) (mapcat identity all)))
                  possible (distinct (concat incomplete nil-compatible))
                  returns (distinct (map :ret (if (seq possible) (map #(nth fits %) possible) fits)))
                  i (or (if vague? (last passing) (first passing)) (first incomplete) nil-only (dec (count all)))]
              (doseq [[pn [code msg extra]] (nth all i)]
                (apply report! ctx pn code msg (mapcat identity extra)))
              ;; A merely incomplete candidate must not hide a possible nil
              ;; boundary on another viable overload. Keep these diagnostics
              ;; separate from both overload selection and return evidence.
              (when unchecked?
                (doseq [[pn [code msg extra]]
                        (distinct (for [j nil-compatible
                                        problem (nth all j)
                                        :when (and (= "TSP2531" (first (second problem)))
                                                   (not (some #{problem} (nth all i))))]
                                    problem))]
                  (apply report! ctx pn code msg (mapcat identity extra))))
              (cond
                unchecked? (if (next returns) t/unknown (first returns))
                (and vague? (next (distinct (map #(:ret (nth fits %)) passing)))) t/unknown
                :else (:ret (nth fits i))))))))

;; ---- inference ----

(defn- args-of [node] (vec (rest (:children node))))

(defn- record! [ctx node ty]
  (when (and *types* (:pos node) (not (:synthetic node)))
    (swap! *types* conj {:file (:file (:file ctx)) :pos (:pos node) :type ty :ns (:ns (:file ctx))}))
  ty)

(defn- known-ns? [ctx ns-sym]
  (or (contains? (:nss (:env ctx)) ns-sym) (some? (core/stdlib-publics ns-sym))))

(defn- resolve-var
  "{:sym full-name :type fn-type-or-nil :missing-in ns} for a symbol that is not a local."
  [ctx sym node]
  (let [file (:file ctx) env (:env ctx)
        r (env/resolve-sym file sym)]
    (if-let [n (some-> (namespace r) symbol)]
      (let [ty (or (var-type ctx r) (get core/table r))
            names (or (core/stdlib-publics n) (get-in env [:ns-defs n]))]
        (when (and names (not ty) (not (contains? names (symbol (name r)))))
          (report! ctx node "TSP2305" (str n " has no var " (name r) (hint-suffix (symbol (name r)) names))))
        {:sym r :type ty :known-ns? (boolean names)})
      (let [local (symbol (str (:ns file)) (name r))]
        (if (or (get-in env [:vars local]) (contains? (:macros env) local))
          {:sym local :type (var-type ctx local)}
          (let [c (symbol "clojure.core" (name r))]
            {:sym c :type (or (var-type ctx c) (get core/table c))}))))))

(defn- java-static?
  "Class name when `Foo/bar` refers to a Java class and not to a Clojure namespace."
  [ctx sym]
  (when-let [n (some-> (namespace sym) symbol)]
    (when-not (or (contains? (:aliases (:file ctx)) n) (known-ns? ctx n))
      (t/resolve-class (:imports (:file ctx)) n))))

(defn- quietly
  "Infers a type without reporting or recording anything."
  [ctx node]
  (binding [*types* nil *trust* (atom {:cast 0 :any 0 :ignore 0}) *ignores* (atom [])]
    (infer (assoc ctx :quiet true) node)))

(defn- widen [ty]
  (case (:t ty)
    :lit (t/lit-prim (:val ty))
    :union (apply t/union (map widen (:types ty)))
    ty))

(defn var-type
  "Type of a var. A function with no declared return type gets it inferred from its body, once."
  [ctx sym]
  (when-let [v (get-in (:env ctx) [:vars sym])]
    (cond
      (not (and (= :hints (:source v)) (some #(= :unknown (:t (:ret %))) (:arities (:type v))))) (:type v)
      (contains? @*rets* sym) (get @*rets* sym)
      :else
      (do (swap! *rets* assoc sym (:type v)) ; a recursive call sees the declared part only
          (let [fctx {:env (:env ctx) :file (:pfile v) :opts (:opts ctx) :locals {}}
                ars (mapv (fn [a {:keys [params body]}]
                            (if (not= :unknown (:t (:ret a)))
                              a
                              (let [{:keys [fixed rest]} (env/split-params params)
                                    c (reduce (fn [c [p ty]] (bind-pattern c p ty)) (assoc fctx :quiet true)
                                              (map vector fixed (:params a)))
                                    c (if rest (bind-pattern c rest (or (:kwargs a) t/unknown)) c)
                                    ret (binding [*types* nil *trust* (atom {:cast 0 :any 0 :ignore 0}) *ignores* (atom [])]
                                          (infer-body c body))]
                                (assoc a :ret (widen (dissoc ret :fresh)) :inferred true))))
                          (:arities (:type v)) (:arities (:parts v)))
                ty (assoc (:type v) :arities ars)]
            (swap! *rets* assoc sym ty)
            ty)))))

(defn- why-of [ctx init ty]
  (cond
    (= :sym (:op init)) (get-in ctx [:why (:val init)])
    (and (= :list (:op init)) (= :kw (:op (first (:children init)))) (second (:children init))
         (t/nilable? (:env ctx) ty))
    (let [k (:val (first (:children init)))
          mty (quietly ctx (second (:children init)))]
      (when (= :opt (:status (lookup-key ctx mty k)))
        (str k " is optional in " (t/show (t/remove-nil (:env ctx) mty)))))
    (and (= :list (:op init)) (= :sym (:op (first (:children init)))) (t/nilable? (:env ctx) ty))
    (str (parse/unparse (first (:children init))) " returns " (t/show ty))))

(def ^:private compiler-tagged-fns '#{str subs name format})

(defn- compiler-knows?
  "Does the Clojure compiler know the class of this expression (so the call is not reflective)?"
  [ctx node]
  (boolean
   (or (env/tag-node node)
       (#{:str :num :bool} (:op node))
       (and (= :sym (:op node)) (contains? (:hinted ctx) (:val node)))
       (when (= :list (:op node))
         (let [h (first (:children node))]
           (when (= :sym (:op h))
             (let [n (name (:val h))]
               (or (and (nil? (namespace (:val h)))
                        (or (str/starts-with? n ".") (str/ends-with? n ".") (= "new" n)
                            (compiler-tagged-fns (:val h))))
                   (java-static? ctx (:val h))
                   (and (not (contains? (:locals ctx) (:val h)))
                        (let [full (:sym (resolve-var (assoc ctx :quiet true) (:val h) h))]
                          (or (core/stdlib-tagged? full)
                              (get-in (:env ctx) [:vars full :tagged?]))))))))))))

(defn- bind-pairs [ctx bindings]
  (reduce (fn [ctx [p init]]
            (let [ty (dissoc (infer ctx init) :fresh)
                  why (why-of ctx init ty)
                  sym (when (= :sym (:op p)) (:val p))
                  captured (when (and sym (not (env/tag-node p))) (condition-capture ctx init))
                  ctx' (bind-pattern ctx p ty why (stable-path ctx init))]
              (record! ctx p (get-in ctx' [:locals (:val p)] ty))
              (cond-> ctx'
                captured (assoc-in [:conditions sym] captured)
                (and sym (compiler-knows? ctx init)) (update :hinted (fnil conj #{}) sym))))
          ctx (partition 2 (:children bindings))))

(defn- bind-seq-pairs [ctx bindings]
  (reduce (fn [ctx [p init]]
            (if (= :kw (:op p))
              (case (:val p)
                :let (bind-pairs ctx init)
                (:when :while) (or (first (condition-contexts ctx init)) ctx)
                ctx)
              (bind-pattern ctx p (elem-type ctx (infer ctx init)))))
          ctx (partition 2 (:children bindings))))

(defn infer-body [ctx forms]
  (reduce (fn [_ f] (infer ctx f)) t/nil-t forms))

(defn- infer-fn
  "Type of a (fn ...) form. `arg-types` gives the parameter types when the context knows them."
  [ctx node arg-types]
  (let [ctx (dissoc ctx :recur-types)
        more (rest (:children node))
        more (if (= :sym (:op (first more))) (rest more) more)
        arities (if (= :vector (:op (first more)))
                  [more]
                  (for [l more :when (= :list (:op l))] (:children l)))]
    {:t :fn
     :arities (vec (for [[params & body] arities
                         :let [{:keys [fixed rest]} (env/split-params params)
                               ptypes (map-indexed (fn [i p] (or (get (vec arg-types) i) (env/pattern-type (:env ctx) (:file ctx) p))) fixed)
                               ctx' (reduce (fn [c [p ty]] (bind-pattern c p ty)) ctx (map vector fixed ptypes))
                               ctx' (if rest (bind-pattern ctx' rest t/unknown) ctx')]]
                     {:params (vec ptypes) :rest (when rest t/unknown) :ret (infer-body ctx' body)}))}))

(defn- apply-type
  "Result of calling a function type with these argument types; mismatches are reported at `at`."
  [ctx ftype arg-types at label]
  (if (= :all (:t ftype))
    (call-generic ctx at label ftype nil arg-types)
    (if-let [fits (seq (filter #(arity-fits? % (count arg-types)) (:arities ftype)))]
    (let [bad (fn [ar] (first (for [[a p] (map vector arg-types (concat (:params ar) (repeat (:rest ar))))
                                    :when (and p (not (t/sub? (:env ctx) (eff ctx a) p)))]
                                [a p])))
          env (:env ctx)
          vague? (some #(#{:unknown :any} (:t (t/resolve-t env %))) arg-types)
          ok (remove bad fits)
          bad-non-nil (fn [ar] (first (for [[a p] (map vector arg-types (concat (:params ar) (repeat (:rest ar))))
                                            :when (and p (not (t/sub? env (t/remove-nil env (eff ctx a)) p)))]
                                        a)))
          nil-ok (remove bad-non-nil fits)
          ar (or (if vague? (last ok) (first ok)) (first nil-ok) (last fits))]
      (when-let [[a p] (bad ar)]
        (if (seq nil-ok)
          (report! ctx at "TSP2531" (str label " takes " (show-ref ctx p) ", but may get nil: " (show-ref ctx a)))
          (let [result (t/sub-result env (eff ctx a) p)]
            (if (= :inconclusive (:status result))
              (let [[code msg extra] (incomplete-problem ctx a p result)]
                (apply report! ctx at code msg (mapcat identity extra)))
              (report! ctx at "TSP2345" (str label " takes " (show-ref ctx p) ", but gets " (show-ref ctx a)))))))
      (if (and vague? (next (distinct (map :ret ok)))) t/unknown (:ret ar)))
    (if (seq (:arities ftype))
      (do (report! ctx at "TSP2554" (str label " is called with " (count arg-types) " argument(s)")) t/unknown)
      t/unknown))))

(defn apply-fn-node [ctx fnode arg-types]
  (case (:op fnode)
    :kw (read-key ctx (first arg-types) (:val fnode) fnode)
    :fn-lit (let [ctx' (reduce (fn [c [s ty]] (bind c s ty)) (dissoc ctx :recur-types)
                               (map vector '[% %1 %2 %3] (cons (first arg-types) arg-types)))]
              (infer ctx' {:op :list :children (:children fnode) :pos (:pos fnode)}))
    :set (t/union (first arg-types) t/nil-t)
    :list (if (#{'fn 'fn*} (env/head (:file ctx) fnode))
            (apply t/union (map :ret (:arities (infer-fn ctx fnode arg-types))))
            (apply-type ctx (infer ctx fnode) arg-types fnode (parse/unparse fnode)))
    (apply-type ctx (infer ctx fnode) arg-types fnode (parse/unparse fnode))))

(defn- single-map [ctx ty]
  (let [ms (t/members (:env ctx) ty)]
    (when (and (= 1 (count ms)) (= :map (:t (first ms)))) (first ms))))

(defn- check-key-value!
  "Key = type: a value stored under a qualified key must fit the spec of that key."
  [ctx k vnode vtype]
  (when (and (qualified-keyword? k) (get-in (:env ctx) [:specs k]))
    (when-not (t/sub? (:env ctx) (eff ctx vtype) (t/ref-t k))
      (report! ctx vnode "TSP2322" (str (t/show vtype) " is not assignable to " (show-ref ctx (t/ref-t k)))))))

(defn- key-type [ctx k vtype]
  (if (and (qualified-keyword? k) (get-in (:env ctx) [:specs k])) (t/ref-t k) (widen vtype)))

;; ---- Java interop ----

(defn- class-of [ctx ty]
  (let [ms (remove #(= :nil (:t %)) (t/members (:env ctx) ty))]
    (when (= 1 (count ms))
      (let [m (first ms)]
        (case (:t m)
          :class (:name m)
          :prim (t/prim->class (:name m))
          :lit (t/prim->class (:name (t/lit-prim (:val m))))
          nil)))))

(defn- java-arg-ok? [ctx a pclass]
  (let [p (t/class-type pclass) env (:env ctx)
        num? (fn [x] (t/sub? env x (t/prim :number)))]
    (or (#{:unknown :any} (:t (t/resolve-t env a)))
        (t/sub? env (if (t/primitives pclass) a (t/remove-nil env a)) p)
        (and (num? (t/remove-nil env a)) (num? p))
        (and (= "char" pclass) (= :class (:t a))))))

(defn- java-ret [cname m]
  (let [base (t/class-type (:ret m))
        shown (java/short-name (:ret m))]
    (cond
      (java/nullable-return? cname (:name m))
      (with-meta (t/union base t/nil-t) {:null-return (str (java/short-name (:declaring m)) "." (:name m) " may return null")})
      (#{:nil :any} (:t base)) base
      :else (t/platform base shown))))

(defn- java-call [ctx node cname mname static? arg-nodes]
  (let [arg-types (mapv #(infer ctx %) arg-nodes)
        ms (map #(assoc % :name mname) (java/methods-named cname mname static?))
        fits (filter #(= (count arg-nodes) (count (:params %))) ms)
        shown (java/short-name cname)]
    (cond
      (not (java/has-class? cname)) t/unknown
      (empty? ms) (if (and (empty? arg-nodes) (java/has-field? cname mname))
                    t/unknown
                    (do (report! ctx (first (:children node)) "TSP2339"
                                 (str shown " has no " (when static? "static ") "method " mname
                                      (hint-suffix (symbol mname) (map symbol (java/method-names cname)))))
                        t/unknown))
      (some :varargs ms) t/unknown
      (empty? fits) (do (report! ctx node "TSP2554"
                                 (str shown "." mname " takes " (str/join " or " (distinct (map #(count (:params %)) ms)))
                                      " argument(s), got " (count arg-nodes)))
                        t/unknown)
      :else
      (let [ok (filter (fn [m] (every? boolean (map #(java-arg-ok? ctx %1 %2) arg-types (:params m)))) fits)]
        (when (empty? ok)
          ;; report only when every overload rejects the same argument
          ;; report the first argument that no overload accepts; stay silent when there is no such argument
          (when-let [i (first (filter (fn [i] (every? #(not (java-arg-ok? ctx (nth arg-types i) (nth (:params %) i))) fits))
                                      (range (count arg-nodes))))]
            (report! ctx (nth arg-nodes i) "TSP2345"
                     (str (t/show (nth arg-types i)) " is not assignable to " (java/short-name (nth (:params (first fits)) i)) ", "
                          (if (= 1 (count fits)) "the parameter" "a parameter") " of " shown "." mname))))
        (let [cands (if (seq ok) ok fits)
              rets (distinct (map #(java-ret cname %) cands))]
          (if (= 1 (count rets)) (first rets) t/unknown))))))

(defn- method-call [ctx node mname]
  (let [[_ target & arg-nodes] (:children node)
        actual (infer ctx target)
        hinted (check-hint! ctx target actual "type of the receiver")
        ;; Compatible hints must not erase known nullability or its provenance.
        tty (eff ctx (if (and hinted
                             (or (#{:unknown :any} (:t actual))
                                 (not (t/sub? (:env ctx) (t/remove-nil (:env ctx) actual) hinted))))
                      hinted actual))
        env (:env ctx)
        cname (class-of ctx tty)]
    (cond
      (nil? target) t/unknown
      (#{:unknown :any} (:t (t/resolve-t env tty)))
      (do (run! #(infer ctx %) arg-nodes)
          (when-not (compiler-knows? ctx target)
            (report! ctx node "TSP7020" (str "reflective call: the type of " (parse/unparse target) " is unknown")
                     :severity :warning))
          t/unknown)
      (nil? cname) (do (run! #(infer ctx %) arg-nodes) t/unknown)
      :else
      (do
        (when (t/nilable? env tty)
          (report! ctx target "TSP2531"
                   (str (parse/unparse target) " may be nil"
                        (when-let [w (or (:null-return (meta tty)) (when (= :sym (:op target)) (get-in ctx [:why (:val target)])))]
                          (str ": " w)))))
        (when (and (java/has-class? cname) (not (compiler-knows? ctx target)) (seq (java/methods-named cname mname false)))
          (let [hint (java/short-name cname)]
            (report! ctx node "TSP7023"
                     (str "reflective call, but " (parse/unparse target) " is known to be " hint)
                     :severity :warning
                     :fixes [{:title (str "Add hint ^" hint) :risk "safe"
                              :edits [{:range {:start [(:row (:pos target)) (:col (:pos target))]
                                               :end [(:row (:pos target)) (:col (:pos target))]}
                                       :text (str "^" hint " ")}]}])))
        (java-call ctx node cname mname false arg-nodes)))))

;; ---- special forms ----

(defmulti special (fn [_ctx _node head] head) :default ::none)
(defmethod special ::none [_ _ _] ::none)

(doseq [h '[ns comment declare require import in-ns defmacro defprotocol defrecord deftype defmulti
            definterface set! var clojure.spec.alpha/def clojure.spec.alpha/fdef]]
  (defmethod special h [_ _ _] t/unknown))

(defmethod special 'quote [_ node _]
  (let [x (second (:children node))]
    (if (= :sym (:op x)) (t/prim :symbol) t/unknown)))

(defmethod special 'do [ctx node _] (infer-body ctx (args-of node)))

(defmethod special 'def [ctx node _] (some->> (last (args-of node)) (infer ctx)) t/unknown)
(defmethod special 'defonce [ctx node _] (some->> (last (args-of node)) (infer ctx)) t/unknown)

(defmethod special 'let [ctx node _]
  (let [[bindings & body] (args-of node)]
    (infer-body (bind-pairs ctx bindings) body)))

(defmethod special 'loop [ctx node _]
  (let [[bindings & body] (args-of node)
        pairs (vec (partition 2 (:children bindings)))
        ;; Initializers have let-like scope and are checked exactly once.
        [_ seeds] (reduce (fn [[c ts] [p init]]
                            (let [ty (widen (infer c init))]
                              [(bind-pattern c p ty) (conj ts ty)]))
                          [ctx []] pairs)
        bind-loop (fn [ts calls]
                    (reduce (fn [c [[p _] ty]] (bind-pattern c p ty))
                            (assoc ctx :recur-types calls) (map vector pairs ts)))
        ;; Join back edges, not just initializers: recur can change a local's
        ;; type. Bound structural growth and keep trial diagnostics/caches local.
        stable (binding [*types* nil *diags* (atom [])
                         *trust* (atom {:cast 0 :any 0 :ignore 0})
                         *ignores* (atom []) *rets* (atom @*rets*)]
                 (loop [ts seeds n 0]
                   (let [calls (atom [])
                         _ (infer-body (assoc (bind-loop ts calls) :quiet true) body)
                         next-ts (if (every? #(= (count ts) (count %)) @calls)
                                   (mapv (fn [i ty]
                                           (widen (apply t/union ty (map #(nth % i) @calls))))
                                         (range (count ts)) ts)
                                   (vec (repeat (count ts) t/unknown)))]
                     (cond (= ts next-ts) ts
                           (>= n 7) (vec (repeat (count ts) t/unknown))
                           :else (recur next-ts (inc n))))))]
    (infer-body (bind-loop stable nil) body)))

(defmethod special 'recur [ctx node _]
  (let [ts (mapv #(infer ctx %) (args-of node))]
    (when-let [calls (:recur-types ctx)] (swap! calls conj ts)))
  t/never)
(defmethod special 'throw [ctx node _] (run! #(infer ctx %) (args-of node)) t/never)

(doseq [h '[some? nil?]]
  (defmethod special h [ctx node head]
    (let [ty (eff ctx (infer ctx (first (args-of node))))
          present (t/remove-nil (:env ctx) ty)
          absent (nil-part (:env ctx) ty)]
      (cond (= :never (:t present)) (t/lit (= head 'nil?))
            (= :never (:t absent)) (t/lit (= head 'some?))
            :else (t/prim :boolean)))))

(defn- if-form [ctx node inverted?]
  (let [[test then else] (args-of node)
        branches (condition-contexts ctx test)
        [tc fc] (if inverted? (reverse branches) branches)]
    (t/union (if tc (infer tc then) t/never)
             (if fc (if else (infer fc else) t/nil-t) t/never))))

(defmethod special 'if [ctx node _] (if-form ctx node false))

(defmethod special 'if-not [ctx node _] (if-form ctx node true))

(defn- when-form [ctx node inverted?]
  (let [[test & body] (args-of node)
        branches (condition-contexts ctx test)
        [tc fc] (if inverted? (reverse branches) branches)]
    (t/union (if tc (infer-body tc body) t/never)
             (if fc t/nil-t t/never))))

(defmethod special 'when [ctx node _] (when-form ctx node false))
(defmethod special 'when-not [ctx node _] (when-form ctx node true))

(defn- if-let-form [ctx node some?]
  (let [[bindings then else] (args-of node)
        [p init] (:children bindings)
        ty (infer ctx init)
        positive (if some? (t/remove-nil (:env ctx) ty) (truth-part (:env ctx) ty true))
        negative (if some? (nil-part (:env ctx) ty) (truth-part (:env ctx) ty false))
        tc (bind-pattern ctx p (dissoc positive :fresh))]
    [(if (= :never (:t positive)) t/never (infer tc then))
     (if (= :never (:t negative)) t/never (if else (infer ctx else) t/nil-t))]))

(defmethod special 'if-let [ctx node _] (apply t/union (if-let-form ctx node false)))
(defmethod special 'if-some [ctx node _] (apply t/union (if-let-form ctx node true)))

(defn- when-let-form [ctx node some?]
  (let [[bindings & body] (args-of node)
        [p init] (:children bindings)
        ty (infer ctx init)
        positive (if some? (t/remove-nil (:env ctx) ty) (truth-part (:env ctx) ty true))
        negative (if some? (nil-part (:env ctx) ty) (truth-part (:env ctx) ty false))
        tc (bind-pattern ctx p (dissoc positive :fresh))]
    (t/union (if (= :never (:t positive)) t/never (infer-body tc body))
             (if (= :never (:t negative)) t/never t/nil-t))))

(defmethod special 'when-let [ctx node _] (when-let-form ctx node false))
(defmethod special 'when-some [ctx node _] (when-let-form ctx node true))

(defmethod special 'cond [ctx node _]
  (loop [ctx ctx pairs (partition 2 (args-of node)) out []]
    (cond
      (nil? ctx) (apply t/union out)
      (empty? pairs) (apply t/union (conj out t/nil-t))
      :else (let [[test branch] (first pairs)
                  [tc fc] (condition-contexts ctx test)]
              (recur fc (rest pairs) (cond-> out tc (conj (infer tc branch))))))))

(defn- logical-form [ctx args and?]
  (if (empty? args)
    (t/lit and?)
    (let [a (first args)
          ty (infer ctx a)]
      (if-not (next args)
        ty
        (let [branches (narrow ctx a)
              [proceed stop] (if and? branches (reverse branches))
              ety (eff ctx ty)
              proceed-type (truth-part (:env ctx) ety and?)
              stop-type (truth-part (:env ctx) ety (not and?))]
          (t/union (if stop stop-type t/never)
                   (if (and proceed (not= :never (:t proceed-type)))
                     (logical-form proceed (rest args) and?)
                     t/never)))))))

(defmethod special 'and [ctx node _] (logical-form ctx (args-of node) true))
(defmethod special 'or [ctx node _] (logical-form ctx (args-of node) false))

(defn- case-int? [v]
  (and (integer? v) (<= Integer/MIN_VALUE v Integer/MAX_VALUE)))

(defn- case-tag-type? [int-mode? {:keys [t val]}]
  (or (= :nil t)
      (and (= :lit t)
           (or (keyword? val) (string? val) (boolean? val)
               ;; Hash-based case can distinguish equal numbers by JVM representation.
               (and int-mode? (case-int? val))))))

(defmethod special 'case [ctx node _]
  (let [[test & clauses] (args-of node)
        env (:env ctx)
        tty (infer ctx test)
        default? (odd? (count clauses))
        pairs (partition 2 clauses)
        lits (fn [n] (if (= :list (:op n)) (:children n) [n]))
        labels (mapcat (comp lits first) pairs)
        literal-labels? (every? literal-node? labels)
        subject (stable-path ctx test)
        int-mode? (every? #(case-int? (:val %)) labels)
        paths (when (and subject literal-labels?)
                (for [[member rebuild] (path-alternatives ctx (:type subject) (:path subject))
                      leaf (expand-boolean member)]
                  [leaf rebuild]))
        leaves (if subject (map first paths) (mapcat expand-boolean (t/members env (eff ctx tty))))
        closed? (and literal-labels? (seq leaves) (every? #(case-tag-type? int-mode? %) leaves))
        variants (when closed? paths)
        members (distinct leaves)
        handled (set (map :val labels))
        missing (remove #(contains? handled (:val %)) members)
        local (plain-local ctx test)
        infer-variant (fn [labels branch]
                        (if-let [branch-ctx (if variants
                                             (refine-alternatives ctx subject variants #(when (contains? labels (:val %)) %))
                                             (when (some #(contains? labels (:val %)) members) ctx))]
                          (infer branch-ctx branch)
                          t/never))]
    (when closed?
      (doseq [l labels :when (not (some #(= (:val l) (:val %)) members))]
        (report! ctx l "TSP2678" (str (pr-str (:val l)) " is not a possible value of " (show-ref ctx tty)
                                      (hint-suffix (:val l) (map :val members)))))
      (when (and (not default?) (seq missing))
        (report! ctx node "TSP8001" (str "case is not exhaustive: " (str/join ", " (map #(pr-str (:val %)) missing))
                                        (if (next missing) " are" " is") " not handled"))))
    (apply t/union
           (concat
            (for [[c branch] pairs]
              (if closed?
                (infer-variant (set (map :val (lits c))) branch)
                (infer (if (and local (every? literal-node? (lits c)))
                         (assoc-in ctx [:locals local] (apply t/union (map (comp literal-type :val) (lits c))))
                         ctx)
                       branch)))
            (when default?
              [(if closed?
                 (infer-variant (set (map :val missing)) (last clauses))
                 (infer ctx (last clauses)))])))))

(defn- literal-domain [ctx expr]
  (when-let [{:keys [type path]} (stable-path ctx expr)]
    (let [leaves (mapcat (comp expand-boolean first) (path-alternatives ctx type path))]
      (when (and (seq leaves) (every? finite-value? leaves))
        (distinct (map :val leaves))))))

(defn- condp-clauses [nodes]
  (loop [[test branch result & more :as left] nodes out []]
    (cond
      (nil? branch) {:pairs out :default test}
      (and (= :>> (:val branch)) result) (recur more (conj out [test result true]))
      :else (recur (drop 2 left) (conj out [test branch false])))))

(defmethod special 'condp [ctx node _]
  (let [[pred expr & clauses] (args-of node)
        pred-type (infer ctx pred)
        expr-type (infer ctx expr)
        equality? (= '= (builtin-head ctx {:op :list :children [pred]}))
        {:keys [pairs default]} (condp-clauses clauses)
        finite? (and equality? (every? #(literal-node? (first %)) pairs) (seq (literal-domain ctx expr)))]
    (loop [ctx ctx [[test result invoke?] & more] pairs out []]
      (cond
        (nil? ctx) (apply t/union out)
        (nil? test) (if default
                      (apply t/union (conj out (infer ctx default)))
                      (do (when-let [missing (and finite? (seq (literal-domain ctx expr)))]
                            (report! ctx node "TSP8001" (str "condp is not exhaustive: " (str/join ", " (map pr-str missing))
                                                            (if (next missing) " are" " is") " not handled")))
                          (apply t/union out)))
        :else (let [test-type (infer ctx test)
                    matched (apply-type ctx pred-type [test-type expr-type] test "condp predicate")
                    [yes no] (or (when (and equality? (literal-node? test))
                                  (equality-contexts ctx expr (:val test)))
                                [ctx ctx])
                    present (truth-part (:env ctx) matched true)
                    absent (truth-part (:env ctx) matched false)
                    yes (when-not (= :never (:t present)) yes)
                    no (when-not (= :never (:t absent)) no)]
                (recur no more
                       (cond-> out yes
                         (conj (if invoke?
                                 (apply-fn-node yes result [present])
                                 (infer yes result))))))))))

(defmethod special 'fn [ctx node _] (infer-fn ctx node nil))
(defmethod special 'fn* [ctx node _] (infer-fn ctx node nil))

(defmethod special 'try [ctx node _]
  (let [forms (args-of node)
        catch? #(= 'catch (env/head (:file ctx) %))
        finally? #(= 'finally (env/head (:file ctx) %))
        body (remove #(or (catch? %) (finally? %)) forms)]
    (run! #(infer-body ctx (args-of %)) (filter finally? forms))
    (apply t/union
           (infer-body ctx body)
           (for [c (filter catch? forms)
                 :let [[cls sym & cbody] (args-of c)
                       cn (t/resolve-class (:imports (:file ctx)) (:val cls))]]
             (infer-body (bind ctx (:val sym) (if cn (t/class-type cn) t/unknown) :hinted true) cbody)))))

(defn- thread [ctx node first? some?]
  (let [[x & steps] (args-of node) env (:env ctx)]
    (loop [ty (infer ctx x) prev x steps steps maybe-nil? false shown (parse/unparse x) n 0]
      (if (empty? steps)
        (if (and some? maybe-nil?) (t/union ty t/nil-t) ty)
        (let [step (first steps)
              arrow (parse/unparse (first (:children node)))
              acc {:op :sym :val '__threaded :pos (:pos prev) :synthetic true
                   :display (if (zero? n) shown (str "(" arrow " " shown ")"))}
              call (if (= :list (:op step))
                     (let [[h & as] (:children step)]
                       (assoc step :children (vec (if first? (concat [h acc] as) (concat [h] as [acc])))))
                     {:op :list :pos (:pos step) :children [step acc]})
              in (if some? (t/remove-nil env ty) ty)
              ctx' (cond-> (bind ctx '__threaded in :why (why-of ctx prev ty))
                     (compiler-knows? ctx prev) (update :hinted (fnil conj #{}) '__threaded))]
          (recur (infer ctx' call) step (rest steps) (or maybe-nil? (t/nilable? env ty))
                 (str shown " " (parse/unparse step)) (inc n)))))))

(defmethod special '-> [ctx node _] (thread ctx node true false))
(defmethod special '->> [ctx node _] (thread ctx node false false))
(defmethod special 'some-> [ctx node _] (thread ctx node true true))
(defmethod special 'some->> [ctx node _] (thread ctx node false true))

(defmethod special 'doseq [ctx node _]
  (let [[bindings & body] (args-of node)]
    (infer-body (bind-seq-pairs ctx bindings) body)
    t/nil-t))

(defmethod special 'for [ctx node _]
  (let [[bindings & body] (args-of node)]
    {:t :coll :kind :seq :of (widen (dissoc (infer-body (bind-seq-pairs ctx bindings) body) :fresh))}))

(defmethod special 'dotimes [ctx node _]
  (let [[bindings & body] (args-of node)
        [s n] (:children bindings)]
    (infer ctx n)
    (infer-body (bind ctx (:val s) (t/prim :int)) body)
    t/nil-t))

(defmethod special 'defmethod [ctx node _]
  (let [[_ _ & more] (args-of node)
        [params & body] (drop-while #(not= :vector (:op %)) more)]
    (when params
      (infer-body (reduce #(bind-pattern %1 %2 (env/pattern-type (:env ctx) (:file ctx) %2)) ctx
                          (:fixed (env/split-params params)))
                  body))
    t/unknown))

(doseq [h '[with-open binding with-redefs with-local-vars]]
  (defmethod special h [ctx node _]
    (let [[bindings & body] (args-of node)]
      (infer-body (if (= 'with-open h)
                    (bind-pairs ctx bindings)
                    (do (run! #(infer ctx (second %)) (partition 2 (:children bindings))) ctx))
                  body))))

(defmethod special 'doto [ctx node _]
  (let [[x & forms] (args-of node)
        ty (infer ctx x)]
    (run! (fn [f] (run! #(infer ctx %) (rest (:children f)))) (filter #(= :list (:op %)) forms))
    ty))

(defmethod special 'as-> [ctx node _]
  (let [[x nm & forms] (args-of node)]
    (reduce (fn [ty f] (infer (bind ctx (:val nm) (dissoc ty :fresh)) f)) (infer ctx x) forms)))

(doseq [h '[cond-> cond->>]]
  (defmethod special h [ctx node _]
    (let [[x & clauses] (args-of node)
          ty (infer ctx x)]
      ;; tests are checked; the threaded forms are not (the result may change shape in every step)
      (run! #(infer ctx (first %)) (partition 2 clauses))
      (if (every? (fn [[_ f]] (and (= :list (:op f)) (#{'assoc 'dissoc 'update 'merge} (env/head (:file ctx) f))
                                   (= 'cond-> h)))
                  (partition 2 clauses))
        t/unknown
        t/unknown))))

;; maps

(defmethod special 'get [ctx node _]
  (let [[m k default] (args-of node)
        mty (infer ctx m)]
    (if (= :kw (:op k))
      (if default
        (read-key-default ctx mty (:val k) k (infer ctx default))
        (read-key ctx mty (:val k) k))
      (do (infer ctx k) (some->> default (infer ctx)) t/unknown))))

(defmethod special 'get-in [ctx node _]
  (let [[m path default] (args-of node)
        mty (infer ctx m)]
    (some->> default (infer ctx))
    (if (and (= :vector (:op path)) (every? #(#{:kw :num} (:op %)) (:children path)))
      (reduce (fn [ty p]
                (if (= :kw (:op p))
                  (read-key ctx ty (:val p) p)
                  (indexed (elem-type ctx ty))))
              mty (:children path))
      (do (infer ctx path) t/unknown))))

(defmethod special 'assoc [ctx node _]
  (let [[m & kvs] (args-of node)
        mty (infer ctx m)]
    (reduce (fn [ty [k v]]
              (let [vty (infer ctx v)]
                (if (= :kw (:op k))
                  (do (check-key-value! ctx (:val k) v vty)
                      (if-let [mm (single-map ctx ty)]
                        (-> mm (dissoc :fresh) (assoc-in [:req (:val k)] (key-type ctx (:val k) vty)) (update :opt dissoc (:val k)))
                        ty))
                  (do (infer ctx k) ty))))
            mty (partition 2 kvs))))

(defmethod special 'dissoc [ctx node _]
  (let [[m & ks] (args-of node)
        mty (infer ctx m)]
    (if-let [mm (and (every? #(= :kw (:op %)) ks) (single-map ctx mty))]
      (reduce #(-> %1 (update :req dissoc (:val %2)) (update :opt dissoc (:val %2))) (dissoc mm :fresh) ks)
      mty)))

(defmethod special 'update [ctx node _]
  (let [[m k f & args] (args-of node)
        mty (infer ctx m)
        arg-types (mapv #(infer ctx %) args)]
    (if (= :kw (:op k))
      (let [cur (read-key ctx mty (:val k) k)
            ret (apply-fn-node ctx f (into [cur] arg-types))]
        (check-key-value! ctx (:val k) f ret)
        mty)
      (do (infer ctx k) (infer ctx f) mty))))

(defmethod special 'merge [ctx node _]
  (let [tys (map #(infer ctx %) (args-of node))
        ms (map #(single-map ctx %) tys)]
    (if (and (seq ms) (every? some? ms))
      (reduce (fn [a b]
                (let [req (merge (reduce-kv (fn [r k v]
                                             (if (contains? r k)
                                               (update r k t/union v)
                                               r))
                                           (:req a) (:opt b))
                                 (:req b))
                      opt (merge-with t/union (:opt a) (:opt b))]
                  {:t :map :req req :opt (apply dissoc opt (keys req))}))
              ms)
      t/unknown)))

(defmethod special 'select-keys [ctx node _]
  (let [[m ks] (args-of node)
        mty (infer ctx m)]
    (if (and (= :vector (:op ks)) (every? #(= :kw (:op %)) (:children ks)))
      (let [infos (for [k (:children ks)] [(:val k) (key-info ctx mty (:val k) k)])]
        {:t :map
         :req (into (array-map) (for [[k i] infos :when (not (:optional? i))] [k (:type i)]))
         :opt (into (array-map) (for [[k i] infos :when (:optional? i)] [k (:type i)]))})
      (do (infer ctx ks) t/unknown))))

;; collections

(defn- coll-of [kind ty] {:t :coll :kind kind :of (widen (dissoc ty :fresh))})

(doseq [h '[first second last peek rand-nth ffirst]]
  (defmethod special h [ctx node _]
    (let [[c] (args-of node)]
      (indexed (elem-type ctx (infer ctx c))))))

(defmethod special 'nth [ctx node _]
  (let [[c & more] (args-of node)]
    (run! #(infer ctx %) more)
    (elem-type ctx (infer ctx c))))

(doseq [h '[rest next seq reverse distinct sort shuffle butlast vec set doall]]
  (defmethod special h [ctx node _]
    (let [[c] (args-of node)
          el (elem-type ctx (infer ctx c))
          res (coll-of (get '{vec :vector set :set} h :seq) el)]
      (if ('#{next seq butlast} h) (t/union res t/nil-t) res))))

(doseq [h '[take drop take-last drop-last partition-all]]
  (defmethod special h [ctx node _]
    (let [[n c] (args-of node)]
      (infer ctx n)
      (coll-of :seq (elem-type ctx (infer ctx c))))))

(doseq [h '[map mapv keep mapcat pmap]]
  (defmethod special h [ctx node _]
    (let [[f & colls] (args-of node)
          els (mapv #(elem-type ctx (infer ctx %)) colls)]
      (if (empty? colls)
        (do (infer ctx f) t/unknown)
        (let [ret (apply-fn-node ctx f els)
              ret (case h keep (t/remove-nil (:env ctx) ret) mapcat (elem-type ctx ret) ret)]
          (coll-of (if (= 'mapv h) :vector :seq) ret))))))

(doseq [h '[filter remove filterv take-while drop-while sort-by group-by some every? not-any? run! max-key min-key]]
  (defmethod special h [ctx node _]
    (let [[f & more] (args-of node)
          c (last more)]
      (if (nil? c)
        (do (infer ctx f) t/unknown)
        (let [el (elem-type ctx (infer ctx c))
              ret (apply-fn-node ctx f [el])]
          (case h
            (filter remove take-while drop-while sort-by) (coll-of :seq el)
            filterv (coll-of :vector el)
            (every? not-any?) (t/prim :boolean)
            some (t/union (t/remove-nil (:env ctx) ret) t/nil-t)
            run! t/nil-t
            (max-key min-key) el
            group-by {:t :map-of :k (widen ret) :v (coll-of :vector el)}))))))

(defmethod special 'reduce [ctx node _]
  (let [[f a b] (args-of node)
        [init c] (if b [a b] [nil a])
        el (elem-type ctx (infer ctx c))
        acc (if init (widen (dissoc (infer ctx init) :fresh)) el)]
    (if (and (= :sym (:op f)) (not (contains? (:locals ctx) (:val f))))
      (apply-type ctx (infer ctx f) [acc el] f (parse/unparse f))
      (do (apply-fn-node ctx f [acc el]) (if init acc t/unknown)))))

(defmethod special 'conj [ctx node _]
  (let [[c & xs] (args-of node)
        cty (infer ctx c)
        xts (map #(infer ctx %) xs)
        m (first (remove #(= :nil (:t %)) (t/members (:env ctx) cty)))]
    (if (= :coll (:t m))
      (assoc m :of (apply t/union (:of m) (map #(widen (dissoc % :fresh)) xts)))
      cty)))

(defmethod special 'into [ctx node _]
  (let [[to & more] (args-of node)
        tty (infer ctx to)
        el (elem-type ctx (infer ctx (last more)))]
    (run! #(infer ctx %) (butlast more))
    (cond (and (= :vector (:op to)) (empty? (:children to)) (= 1 (count more))) (coll-of :vector el)
          (and (= :set (:op to)) (empty? (:children to)) (= 1 (count more))) (coll-of :set el)
          :else (dissoc tty :fresh))))

(defmethod special 'new [ctx node _]
  (let [[cls & args] (args-of node)]
    (run! #(infer ctx %) args)
    (if-let [c (and (= :sym (:op cls)) (t/resolve-class (:imports (:file ctx)) (:val cls)))]
      ;; A concrete Object constructor is not the declared Object/top escape.
      (if (= c "java.lang.Object") {:t :class :name c} (t/class-type c))
      t/unknown)))

;; ---- defn ----

(defn- check-return! [ctx node actual expected]
  (when-not (or (:generic-failure actual) (#{:unknown :any} (:t expected)))
    (let [env (:env ctx) actual (eff ctx actual)
          result (t/sub-result env actual expected)
          emit (fn [[c m extra]] (apply report! ctx node c m (mapcat identity extra)))]
      (cond
        (= :proved (:status result)) (run! emit (problems ctx node actual expected))
        (seq (fresh-problems ctx actual expected)) (run! emit (fresh-problems ctx actual expected))
        (= :inconclusive (:status result)) (emit (incomplete-problem ctx actual expected result))
        (and (t/nilable? env actual) (not (t/nilable? env expected)) (t/sub? env (t/remove-nil env actual) expected))
        (report! ctx node "TSP2322"
                 (str "may return nil, but the signature says " (show-ref ctx expected)
                      (when-let [w (and (= :sym (:op node)) (get-in ctx [:why (:val node)]))] (str ": " w))))
        :else (if-let [[k opt?] (missing-req ctx actual expected)]
                (report! ctx node "TSP2741" (str "the returned map has no required key " k " of " (t/show expected)
                                                 (when opt? " (it is optional in the returned map)")))
                (report! ctx node "TSP2322" (str "returns " (show-ref ctx actual) ", but the signature says " (show-ref ctx expected))))))))

(defn- check-defn [ctx node]
  (let [file (:file ctx) env (:env ctx)
        parts (env/defn-parts node)
        nm (:name parts)
        sym (symbol (str (:ns file)) (str (:val nm)))
        v (get-in env [:vars sym])
        ftype (if (= :all (:t (:type v))) (solver/open-rigid (:type v)) (:type v))]
    (when-let [tn (env/tag-node nm)]
      (when (t/primitives (str (:val tn)))
        (report! ctx tn "TSP7021"
                 (str "a hint on the name is not a return type: write (defn " (:val nm) " ^" (:val tn) " [...] ...)"))))
    (when (and (:strict (:opts ctx)) (not (:private? parts)) (= :hints (:source v))
               (some (fn [a] (or (= :unknown (:t (:ret a))) (some #(= :unknown (:t %)) (:params a)))) (:arities ftype)))
      (report! ctx nm "TSP7010" (str "public function " (:val nm) " has no signature: add s/fdef, {:t/sig '[...]} or type hints")))
    (doseq [{:keys [params body]} (:arities parts)
            :let [{:keys [fixed rest]} (env/split-params params)
                  declared (or (first (filter #(and (= (count fixed) (count (:params %)))
                                                    (= (boolean rest) (boolean (or (:rest %) (:kwargs %)))))
                                              (:arities ftype)))
                               (first (filter #(= (count fixed) (count (:params %))) (:arities ftype))))]]
      (if (nil? declared)
        (report! ctx params "TSP2790" (str "the signature of " (:val nm) " has no arity with " (count fixed) " parameter(s)"))
        (let [prims (filter #(some-> (env/tag-node %) :val str t/primitives) fixed)
              _ (when (and (seq prims) (> (count fixed) 4))
                  (report! ctx params "TSP7025" "functions taking primitives support only 4 or fewer parameters"))
              ctx' (reduce (fn [c [p ty]]
                             (let [c' (bind-pattern c p ty)]
                               (record! ctx p ty)
                               (if (= :sym (:op p)) (update c' :params (fnil conj #{}) (:val p)) c')))
                           (dissoc ctx :recur-types) (map vector fixed (:params declared)))
              ctx' (cond (nil? rest) ctx'
                         (:kwargs declared) (bind-pattern ctx' rest (:kwargs declared))
                         (:rest declared) (bind-pattern ctx' rest (t/union {:t :coll :kind :seq :of (:rest declared)} t/nil-t))
                         :else (bind-pattern ctx' rest t/unknown))
              ret-hint (env/hint-type file params)]
          (when (and ret-hint (not (#{:unknown :any} (:t (:ret declared))))
                     (not (t/sub? env ret-hint (:ret declared))) (not (t/sub? env (:ret declared) ret-hint)))
            (report! ctx (env/tag-node params) "TSP2322"
                     (str "hint " (t/show ret-hint) " contradicts the signature: " (t/show (:ret declared)))))
          (let [rty (infer-body ctx' body)]
            (when (seq body) (check-return! ctx' (last body) rty (:ret declared)))))))
    t/unknown))

(defmethod special 'defn [ctx node _] (check-defn ctx node))
(defmethod special 'defn- [ctx node _] (check-defn ctx node))

;; ---- infer ----

(defn- parse-directive-type [ctx d]
  (let [form (parse/node->data (second (:children d)))]
    (t/parse-type (env/type-ctx (:file ctx) (fn [f] (report! ctx d "TSP9001" (str "not a type: " (pr-str f))))) form)))

(defn- check-macro-body [ctx node contract]
  (if-not (macros/valid-bindings? node contract)
    (report! ctx node "TSP7032"
             "macro contract requires a first-argument vector of distinct simple symbols matching :bindings; body not checked")
    (let [[_ bindings & body] (:children node)
          body-ctx (reduce (fn [c [p ty]]
                             (record! ctx p ty)
                             (bind-pattern c p ty))
                           ctx (map vector (:children bindings) (:bindings contract)))
          actual (infer-body body-ctx body)]
      (check-return! body-ctx (or (last body) node) actual (:ret contract))))
  ;; A trusted body contract says nothing about the expansion's expression type.
  t/unknown)

(defn- infer-list [ctx node]
  (let [h (first (:children node))
        args (args-of node)
        file (:file ctx)]
    (cond
      (nil? h) {:t :coll :kind :seq :of t/never}
      (= :kw (:op h)) (let [[m default] args
                            ty (infer ctx m)]
                        (if default
                          (read-key-default ctx ty (:val h) h (infer ctx default))
                          (read-key ctx ty (:val h) h)))
      (not= :sym (:op h)) (do (infer ctx h) (run! #(infer ctx %) args) t/unknown)
      (contains? (:locals ctx) (:val h)) (let [ft (get-in ctx [:locals (:val h)])]
                                           (if (or (= :all (:t ft)) (seq (:arities ft)))
                                             (call-fn ctx node (str (:val h)) ft args)
                                             (do (run! #(infer ctx %) args) t/unknown)))
      :else
      (let [sym (:val h) n (name sym) head (env/head file node)
            own? (and (nil? (namespace head)) (contains? (get-in (:env ctx) [:ns-defs (:ns file)]) head))
            core-sym (if (namespace head) head (symbol "clojure.core" (name head)))
            extern? (= :extern (get-in ctx [:env :vars core-sym :source]))
            has-rule? (and (not own?) (not extern?) (not= (get-method special head) (get-method special ::none)))
            ;; An unqualified, unreferred head names a contract only when this
            ;; namespace itself defines that macro: never a function or core form.
            local-sym (symbol (str (:ns file)) (name head))
            contract-sym (cond (namespace head) head
                               (contains? (:macros (:env ctx)) local-sym) local-sym)
            contract (when contract-sym (get-in ctx [:env :macro-contracts contract-sym]))
            r (cond contract (check-macro-body ctx node contract)
                    has-rule? (special ctx node head)
                    ;; a macro of Clojure itself without a rule yet: its body is not checked
                    (and (not own?) (not extern?) (core/stdlib-macro? core-sym)) t/unknown
                    :else ::none)]
        (cond
          (not= ::none r) r
          (and (nil? (namespace sym)) (str/starts-with? n ".-")) (do (run! #(infer ctx %) args) t/unknown)
          (and (nil? (namespace sym)) (str/starts-with? n ".") (> (count n) 1) (not= ".." n)) (method-call ctx node (subs n 1))
          (and (nil? (namespace sym)) (str/ends-with? n ".") (> (count n) 1))
          (do (run! #(infer ctx %) args)
              (if-let [c (t/resolve-class (:imports file) (symbol (subs n 0 (dec (count n)))))]
                (if (= c "java.lang.Object") {:t :class :name c} (t/class-type c)) t/unknown))
          (java-static? ctx sym) (java-call ctx node (java-static? ctx sym) n true args)
          (contains? (:macros (:env ctx)) (:sym (resolve-var (assoc ctx :quiet true) sym h)))
          (do (report! ctx h "TSP7031" (str "macro " sym " is not described: its body is not checked") :severity :warning)
              t/unknown)
          :else
          (let [{:keys [type] full :sym} (resolve-var ctx sym h)]
            (if type
              (call-fn ctx node (str sym) type args)
              (do (run! #(infer ctx %) args) t/unknown))))))))

(defn- infer* [ctx node]
  (case (:op node)
    :str (t/lit (:val node))
    :char (t/class-type "java.lang.Character")
    :num (t/lit (:val node))
    :kw (t/lit (:val node))
    :bool (t/lit (:val node))
    :nil t/nil-t
    :regex (t/class-type "java.util.regex.Pattern")
    :sym (let [s (:val node)]
           (cond (contains? (:locals ctx) s) (get-in ctx [:locals s])
                 (java-static? ctx s) t/unknown
                 (and (nil? (namespace s)) (t/resolve-class (:imports (:file ctx)) s)) t/unknown
                 :else (or (:type (resolve-var ctx s node)) t/unknown)))
    :vector {:t :coll :kind :vector :of (apply t/union (map #(widen (dissoc (infer ctx %) :fresh)) (:children node)))}
    :set {:t :coll :kind :set :of (apply t/union (map #(widen (infer ctx %)) (:children node)))}
    :map (let [pairs (partition 2 (:children node))]
           (if (every? #(= :kw (:op (first %))) pairs)
             {:t :map :fresh true :inferred true :opt {}
              :req (into (array-map)
                         (for [[k v] pairs]
                           (let [vty (infer ctx v) kk (:val k)]
                             (check-key-value! ctx kk v vty)
                             (when (and (qualified-keyword? kk) (not (get-in (:env ctx) [:specs kk]))
                                        (contains? (:key-nss (:env ctx)) (namespace kk))
                                        (or (:strict (:opts ctx)) (similar kk (keys (:specs (:env ctx))))))
                               (report! ctx k "TSP2304" (str "no spec for " kk (hint-suffix kk (keys (:specs (:env ctx)))))))
                             ;; a wrong value is reported once; after that the key has the type of its spec
                             [kk (if (and (qualified-keyword? kk) (get-in (:env ctx) [:specs kk])
                                          (not (t/sub? (:env ctx) (eff ctx vty) (t/ref-t kk))))
                                   (t/ref-t kk)
                                   (dissoc vty :fresh))])))}
             (do (run! #(infer ctx %) (:children node)) (t/prim :map))))
    :quote (if (= :sym (:op (first (:children node)))) (t/prim :symbol) t/unknown)
    :fn-lit (do (infer (reduce #(bind %1 %2 t/unknown) (dissoc ctx :recur-types) '[% %1 %2 %3 %&])
                       {:op :list :children (:children node) :pos (:pos node)})
                {:t :fn})
    :list (infer-list ctx node)
    t/unknown))

(defn infer [ctx node]
  (let [dirs (:directives node)
        dhead (fn [d] (:val (first (:children d))))
        find-d (fn [h] (first (filter #(= h (dhead %)) dirs)))
        ty (infer* ctx node)
        ty (if-let [h (and (not= :sym (:op node)) (env/hint-type (:file ctx) node))]
             (do (check-hint! ctx node ty "type of the expression")
                 (if-let [why (:null-return (meta ty))]
                   (with-meta (t/union (dissoc h :platform) t/nil-t) {:null-return why})
                   h))
             ty)
        ty (cond
             (find-d 't/cast) (do (swap! *trust* update :cast inc) (parse-directive-type ctx (find-d 't/cast)))
             (find-d 't/any) (do (swap! *trust* update :any inc) t/any)
             :else ty)]
    (record! ctx node ty)))

;; ---- entry points ----

(def ^:private known-diagnostic-codes
  #{"TSP270-01" "TSP270-02" "TSP270-03"
    "TSP2304" "TSP2305" "TSP2322" "TSP2339" "TSP2345" "TSP2353"
    "TSP2531" "TSP2554" "TSP2578" "TSP2678" "TSP2741" "TSP2790"
    "TSP7010" "TSP7020" "TSP7021" "TSP7023" "TSP7025" "TSP7031" "TSP7032"
    "TSP8001" "TSP9001" "TSP9002" "TSP9003" "TSP9004" "TSP830-01" "TSP210-01"})

(defn- directive-children
  "Directive scope follows source code, not quoted data or opaque declarations.
  Keep directives on the boundary form itself, but do not inspect its contents.
  Ordinary collection expressions and function binding forms remain traversable."
  [file node]
  (when-not (or (= :quote (:op node))
                (and (= :list (:op node))
                     (contains? '#{quote ns comment declare require import in-ns defmacro
                                   defprotocol defrecord deftype defmulti definterface set! var
                                   clojure.spec.alpha/def clojure.spec.alpha/fdef}
                                (env/head file node))))
    (:children node)))

(defn- collect-ignores
  "Collect once from source AST, excluding data and opaque declaration contents.
  Invalid directives never enter matching and cannot themselves be ignored."
  [files]
  (reduce
    (fn [acc [file node d]]
      (let [args (rest (:children d))
            broad? (and (= 1 (count args)) (= :str (:op (first args))))
            code-node (when-not broad? (first args))
            code (when (= :sym (:op code-node)) (str (:val code-node)))
            reason (if broad? (first args) (second args))
            problem (cond
                      (:missing-directive-target node) "t/ignore has no following form"
                      (not (or broad? (= 2 (count args)))) "t/ignore requires [CODE] and a nonblank string reason"
                      (or (not= :str (:op reason)) (str/blank? (:val reason))) "t/ignore requires a nonblank string reason"
                      (and (not broad?) (not (known-diagnostic-codes code))) (str "unknown t/ignore code: " (parse/unparse code-node)))
            entry {:file file :pos (:pos node) :at (:pos d) :code code}]
        (-> acc
            (update :count inc)
            (update (if problem :invalid :valid) conj
                    (if problem
                      {:code "TSP830-01" :severity :error :file file :pos (:pos d) :message problem}
                      entry)))))
    {:count 0 :valid [] :invalid []}
    (for [f files form (:forms f)
          node (tree-seq #(seq (directive-children f %)) #(directive-children f %) form)
          d (:directives node)
          :when (= 't/ignore (:val (first (:children d))))]
      [(:file f) node d])))

(defn- inside? [pos outer]
  (let [start (juxt :row :col) end (juxt :end-row :end-col)]
    (and (every? number? (concat (start pos) (end pos) (start outer) (end outer)))
         (not (neg? (compare (start pos) (start outer))))
         (neg? (compare (start pos) (end outer)))
         (not (pos? (compare (end pos) (end outer)))))))

(defn- diag-id [d]
  (let [identity [(:code d) (str (:file d))
                  (mapv #(get (:pos d) %) [:row :col :end-row :end-col])
                  (:message d)]
        digest (.digest (java.security.MessageDigest/getInstance "SHA-256")
                        (.getBytes (pr-str identity) java.nio.charset.StandardCharsets/UTF_8))]
    ;; 128 bits of SHA-256: stable across runs, with practical collision resistance.
    (apply str (map #(format "%02x" (bit-and 0xff %)) (take 16 digest)))))

(defn check-files
  "Checks parsed files. Returns {:diagnostics [...] :types [...] :trust {...} :env env}."
  [files opts]
  (let [[env0 env-diags] (env/build files (:extern-contracts opts))
        compiled (macros/compile-contracts
                  (get opts :macro-contracts {}) env0
                  #(not= (get-method special %) (get-method special ::none)))
        env-diags (concat env-diags (:diagnostics compiled))
        env (assoc env0 :macro-contracts (:contracts compiled)
                   :nss (set (map :ns files))
                   :ns-defs (into {} (for [f files]
                                       [(:ns f) (set (for [form (:forms f)
                                                           :when (= :list (:op form))
                                                           :let [[h nm] (:children form)]
                                                           :when (and (= :sym (:op h)) (= :sym (:op nm))
                                                                      (str/starts-with? (name (:val h)) "def"))]
                                                       (:val nm)))])))]
    (binding [*diags* (atom (vec (map #(merge {:severity :error} %) env-diags)))
              *types* (atom [])
              *ignores* (atom [])
              *trust* (atom {:cast 0 :any 0 :ignore 0})
              *rets* (atom {})]
      (doseq [f files]
        (binding [t/*cur-ns* (:ns f)]
          (let [ctx {:env env :file f :opts opts :locals {}}]
            (doseq [form (:forms f)] (infer ctx form)))))
      (let [{:keys [valid invalid count]} (collect-ignores files)
            ;; Inner ranges win; equal ranges retain source directive order.
            ignores (sort-by (fn [i] [(- (or (:row (:pos i)) 0)) (- (or (:col (:pos i)) 0))
                                     (:end-row (:pos i)) (:end-col (:pos i))]) valid)
            _ (swap! *trust* assoc :ignore count)
            hit (fn [d] (first (filter #(and (= (:file %) (:file d))
                                             (or (nil? (:code %)) (= (:code %) (:code d)))
                                             (inside? (:pos d) (:pos %))) ignores)))
            all (distinct @*diags*)
            used (set (keep hit all))
            unused (for [i ignores :when (not (used i))]
                     {:code "TSP2578" :severity :warning :file (:file i) :pos (:at i) :message "unused t/ignore: nothing to ignore here"})
            levels (:levels opts)
            ds (->> (concat (remove hit all) invalid unused)
                    (map #(if-let [l (get levels (symbol (:code %)))] (assoc % :severity l) %))
                    (remove #(= :off (:severity %)))
                    (map #(assoc % :id (diag-id %)))
                    (sort-by (juxt :file #(:row (:pos %)) #(:col (:pos %)) :code)))]
        {:diagnostics (vec ds) :types @*types* :trust @*trust* :env env
         :sigs (into {} (for [[sym v] (:vars env)]
                          [sym (var-type {:env env :opts opts} sym)]))}))))
