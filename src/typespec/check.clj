(ns typespec.check
  "The checker: walks forms, infers types, reports diagnostics."
  (:require [clojure.string :as str]
            [clojure.walk :as walk]
            [typespec.parse :as parse]
            [typespec.types :as t]
            [typespec.length :as length]
            [typespec.literals :as literals]
            [typespec.type-operators :as operators]
            [typespec.spec-conform :as conform]
            [typespec.solver :as solver]
            [typespec.env :as env]
            [typespec.java :as java]
            [typespec.core-types :as core]
            [typespec.guards :as guards]
            [typespec.macro-contracts :as macros])
  (:import [clojure.lang Reflector]
           [java.lang.reflect Modifier]))

(def ^:dynamic *diags* nil)
(def ^:dynamic *types* nil)
(def ^:dynamic *var-uses* nil)
(def ^:dynamic *spec-uses* nil)
(def ^:dynamic *ignores* nil)
(def ^:dynamic *trust* nil)
(def ^:dynamic *rets* nil)

(def ^:private map-path-budget 256)
(def ^:private empty-map-shape {:t :map :req {} :opt {} :inferred true :complete-keys? true})

(declare infer infer-body bind-pattern apply-fn-node why-of var-type infer-fn narrow record! force-callback fn-bodies
         assoc-map-key dissoc-map-key specialize-map-return dynamic-definition? read-map-of literal-element-type
         finite-map-keys array-component array-read-type append-to-vectors checked-vector-into
         unsupported-sequence-member specialize-call-return run-composition map-entry-sequence)

(defn- static-map-key [node]
  (case (:op node)
    (:kw :str) (:val node)
    :quote (let [value (first (:children node))]
             (when (= :sym (:op value)) (:val value)))
    :list (let [[head value & more] (:children node)]
            (when (and (= 'quote (:val head)) (= :sym (:op value)) (empty? more))
              (:val value)))
    nil))

(defn- literal-node? [node]
  (and (#{:str :kw :num :bool :nil} (:op node)) (empty? (:directives node))
       (not (env/tag-node node))))

(defn- stable-map-key [node]
  (when (and (empty? (:directives node)) (not (env/tag-node node)))
    (static-map-key node)))

(defn- selector-literal-key [member]
  (when (= :lit (:t member))
    (let [value (:val member)]
      (when (or (keyword? value) (string? value) (symbol? value)) value))))

(defn- quoted-map-key-type [node ty]
  (let [key (when (= (t/prim :symbol) ty) (static-map-key node))]
    (if (symbol? key) (t/with-assumption (t/lit key) (t/assumption ty)) ty)))

(defn- at-type-path [node path]
  (reduce (fn [current step]
            (if-let [child (case (:op current)
                             :map (some (fn [[k v]] (when (= step (static-map-key k)) v))
                                        (partition 2 (:children current)))
                             :vector (when (integer? step) (get (:children current) step))
                             nil)]
              child
              (reduced current)))
          node path))

(defn- report! [ctx node code msg & {:as extra}]
  (when-not (:quiet ctx)
    (let [at (at-type-path node (:type-path extra))]
      (swap! *diags* conj (merge {:code code :severity :error :file (:file (:file ctx)) :pos (:pos at) :message msg} extra))))
  nil)

(defn- eff
  "With :strict-nil, a platform type (from a hint or from Java) may be nil."
  [ctx ty]
  (if (:strict-nil (:opts ctx)) (t/allow-platform-nil ty) ty))

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
  (let [named? #(or (string? %) (instance? clojure.lang.Named %))]
    (when (named? x)
      (let [norm #(str/replace (str/lower-case (str %)) #"[-_]" "")
            sx (str x)
            nx (norm sx)]
        (->> candidates
             (filter named?)
             (remove #(= (str %) sx))
             (map (fn [c] [(if (= (norm c) nx) 0 (edit-distance sx (str c))) c]))
             (filter (fn [[d c]]
                       (<= d (max 1 (min 2 (quot (count (if (string? c) c (name c))) 2))))))
             (sort-by first) first second)))))

(defn- hint-suffix [x candidates]
  (if-let [c (similar x candidates)] (str ". Did you mean " c "?") ""))

;; ---- maps and keys ----

(defn- map-members [ctx ty]
  (filter #(= :map (:t %)) (t/members (:env ctx) ty)))

(defn- indexed-map-entry [env index k]
  (if-let [value (t/map-of-entry env index k)] [:opt value] [:absent nil]))

(defn- lookup-key [ctx ty k & [path?]]
  (let [env (:env ctx)
        ms (remove #(= :nil (:t %)) (t/members env ty))
        spec (when (and (qualified-keyword? k) (get-in env [:specs k])) (t/ref-t k))
        loose [:unknown (or spec t/unknown)]
        one (fn [m]
              (case (:t m)
                :map (cond (contains? (:req m) k) [:req (get (:req m) k)]
                           (contains? (:opt m) k) [:opt (get (:opt m) k)]
                           (:map-index m) (indexed-map-entry env (:map-index m) k)
                           (:complete-keys? m) [:absent nil]
                           spec [:opt spec]
                           ;; a map whose type was inferred (a literal, a pattern) may hold more keys than we saw
                           (:inferred m) loose
                           :else [:missing nil])
                :map-of (indexed-map-entry env m k)
                :coll (cond (and path? (= :set (:kind m))) [:opt (t/lit k)]
                            (and path? (nil? (:kind m))) loose
                            :else [:not-map nil])
                (:unknown :any :class :var :rigid :ivar) loose
                :prim (if (or (= :map (:name m)) (and path? (= :coll (:name m))))
                        loose
                        (if (and path? (= :set (:name m))) [:opt (t/lit k)] [:not-map nil]))
                [:not-map nil]))
        rs (map one ms)
        ty-of #(t/with-assumption (apply t/union (keep second rs)) (t/assumption ty))]
    (cond (= :never (:t ty)) {:status :req :type t/never}
          (empty? ms) (if path? {:status :absent} {:status :nil :type t/nil-t})
          (or (every? #(= :absent (first %)) rs)
              (and path? (every? #(#{:absent :not-map} (first %)) rs))) {:status :absent}
          (and (not path?) (some #(= :not-map (first %)) rs)) {:status :not-map}
          (some #(= :missing (first %)) rs) {:status :missing}
          (every? #(= :req (first %)) rs) {:status :req :type (ty-of)}
          (every? #(= :unknown (first %)) rs) {:status :unknown :type (ty-of)}
          :else {:status :opt :type (ty-of)})))

(defn- key-info
  "{:type :optional? :declared?} for reading key k from a value of type ty; reports unknown keys at node."
  [ctx ty k node & [path?]]
  (let [env (:env ctx)
        {:keys [status type]} (lookup-key ctx ty k path?)
        spec-keys (keys (:specs env))]
    (case status
      :req {:type type :optional? (t/nilable? env ty) :declared? true :status :req}
      :opt {:type type :optional? true :declared? true}
      :nil {:type t/never :optional? true :declared? true}
      :absent {:type t/never :optional? true :declared? true}
      :unknown (do (when (and (qualified-keyword? k) (not (get-in env [:specs k]))
                              (contains? (:key-nss env) (namespace k))
                              (or (:strict (:opts ctx)) (similar k spec-keys)))
                     (report! ctx node "TSP2304" (str "no spec for " k (hint-suffix k spec-keys))))
                   ;; nothing is known about the map: nil is not checked, as for other platform types
                   {:type (if (#{:unknown :any} (:t type)) type (assoc type :platform true))
                    :optional? (boolean path?) :declared? false})
      :missing (do (report! ctx node "TSP2339"
                            (str k " is not a key of " (t/show (t/remove-nil env ty))
                                 (hint-suffix k (concat (mapcat #(concat (keys (:req %)) (keys (:opt %))) (map-members ctx ty))
                                                        (when (qualified-keyword? k) spec-keys)))))
                   {:type t/unknown :optional? false :declared? false :invalid? true})
      :not-map (do (report! ctx node "TSP2339" (str "cannot read " k " from " (t/show ty)))
                   {:type t/unknown :optional? false :declared? false :invalid? true}))))

(defn- read-key
  ([ctx ty k node] (read-key ctx ty k node t/unknown))
  ([ctx ty k node invalid-result]
   (read-key ctx ty k node t/nil-t invalid-result))
  ([ctx ty k node default-type invalid-result]
   (let [{:keys [type optional? invalid?]} (key-info ctx ty k node)]
     ;; A default replaces absence, never a nil value stored at the key.
     (cond invalid? invalid-result
           optional? (t/union type default-type)
           :else type))))

;; ---- assignability ----

(defn- incomplete-problem [ctx actual expected result]
  ["TSP210-01"
   (str "compatibility not checked: " (show-ref ctx actual) " to " (show-ref ctx expected)
        " (" (str/join ", " (sort (map name (:reasons result)))) ")")
   {:severity (if (:strict (:opts ctx)) :error :warning)
    :reasons (:reasons result)}])

(defn- length-message [actual expected]
  (str "expected collection length " (length/describe expected) ", got " (length/describe actual)))

(defn- nested-type-problem [ctx code {:keys [path missing-key actual expected length?]}]
  (when (seq path)
    [(cond missing-key "TSP2741" length? "TSP330-01" :else code)
     (str "at " (pr-str path) ": "
          (cond missing-key (str "required key " missing-key " is missing")
                length? (length-message actual expected)
                :else (str (show-ref ctx actual) " is not assignable to " (show-ref ctx expected))))
     {:type-path path}]))

(defn- nested-platform-problem [ctx code actual expected result]
  (when-let [{:keys [path platform-origin expected]}
             (t/nested-platform-mismatch (:env ctx) actual expected result)]
    [code (str "at " (pr-str path) ": " (show-ref ctx platform-origin)
               " may be nil, expected " (show-ref ctx expected))
     {:type-path path}]))

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
             (conj (incomplete-problem ctx v exp result) [:value k])
             ["TSP2322" (str "value of " k ": " (t/show (eff ctx v)) " is not assignable to " (show-ref ctx exp)) nil [:value k]]))
         ;; An open inferred shape only describes the keys we have seen.
         (when (or (not (:inferred m)) (:complete-keys? m))
           (for [k (keys (:req actual))
                 :when (and (not (some #{k} known))
                            (not (and (qualified-keyword? k) (get-in env [:specs k])))
                            ;; a qualified key with no spec is already reported as TSP2304
                            (not (and (qualified-keyword? k) (contains? (:key-nss env) (namespace k))))
                            (or (qualified-keyword? k) (similar k known)))]
             ["TSP2353" (str "unknown key " k " in a literal of " (t/show expected) (hint-suffix k known)) nil [:key k]])))))))

(defn- missing-req
  "[key optional-there?] when `actual` is a map type that lacks a required key of `expected`."
  [ctx actual expected]
  (let [env (:env ctx)
        am (remove #(= :nil (:t %)) (t/members env actual))
        em (remove #(= :nil (:t %)) (t/members env expected))]
    (when (and (= 1 (count am) (count em)) (= :map (:t (first am)) (:t (first em))) (not (t/nilable? env actual)))
      (when-let [k (first (remove #(contains? (:req (first am)) %) (keys (:req (first em)))))]
        [k (contains? (:opt (first am)) k)]))))

(defn- collection-problem [ctx actual expected]
  (let [env (:env ctx)
        am (t/members env actual)
        em (t/members env expected)
        source (first am)
        target (first em)
        a (:items source)
        e (:items target)]
    (when (= 1 (count am) (count em))
      (cond
        (and a e (= :vector (:kind source) (:kind target)))
        (if (not= (count a) (count e))
          ["TSP330-01" (str "expected vector length " (count e) ", got " (count a))]
          (first (keep-indexed (fn [i item]
                                (let [target (nth e i)]
                                  (when (= :mismatch (:status (t/sub-result env item target)))
                                    ["TSP2345" (str "vector element " i ": " (show-ref ctx (eff ctx item))
                                                     " is not assignable to " (show-ref ctx target))])))
                              a)))

        (and (= :coll (:t source) (:t target))
             (or (nil? (:kind target)) (= (:kind source) (:kind target)))
             (or (length/constrained? target) (and e (length/constrained? source)))
             (not (length/subset? source target)))
        ["TSP330-01" (length-message source target)]))))

(defn- problems
  "Why a value of type `actual` (written as `node`) cannot be used as `expected`. Empty when it can."
  [ctx node actual expected]
  (let [env (:env ctx) actual (eff ctx actual)
        result (t/sub-result env actual expected)
        nested-error (when (= :mismatch (:status result))
                       (or (nested-platform-problem ctx "TSP2531" actual expected result)
                           (nested-type-problem ctx (if (:fresh actual) "TSP2322" "TSP2345") result)))
        collection-error (when (= :mismatch (:status result)) (collection-problem ctx actual expected))]
    (cond
      (:generic-failure actual) []
      (= :proved (:status result)) (if (:fresh actual) (filter #(= "TSP2353" (first %)) (fresh-problems ctx actual expected)) [])
      nested-error [nested-error]
      (seq (fresh-problems ctx actual expected)) (fresh-problems ctx actual expected)
      (= :inconclusive (:status result)) [(incomplete-problem ctx actual expected result)]
      collection-error [collection-error]
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

(defn- bind [ctx sym ty & {:keys [why hinted param compiler-class]}]
  (-> ctx
      (assoc-in [:locals sym] ty)
      (update :why #(if why (assoc % sym why) (dissoc % sym)))
      (update :hinted #(if hinted (conj (or % #{}) sym) (disj (or % #{}) sym)))
      (update :compiler-local-classes #(if compiler-class (assoc % sym compiler-class) (dissoc % sym)))
      (update :params #(if param (conj (or % #{}) sym) (disj (or % #{}) sym)))
      (assoc-in [:binding-ids sym] (Object.))
      (update :projections dissoc sym)
      (update :conditions dissoc sym)
      (update :contextual-params dissoc sym)
      (update :missing-of dissoc sym)))

(defn- plain-local [ctx node]
  (when (and (= :sym (:op node)) (empty? (:directives node))
             (contains? (:locals ctx) (:val node)))
    (:val node)))

(defn- active-condition [ctx local]
  (when-let [{:keys [bindings] :as condition} (get-in ctx [:conditions local])]
    (when (every? (fn [[sym id]] (= id (get-in ctx [:binding-ids sym]))) bindings)
      condition)))

(defn- builtin-head [ctx node]
  (let [head (env/head (:file ctx) node)
        raw (:val (first (:children node)))
        ns-sym (:ns (:file ctx))
        core-sym (when head (if (namespace head) head (symbol "clojure.core" (name head))))]
    (when (and head (empty? (:directives node)) (not (env/tag-node node))
               (not (contains? (:locals ctx) raw))
               (not (and core-sym (contains? (get-in ctx [:env :ns-defs ns-sym]) head)))
               (not= :extern (get-in ctx [:env :vars core-sym :source])))
      head)))

(defn- guard-symbol [ctx node]
  (when (and (= :sym (:op node)) (not (contains? (:locals ctx) (:val node))))
    (let [file (:file ctx)
          resolved (env/resolve-sym file (:val node))
          local (symbol (str (:ns file)) (name resolved))
          sym (cond (namespace resolved) resolved
                    (or (get-in ctx [:env :vars local])
                        (contains? (get-in ctx [:env :ns-defs (:ns file)]) resolved)) local
                    :else (symbol "clojure.core" (name resolved)))]
      (when-not (and (or (= "clojure.core" (namespace sym)) (= 'clojure.spec.alpha/valid? sym))
                     (= :extern (get-in ctx [:env :vars sym :source])))
        sym))))

(defn- guard-call [ctx node]
  (when (and (not (:guards-disabled? ctx))
             (= :list (:op node)) (= 2 (count (:children node)))
             (empty? (:directives node)) (not (env/tag-node node)))
    (get-in ctx [:env :guards (guard-symbol ctx (first (:children node)))])))

(defn- guard-compiler [ctx]
  {:resolve-head #(when (= :list (:op %)) (guard-symbol ctx (first (:children %))))
   :class-name #(when-let [name (t/resolve-class (:imports (:file ctx)) %)]
                  (when-let [class (t/load-class name)]
                    (when-not (.isPrimitive ^Class class) name)))
   :macro? #(and % (or (contains? (get-in ctx [:env :macros]) %)
                       (contains? (get-in ctx [:env :macro-contracts]) %)
                       (core/stdlib-macro? %)))
   :callable? #(contains? (get-in ctx [:env :vars]) %)})

(defn- remember-projection [ctx sym origin path]
  (assoc-in ctx [:projections sym]
            {:origin origin :path path :type (get-in ctx [:locals sym])}))

(declare projected-type)

(defn- map-entry-type? [ctx ty]
  (every? :map-entry? (t/members (:env ctx) ty)))

(defn- single-member [ctx ty]
  (let [[member & more] (t/members (:env ctx) (eff ctx ty))]
    (when-not (seq more) member)))

(defn- stable-local-member [ctx node]
  (when-let [local (when-not (env/tag-node node) (plain-local ctx node))]
    (single-member ctx (get-in ctx [:locals local]))))

(defn- stable-selector-value [ctx node]
  (if (literal-node? node)
    (:val node)
    (let [member (stable-local-member ctx node)]
      (when (= :lit (:t member)) (:val member)))))

(defn- stable-lookup-key [ctx node]
  (or (stable-map-key node)
      (selector-literal-key (stable-local-member ctx node))))

(defn- nil-lookup-default? [ctx node]
  (or (nil? node)
      (and (= :nil (:op node)) (literal-node? node))
      (= :nil (:t (stable-local-member ctx node)))))

(defn- lookup-index [^Number key]
  ;; Clojure vector and string lookup uses Number.intValue, including wraparound.
  (.intValue key))

(defn- checked-int-index [value]
  (try (int value)
       (catch IllegalArgumentException _ nil)
       (catch ArithmeticException _ nil)))

(defn- nth-lookup-step [ctx node default]
  (let [value (stable-selector-value ctx node)
        index (when (number? value) (checked-int-index value))]
    (when (and index (not (neg? index)) (nil-lookup-default? ctx default))
      (if default {:vector-index index} index))))

(defn- vector-lookup-step [value]
  (when (integer? value)
    (let [index (lookup-index value)]
      (when-not (neg? index) {:vector-index index}))))

(defn- selector-path-step [member]
  (when (= :lit (:t member))
    (or (selector-literal-key member) (vector-lookup-step (:val member)))))

(defn- stable-lookup-step [ctx node]
  (or (stable-map-key node)
      (if (literal-node? node)
        (vector-lookup-step (:val node))
        (selector-path-step (stable-local-member ctx node)))))

(defn- stable-lookup-path [ctx node]
  (when (and (empty? (:directives node)) (not (env/tag-node node)))
    (case (:op node)
      :vector (mapv #(stable-lookup-step ctx %) (:children node))
      :nil []
      (when-let [member (stable-local-member ctx node)]
        (if (= :nil (:t member))
          []
          (when-let [items (:items member)]
            (mapv #(selector-path-step (single-member ctx %)) items)))))))

(defn- optional-position-index [step]
  (or (:vector-index step) (:seq-index step)))

(defn- position-step? [step]
  (or (integer? step) (some? (optional-position-index step))))

(defn- optional-step-presence [member step]
  (when-let [index (optional-position-index step)]
    (when (or (= :vector (:kind member))
              (and (:seq-index step) (= :seq (:kind member))))
      (length/position member index))))

(defn- projected-position [member step]
  (if-let [index (optional-position-index step)]
    (when (#{:present :possible} (optional-step-presence member step))
      (if-let [items (:items member)] (get items index) (:of member)))
    (get (:items member) step)))

(defn- missing-optional-position? [member step]
  (and (optional-position-index step)
       (or (= :nil (:t member)) (= :absent (optional-step-presence member step)))))

(defn- collection-with-position [member index]
  (let [[lo hi] (length/bounds member)]
    (length/with-bounds member (max lo (inc index)) hi)))

(defn- collection-without-position [member index]
  (let [[lo hi] (length/bounds member)]
    (length/with-bounds member lo (if hi (min hi index) index))))

(defn- stable-path [ctx node]
  (when (and (empty? (:directives node)) (not (env/tag-node node)))
    (if-let [sym (plain-local ctx node)]
      (let [ty (get-in ctx [:locals sym])
            {:keys [origin path type]} (get-in ctx [:projections sym])]
        (if (and origin (= type ty))
          {:origin origin :path path :type (get-in ctx [:origins origin])}
          {:local sym :path [] :type ty}))
      (when (= :list (:op node))
        (let [[head a b default & extra] (:children node)
              h (builtin-head ctx node)
              lookup-step (when (and (= 'get h) (empty? extra) (nil-lookup-default? ctx default))
                            (stable-lookup-step ctx b))
              map-path (when (and (= 'get-in h) (empty? extra)) (stable-lookup-path ctx b))
              nth-step (when (and (= 'nth h) (empty? extra)) (nth-lookup-step ctx b default))
              [base path] (cond
                            (and (= :kw (:op head)) a (nil? default) (nil-lookup-default? ctx b)) [a [(:val head)]]
                            lookup-step [a [lookup-step]]
                            (and (some? map-path)
                                 (or (empty? map-path)
                                     (and (nil-lookup-default? ctx default) (every? some? map-path)))) [a map-path]
                            (and (#{'first 'second} h) a (nil? b))
                            [a [{:seq-index (if (= 'first h) 0 1)}]]
                            (and (#{'key 'val} h) a (nil? b))
                            [a [(if (= 'key h) 0 1)]]
                            nth-step [a [nth-step]])]
          (when-let [p (and base (stable-path ctx base))]
            (when (or (not (integer? (first path)))
                      (let [base-type (projected-type ctx (:type p) (:path p))]
                        (and (or (not (#{'key 'val} h)) (map-entry-type? ctx base-type))
                             (projected-type ctx base-type path))))
              (update p :path into path))))))))

(defn- track-path [ctx {:keys [local origin path type]}]
  (if origin
    [ctx origin path]
    (let [origin (Object.)
          ctx (assoc-in ctx [:origins origin] type)]
      [(cond-> ctx local (remember-projection local origin [])) origin path])))

(defn- projected-type [ctx ty path]
  (reduce (fn [current key]
            (if (position-step? key)
              (let [positions (for [member (t/members (:env ctx) current)]
                                (if-let [field (projected-position member key)]
                                  (if (= :possible (optional-step-presence member key))
                                    (t/union field t/nil-t)
                                    field)
                                  (when (missing-optional-position? member key) t/nil-t)))]
                (if (every? some? positions) (apply t/union positions) (reduced nil)))
              (let [{:keys [type status]} (lookup-key ctx current key)]
                (case status
                  :req (if (t/nilable? (:env ctx) current) (t/union type t/nil-t) type)
                  :opt (t/union type t/nil-t)
                  (:nil :absent) t/nil-t
                  (reduced nil)))))
          ty path))

(defn- required-map-projection? [ctx ty path]
  (when (and (seq path) (not (position-step? (peek path))))
    (when-let [parent (projected-type ctx ty (pop path))]
      (and (= :req (:status (lookup-key ctx parent (peek path))))
           (not (t/nilable? (:env ctx) parent))))))

(defn- refine-origin [ctx {:keys [local origin]} ty]
  (when-not (= :never (:t ty))
    (if origin
      (reduce-kv (fn [c sym {:keys [path type] :as p}]
                   (if (and (= origin (:origin p)) (= type (get-in ctx [:locals sym])))
                     (if-let [narrowed (projected-type c ty path)]
                       (cond-> (-> c (assoc-in [:locals sym] narrowed) (remember-projection sym origin path))
                         (and (get-in c [:why sym]) (t/nilable? (:env c) narrowed)
                              (required-map-projection? c ty path))
                         (update :why dissoc sym))
                       c)
                     c))
                 (assoc-in ctx [:origins origin] ty) (:projections ctx))
      (assoc-in ctx [:locals local] ty))))

(defn- optional-map-value [env shape key]
  (or (get-in shape [:opt key])
      (when-let [index (:map-index shape)] (or (t/map-of-entry env index key) t/never))
      (when (:complete-keys? shape) t/never)))

(defn- indexed-map-shape [index]
  (t/with-assumption (assoc empty-map-shape :complete-keys? false :map-index index)
                     (t/assumption index)))

(defn- map-index-domain [shape]
  (when-let [index (:map-index shape)]
    (let [entries (remove #(= t/never (val %)) (concat (:req shape) (:opt shape)))]
      (t/with-assumption
       {:t :map-of
        :k (apply t/union (:k index) (map (comp t/lit key) entries))
        :v (apply t/union (:v index) (map val entries))}
       (t/assumption shape)))))

(defn- position-alternatives [member step descend]
  (if-let [field (projected-position member step)]
    (when-let [children (descend field)]
      (let [index (or (optional-position-index step) step)
            present (for [[leaf rebuild] children]
                      [leaf (if-let [items (:items member)]
                              (fn [part]
                                (let [items (assoc items index (rebuild part))]
                                  (assoc member :items items :of (apply t/union items))))
                              ;; The test only describes this element of a homogeneous collection.
                              (constantly (collection-with-position member index)))])]
        (if (= :possible (optional-step-presence member step))
          (cons [t/nil-t (constantly (collection-without-position member index))] present)
          present)))
    (when (missing-optional-position? member step)
      [[t/nil-t (constantly member)]])))

(defn- path-alternatives
  "Pairs of [leaf type, rebuild]. Each rebuild retains its enclosing map or tuple."
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
                           optional (fn [m]
                                      (when-let [field (optional-map-value env m key)]
                                        (if (= t/never field)
                                          [[t/nil-t (constantly m)]]
                                          (when-let [present (required (update m :opt dissoc key) field)]
                                            (cons [t/nil-t (constantly (assoc-in m [:opt key] t/never))]
                                                  present)))))
                           paths (for [m members]
                                   (cond
                                     (position-step? key)
                                     (position-alternatives m key #(walk % more))

                                     (get-in m [:req key]) (required m (get-in m [:req key]))
                                     (and presence? (= :map (:t m))) (optional m)
                                     (and presence? (= :map-of (:t m))) (optional (indexed-map-shape m))
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
  (let [members (t/members env ty)
        opaque? #(#{:unknown :any :var :rigid :ivar} (:t %))
        boolean? #(and (= :prim (:t %)) (= :boolean (:name %)))
        false-literal? #(and (= :lit (:t %)) (false? (:val %)))]
    (if (and truthy?
             (not-any? #(or (false-literal? %) (boolean? %) (opaque? %) (= :some (:t %))) members))
      (t/remove-nil env ty)
      (apply t/union
             (for [m members
                   :let [falsy? (or (= :nil (:t m)) (false-literal? m))]
                   :when (or (opaque? m) (boolean? m) (= :some (:t m))
                             (= truthy? (not falsy?)))]
               (cond
                 (boolean? m) (t/lit truthy?)
                 (and (= :some (:t m)) (not truthy?)) (t/lit false)
                 :else m))))))

(defn- nil-part [env ty]
  (if (some t/may-be-nil-leaf? (t/members env ty)) t/nil-t t/never))

(defn- refine-alternatives [ctx path alternatives select]
  (refine-origin ctx path
                 (apply t/union (keep (fn [[leaf rebuild]]
                                        (when-let [narrowed (select leaf)]
                                          (rebuild narrowed)))
                                      alternatives))))

(defn- literal-type [v]
  (if (nil? v) t/nil-t (t/lit v)))

(defn- finite-value? [ty]
  (#{:lit :nil} (:t ty)))

(defn- boolean-type? [ty]
  (= (t/prim :boolean) (t/strip ty)))

(defn- expand-boolean [ty]
  (if (boolean-type? ty) [(t/lit false) (t/lit true)] [ty]))

(defn- found-value-path [ctx node]
  (when (and (= 'val (builtin-head ctx node)) (= 2 (count (:children node))))
    (let [entry (second (:children node))
          local (when-not (env/tag-node entry) (plain-local ctx entry))
          {:keys [head test]} (active-condition ctx local)]
      (when (= 'find head)
        (let [[_ source key-node] (:children test)]
          (when-let [key (stable-lookup-key ctx key-node)]
            (when-let [{:keys [type path] :as subject} (stable-path ctx source)]
              (let [field (conj path key)]
                (when (required-map-projection? ctx type field)
                  (assoc subject :path field))))))))))

(defn- refine-path [ctx node split]
  (let [subject (stable-path ctx node)
        projected-local (when (some optional-position-index (:path subject)) (plain-local ctx node))
        alternatives (when subject (path-alternatives ctx (:type subject) (:path subject) true))
        local (when-not alternatives (plain-local ctx node))
        subject (if local {:local local} subject)
        alternatives (or alternatives (when local (path-alternatives ctx (get-in ctx [:locals local]) [])))]
    (when alternatives
      (let [split-leaves (fn [leaves] (map (fn [[leaf rebuild]] [(split leaf) rebuild]) leaves))
            parts (split-leaves alternatives)
            field (found-value-path ctx node)
            field-parts (when field
                          (when-let [leaves (path-alternatives ctx (:type field) (:path field) true)]
                            (split-leaves leaves)))
            branch (fn [i]
                     (when-let [branch (refine-alternatives ctx subject parts #(nth % i))]
                       (if field-parts
                         (refine-alternatives branch field field-parts #(nth % i))
                         (cond-> branch
                           projected-local
                           (assoc-in [:locals projected-local]
                                     (apply t/union (keep #(nth (first %) i) parts)))))))]
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

(defn- count-parts [size member]
  (case (:t member)
    :never [nil nil]
    :nil (if (zero? size) [member nil] [nil member])
    :coll
    (let [[lo hi] (length/bounds member)]
      (if (or (< size lo) (and hi (> size hi)))
        [nil member]
        (let [shorter (when (< lo size) (length/with-bounds member lo (dec' size)))
              longer (when (or (nil? hi) (< size hi)) (length/with-bounds member (inc' size) hi))]
          [(if (= lo hi) member (length/with-bounds member size size))
           (if (and shorter longer) (t/union shorter longer) (or shorter longer))])))
    [member member]))

(defn- count-source [ctx expr size]
  (let [[source & extra] (rest (:children expr))]
    (when (and (not (:guards-disabled? ctx)) (integer? size)
               (= 'count (builtin-head ctx expr)) source (empty? extra))
      source)))

(defn- count-equality-contexts [ctx expr size]
  (when-let [source (count-source ctx expr size)]
    (refine-path ctx source #(count-parts size %))))

(defn- count-order-parts [cutoff member]
  (case (:t member)
    :never [nil nil]
    :nil (if (pos? cutoff) [member nil] [nil member])
    :coll (let [[lo hi] (length/bounds member)]
            (cond
              (and hi (< hi cutoff)) [member nil]
              (>= lo cutoff) [nil member]
              :else [(length/with-bounds member lo (dec' cutoff))
                     (length/with-bounds member cutoff hi)]))
    [member member]))

(defn- count-order-contexts [ctx op a b]
  (let [[op measured bound] (if (literal-node? a)
                              [({'< '> '> '< '<= '>= '>= '<=} op) b a]
                              [op a b])]
    (when (literal-node? bound)
      (let [size (:val bound)]
        (when-let [source (count-source ctx measured size)]
          (let [cutoff (if (#{'<= '>} op) (inc' size) size)
                branches (refine-path ctx source #(count-order-parts cutoff %))]
            (if (#{'< '<=} op) branches (some-> branches rseq vec))))))))

(defn- invalid-parts [ctx leaf]
  (if (#{:coll :map :map-of :fn} (:t leaf))
    [nil leaf]
    (equality-parts ctx conform/invalid-key leaf)))

(defn- presence-parts [env key leaf]
  (case (:t leaf)
    :nil [nil leaf]
    :never [nil nil]
    (:map :map-of)
    (let [shape (if (= :map-of (:t leaf)) (indexed-map-shape leaf) leaf)]
      (if (contains? (:req shape) key)
        [shape nil]
        (if-let [field (optional-map-value env shape key)]
          (if (= t/never field)
            [nil leaf]
            [(assoc-map-key shape key field) (assoc-in shape [:opt key] t/never)])
          [leaf leaf])))
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

(defn- computed-presence-contexts [ctx source key-node]
  (when-let [local (when-not (env/tag-node key-node) (plain-local ctx key-node))]
    (let [env (:env ctx)
          keys (finite-map-keys env (eff ctx (get-in ctx [:locals local])))
          subject (when keys (stable-path ctx source))
          source-type (when subject (projected-type ctx (:type subject) (:path subject)))
          maps (when source-type (vec (take (inc map-path-budget) (t/members env source-type))))]
      (when (and (seq maps) (every? #(#{:map :map-of :nil} (:t %)) maps)
                 (<= (* (count keys) (+ (count keys) (count maps))) map-path-budget))
        (let [branches (mapv (fn [key]
                               (when-let [candidate (first (equality-contexts ctx key-node key))]
                                 (or (refine-path candidate source #(presence-parts env key %))
                                     [candidate candidate])))
                             keys)]
          [(join-contexts ctx (map first branches)) (join-contexts ctx (map second branches))])))))

(defn- presence-contexts [ctx source key-node]
  (if-let [key (stable-map-key key-node)]
    (refine-path ctx source #(presence-parts (:env ctx) key %))
    (computed-presence-contexts ctx source key-node)))

(defn- vector-presence-parts [index member]
  (cond
    (= :nil (:t member)) [nil member]
    (= :vector (:kind member))
    (case (length/position member index)
      :absent [nil member]
      :present [member nil]
      :possible [(collection-with-position member index) (collection-without-position member index)])
    :else [member member]))

(defn- contains-contexts [ctx source key-node]
  (or (when-not (:guards-disabled? ctx)
        (let [key (stable-selector-value ctx key-node)]
          (when (integer? key)
            (let [index (lookup-index key)]
              (refine-path ctx source #(vector-presence-parts index %))))))
      (presence-contexts ctx source key-node)))

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
              (guard-call ctx node)
              (#{'some? 'nil? 'seq 'empty? 'not 'and 'or '= 'not= '< '<= '> '>= 'contains? 'find 'instance?
                 'clojure.spec.alpha/valid? 'clojure.spec.alpha/invalid?} head))
      {:test node
       :head head
       :bindings (into {} (for [n (tree-seq (comp seq :children) :children node) :when (= :sym (:op n))]
                            [(:val n) (get-in ctx [:binding-ids (:val n)])]))})))

(defn- find-call-contexts [ctx node]
  (when (and (= :list (:op node)) (= 'find (builtin-head ctx node)))
    (let [[_ source key & extra] (:children node)]
      (when (and key (empty? extra))
        (presence-contexts ctx source key)))))

(defn- find-contexts [ctx node]
  (or (find-call-contexts ctx node)
      (when-let [local (when-not (env/tag-node node) (plain-local ctx node))]
        (let [{:keys [head test]} (active-condition ctx local)]
          (when (= 'find head)
            (when-let [branches (find-call-contexts ctx test)]
              (let [env (:env ctx)
                    ty (eff ctx (get-in ctx [:locals local]))]
                (mapv (fn [branch type]
                        (when (and branch (not= t/never type))
                          (assoc-in branch [:locals local] type)))
                      branches [(t/remove-nil env ty) (nil-part env ty)]))))))))

(defn- truth-contexts [ctx node]
  (or (find-contexts ctx node)
      (refine-path ctx node (fn [leaf]
                             (for [truthy? [true false]
                                   :let [ty (truth-part (:env ctx) leaf truthy?)]]
                               (when-not (= :never (:t ty)) ty))))))

(defn- some-contexts [ctx node]
  (or (find-contexts ctx node)
      (refine-path ctx node (fn [leaf]
                             (let [present (t/non-nil-leaf leaf)]
                               [(when-not (= t/never present) present)
                                (when (t/may-be-nil-leaf? leaf) t/nil-t)])))))

(defn- emptiness-contexts [ctx node some?]
  (let [[source & extra] (rest (:children node))]
    (when (and source (empty? extra))
      (case (builtin-head ctx node)
        seq (some-> (refine-path ctx source #(count-parts 0 %)) rseq vec)
        empty? (when (and (not some?) (not (:guards-disabled? ctx)))
                 (refine-path ctx source #(count-parts 0 %)))
        nil))))

(defn- narrow
  "Truth and falsity contexts. A nil context denotes an unreachable path."
  [ctx test]
  (let [env (:env ctx)
        h (builtin-head ctx test)
        [a b :as args] (vec (rest (:children test)))
        local (plain-local ctx test)
        {captured-test :test captured-head :head} (active-condition ctx local)]
    (cond
      (and captured-test (not= 'find captured-head))
      (narrow ctx captured-test)

      (literal-node? test)
      (if (:val test) [ctx nil] [nil ctx])

      local
      (let [ty (eff ctx (get-in ctx [:locals local]))
            positive (truth-part env ty true)
            negative (truth-part env ty false)
            update-type (fn [ty] (when-not (= :never (:t ty)) (assoc-in ctx [:locals local] ty)))
            fc (update-type negative)]
        (or (when (or (= 'find captured-head) (seq (get-in ctx [:projections local :path])))
              (truth-contexts ctx test))
            [(update-type positive)
             (if (= t/nil-t negative)
               (reduce (fn [c s] (update-in c [:locals s] #(t/remove-nil env %))) fc (get-in ctx [:missing-of local]))
               fc)]))

      (#{'some? 'nil?} h)
      (if-let [branches (when (= 1 (count args)) (some-contexts ctx a))]
        (if (= 'some? h) branches (vec (rseq branches)))
        [ctx ctx])

      (and (= 'not h) (= 1 (count args))) (vec (reverse (narrow ctx a)))
      (#{'empty? 'seq} h) (or (emptiness-contexts ctx test false) [ctx ctx])
      (= 'and h) (logical-contexts ctx args true)
      (= 'or h) (logical-contexts ctx args false)

      (and (#{'= 'not=} h) (= 2 (count args)))
      (let [branches (or (cond (and (literal-node? a) (literal-node? b))
                               (if (= (:val a) (:val b)) [ctx nil] [nil ctx])
                               (literal-node? b) (or (count-equality-contexts ctx a (:val b))
                                                     (equality-contexts ctx a (:val b)))
                               (literal-node? a) (or (count-equality-contexts ctx b (:val a))
                                                     (equality-contexts ctx b (:val a))))
                         [ctx ctx])]
        (if (= '= h) branches (vec (reverse branches))))

      (and (#{'< '<= '> '>=} h) (= 2 (count args)))
      (or (count-order-contexts ctx h a b) [ctx ctx])

      (and (= 'contains? h) (= 2 (count args)))
      (or (contains-contexts ctx a b) [ctx ctx])

      (and (= 'instance? h) (= 2 (count args)) (= :sym (:op a)))
      (or (when-let [c (t/resolve-class (:imports (:file ctx)) (:val a))]
            (refine-path ctx b #(instance-parts ctx c %)))
          [ctx ctx])

      (and (= 'clojure.spec.alpha/valid? h) (= 2 (count args)) (= :kw (:op a)) (plain-local ctx b))
      [(assoc-in ctx [:locals (plain-local ctx b)] (t/ref-t (:val a))) ctx]

      (and (= 'clojure.spec.alpha/invalid? h) (= 1 (count args)))
      (or (refine-path ctx a #(invalid-parts ctx %)) [ctx ctx])

      (and (primitive-predicates h) (= 1 (count args)))
      (let [target (t/prim (first (primitive-predicates h)))]
        (or (refine-path ctx a #(predicate-parts ctx h target %)) [ctx ctx]))

      :else (or (when-let [summary (guard-call ctx test)]
                  (refine-path ctx a #(mapv (fn [ty] (when-not (= t/never ty) ty))
                                            (guards/apply-summary env summary %))))
                (truth-contexts ctx test) [ctx ctx]))))

(defn- condition-contexts [ctx test]
  (let [ty (eff ctx (infer ctx test))
        [tc fc] (narrow ctx test)]
    [(when-not (= :never (:t (truth-part (:env ctx) ty true))) tc)
     (when-not (= :never (:t (truth-part (:env ctx) ty false))) fc)]))

;; ---- destructuring ----

(defn- bounded-combinations [paths]
  (let [variants (fn [steps]
                   (reduce (fn [prefixes choices]
                             (for [prefix prefixes, choice choices] (conj prefix choice)))
                           [[]] steps))
        result (vec (take (inc map-path-budget) (mapcat variants paths)))]
    (when (<= (count result) map-path-budget) result)))

(defn- keys-directive [node]
  (when (= :kw (:op node))
    (let [key (:val node)
          nsp (namespace key)]
      (case (name key)
        ("keys" "keys!") {:key-of #(keyword (or nsp (namespace %)) (name %))
                          :bang? (= "keys!" (name key))}
        "strs" (when-not nsp {:key-of #(when (simple-symbol? %) (name %))})
        "syms" {:key-of #(when (symbol? %) (symbol (or nsp (namespace %)) (name %)))}
        nil))))

(defn- has-bang? [pattern]
  (and (= :map (:op pattern))
       (some (fn [[k _]] (or (:bang? (keys-directive k)) (has-bang? k))) (partition 2 (:children pattern)))))

(defn- map-binding-key [node]
  (case (:op node)
    :sym (:val node)
    :kw (when-not (or (:bang? (keys-directive node))
                      (#{:missing :select :excess :all :strs! :syms!} (:val node)))
          (:val node))
    (:map :vector)
    (when (every? #(#{:sym :kw :str :num :bool :char :nil :list :vector :map :set :quote :var} (:op %))
                  (tree-seq :children :children node))
      (parse/node->data node))
    nil))

(defn- map-binding-order [pairs]
  (let [binding-keys (mapv (comp map-binding-key first) pairs)]
    (if (every? some? binding-keys)
      ;; Clojure expands the directives into a map before binding any fields.
      ;; Assoc can overwrite an explicit binding or promote the array map.
      (let [entries (into {} (map vector binding-keys pairs))
            directives (into {}
                             (keep (fn [[name [node selectors]]]
                                     (when (= :vector (:op selectors))
                                       (when-let [directive (keys-directive node)]
                                         [name [directive selectors]]))))
                             entries)]
        (vals (reduce (fn [bindings [name [{:keys [key-of]} selectors]]]
                        (reduce (fn [bindings node]
                                  (let [local (:val node)]
                                    (if-let [key (and (#{:sym :kw} (:op node)) (key-of local))]
                                      (let [binding (or (first (get bindings local)) node)]
                                        (assoc bindings local [binding node key]))
                                      bindings)))
                                (dissoc bindings name) (:children selectors)))
                      (dissoc entries :as :or) directives)))
      (sort-by (fn [[k _]] (if (keys-directive k) 1 0)) pairs))))

(defn- bind-map-entry [ctx pattern node key ty outer-why projection defaults & [selected]]
  (let [env (:env ctx)
        {:keys [type optional? declared? status]} (or selected (key-info ctx ty key node))
        local (when (= :sym (:op pattern)) (:val pattern))
        default (get defaults local)
        default-type (when default (infer ctx default))
        bang? (has-bang? pattern)
        tname (when (and declared? (or optional? bang?)) (t/show (t/remove-nil env ty)))
        why (cond
              default (when (t/nilable? env type)
                        (str key " is " (t/show (t/resolve-t env type))
                             ", and :or applies only when the key is absent"))
              optional? (cond
                          (= :req status) (or outer-why "the destructured value may be nil")
                          declared? (str key " is optional in " tname)
                          :else (str key " may be absent")))]
    (when (and bang? declared? (or optional? (t/nilable? env type)))
      (report! ctx node "TSP2741" (str ":keys! below requires " key ", but it is optional in " tname)))
    (bind-pattern ctx pattern (cond bang? (t/remove-nil env type)
                                    optional? (t/union type (or default-type t/nil-t))
                                    :else type)
                  why
                  (when-not (or default bang?) projection))))

(defn- finite-map-keys [env ty]
  (let [keys (mapv selector-literal-key (take (inc map-path-budget) (t/members env ty)))]
    (when (and (<= 1 (count keys) map-path-budget) (every? some? keys)) keys)))

(defn- select-map-entries [ctx source selector selector-type keys]
  (let [entries (mapv #(key-info ctx source % selector) (distinct keys))
        declared? (every? :declared? entries)]
    {:type (t/with-assumption (apply t/union (map :type entries))
                              (or (t/assumption source) (t/assumption selector-type)))
     :declared? declared?
     :optional? (or (not declared?) (some :optional? entries))
     :status (when (every? #(= :req (:status %)) entries) :req)}))

(defn- bind-map-selector [ctx pattern selector source why project defaults]
  (if-let [key (static-map-key selector)]
    (bind-map-entry ctx pattern selector key source why (project key) defaults)
    (let [selector-type (eff ctx (infer ctx selector))
          keys (mapv selector-literal-key (t/members (:env ctx) selector-type))
          key (first keys)]
      (if (and key (= 1 (count keys)))
        (bind-map-entry ctx pattern selector key source why (project key) defaults)
        (let [selected (if (and (seq keys) (every? some? keys))
                         (select-map-entries ctx source selector selector-type keys)
                         {:type (read-map-of ctx source selector-type t/never) :optional? true})]
          (bind-map-entry ctx pattern selector (str "value selected by " (parse/unparse selector))
                          source why nil defaults selected))))))

(defn- append-destructuring-map [ctx shape tail]
  ;; Only an unmodified entry sequence in the tail acts like its source map.
  (let [tail (or (:entry-map tail) tail)]
    (case (:t tail)
      :nil shape
      :lit (if (= "" (:val tail)) shape (t/prim :map))
      :coll (let [[key value] (:items tail)]
              (if-let [k (when (and (= :vector (:kind tail)) (= 2 (count (:items tail))))
                           (selector-literal-key (t/resolve-t (:env ctx) key)))]
                (assoc-map-key shape k value)
                (t/prim :map)))
      :map (let [index (:map-index tail)
                 unspecified (fn [k]
                               (cond index (or (t/map-of-entry (:env ctx) index k) t/never)
                                     (:complete-keys? tail) t/never
                                     :else t/unknown))
                 required (reduce-kv (fn [required k value]
                                       (if (contains? required k)
                                         required
                                         (let [override (or (get (:opt tail) k) (unspecified k))]
                                           (assoc required k (t/union value override)))))
                                     (:req tail) (:req shape))]
             (cond-> (assoc shape :req required
                                  :opt (apply dissoc (:opt tail) (keys required))
                                  :complete-keys? (boolean (:complete-keys? tail)))
               index (assoc :map-index index)))
      :map-of (let [env (:env ctx)
                    keys (mapv selector-literal-key (t/members env (:k tail)))]
                (if (and (<= (count keys) map-path-budget) (every? some? keys))
                  (recur ctx shape (assoc empty-map-shape :opt (zipmap keys (repeat (:v tail)))))
                  (recur ctx shape (indexed-map-shape tail))))
      (t/prim :map))))

(defn- invalid-destructuring-tail? [member]
  (case (:t member)
    :lit (let [value (:val member)]
           (or (number? value) (boolean? value) (keyword? value) (symbol? value)
               (and (string? value) (seq value))))
    :prim (#{:int :double :boolean :keyword :symbol} (:name member))
    :coll (when (= :vector (:kind member))
            (let [[lower upper] (length/bounds member)]
              (or (> lower 2) (and upper (< upper 2)))))
    false))

(defn- invalid-destructuring-entry? [member]
  (and (not (:map-entry? member))
       (case (:t member)
         :nil true
         :lit true
         :prim (#{:int :double :boolean :keyword :symbol :string} (:name member))
         false)))

(defn- destructuring-entry-sequence-members [ctx pattern items]
  (let [members (mapv #(t/members (:env ctx) %) items)
        choices (mapv #(remove invalid-destructuring-entry? %) members)]
    (when-let [[index bad] (first (for [[index types] (map-indexed vector members)
                                       type types
                                       :when (invalid-destructuring-entry? type)]
                                   [index type]))]
      (let [entry (str "map destructuring tail entry at index " index)]
        (if (= :nil (:t bad))
          (report! ctx pattern "TSP2531" (str entry " may be nil"))
          (report! ctx pattern "TSP2345" (str entry " must be a Map.Entry, got " (t/show bad))))))
    (cond
      (some empty? choices) []
      (every? :map-entry? (mapcat identity choices))
      (if-let [variants (bounded-combinations [choices])]
        (mapv (fn [entries]
                (reduce (fn [shape entry]
                          (let [next-shape (append-destructuring-map ctx shape entry)]
                            (if (= :map (:t next-shape)) next-shape (reduced next-shape))))
                        empty-map-shape entries))
              variants)
        [(t/prim :map)])
      :else [(t/prim :map)])))

(defn- destructuring-tail-members [ctx pattern type]
  (let [members (t/members (:env ctx) type)]
    (when-let [bad (some #(when (invalid-destructuring-tail? %) %) members)]
      (report! ctx pattern "TSP2345"
               (if (= :vector (:kind bad))
                 (str "map destructuring tail expects a two-element vector, got length " (length/describe bad))
                 (str "map destructuring cannot append " (t/show bad) " as map entries"))))
    (mapcat (fn [{:keys [kind items] :as member}]
              (cond
                (and (= :seq kind) (= 0 (second (length/bounds member)))) [empty-map-shape]
                (and (= :seq kind) items) (destructuring-entry-sequence-members ctx pattern items)
                :else [member]))
            (remove invalid-destructuring-tail? members))))

(defn- destructuring-sequence-map [ctx pattern member]
  (let [env (:env ctx)
        items (:items member)
        count-items (count items)]
    (cond
      (= 0 (second (length/bounds member))) empty-map-shape

      (nil? items) t/unknown

      (= 1 count-items)
      (let [item (first items)]
        (if (every? #(or (#{:map :map-of :nil} (:t %))
                         (and (= :prim (:t %)) (= :map (:name %))))
                    (t/members env item))
          item
          t/unknown))

      :else
      (let [pairs (partition 2 items)
            key-choices (mapv (fn [[key-type]] (mapv selector-literal-key (t/members env key-type))) pairs)
            values (mapv second pairs)
            tail? (odd? count-items)
            tails (when tail? (destructuring-tail-members ctx pattern (peek items)))
            steps (cond-> key-choices tail? (conj tails))]
        (if (and tail? (empty? tails))
          t/never
          (if-let [variants (when (every? #(and (seq %) (every? some? %)) key-choices)
                              (bounded-combinations [steps]))]
            (apply t/union
                   (for [variant variants
                         :let [keys (if tail? (pop variant) variant)
                               shape (assoc empty-map-shape :req (into (array-map) (map vector keys values)))]]
                     (if tail? (append-destructuring-map ctx shape (peek variant)) shape)))
            (t/prim :map)))))))

(defn- destructuring-map-input [ctx pattern ty]
  (let [members (t/members (:env ctx) ty)
        seq-type? #(or (= :seq (:kind %)) (and (= :prim (:t %)) (= :seq (:name %))))]
    (if (some seq-type? members)
      (t/with-assumption
       (apply t/union (map #(if (seq-type? %) (destructuring-sequence-map ctx pattern %) %) members))
       (t/assumption ty))
      ty)))

(defn- destructure-map [ctx pattern ty outer-why projection]
  (let [env (:env ctx)
        source (destructuring-map-input ctx pattern ty)
        ;; A sequence and the map constructed from it have different identities.
        projection (when (= source ty) projection)
        ty source
        pairs (partition 2 (:children pattern))
        special (into {} (for [[k v] pairs :when (and (= :kw (:op k)) (nil? (namespace (:val k))))] [(:val k) v]))
        defaults (into {} (for [[k v] (partition 2 (:children (:or special)))] [(:val k) v]))
        missing-sym (some-> (:missing special) :val)
        tname (t/show (t/remove-nil env ty))
        ;; Destructuring evaluates its input once. Its whole value and plain
        ;; field bindings share this identity, even without :as.
        [ctx origin path] (track-path ctx (or projection {:path [] :type ty}))
        ctx (cond-> ctx missing-sym (bind missing-sym (t/union (t/prim :map) t/nil-t)))
        ctx (if-let [alias (:as special)]
              (-> (bind ctx (:val alias) ty)
                  (remember-projection (:val alias) origin path))
              ctx)
        project (fn [key] {:origin origin :path (conj path key)})
        ctx (reduce
             (fn [ctx [k v key]]
               (let [kd (keys-directive k)
                     binding (if (#{:sym :kw} (:op k)) (assoc k :op :sym :val (symbol (name (:val k)))) k)]
                 (cond
                   key
                   (bind-map-entry ctx binding v key ty outer-why (project key) defaults)

                   (and kd (= :vector (:op v)))
                   (reduce
                    (fn [ctx s]
                      (if-let [key (when (#{:sym :kw} (:op s)) ((:key-of kd) (:val s)))]
                        (let [sym (symbol (name (:val s)))
                              binding (assoc s :op :sym :val sym)]
                          (if (:bang? kd)
                            (let [{:keys [type optional? declared?]} (key-info ctx ty key s)]
                              (if missing-sym
                                (-> (bind ctx sym (t/union type t/nil-t)
                                          :why (str key " is missing when " missing-sym " is not nil"))
                                    (update-in [:missing-of missing-sym] (fnil conj []) sym))
                                (do (when (and optional? declared?)
                                      (report! ctx s "TSP2741" (str ":keys! requires " key ", but it is optional in " tname)))
                                    (bind ctx sym type))))
                            (bind-map-entry ctx binding s key ty outer-why (project key) defaults)))
                        ctx))
                    ctx (:children v))

                   (and (= :kw (:op k)) (#{:as :or :missing} (:val k))) ctx
                   (and (= :kw (:op k)) (#{:select :excess :all} (:val k))) (bind ctx (:val v) t/unknown)
                   (and (= :kw (:op k)) (#{:strs! :syms!} (:val k)))
                   (reduce #(bind %1 (:val %2) t/unknown) ctx (filter #(= :sym (:op %)) (:children v)))

                   :else
                   (bind-map-selector ctx binding v ty outer-why project defaults))))
             ctx (map-binding-order pairs))]
    ctx))

(defn- array-sequence-element [member]
  (let [element (array-read-type member)]
    (if (and (:platform member) (t/nilable? {} element))
      (assoc (t/remove-nil {} element) :platform true)
      element)))

(defn- member-element [member]
  (case (:t member)
    :coll (:of member)
    :nil t/never
    :map-of (t/prim :vector)
    :map (if (:map-index member) (t/prim :vector) t/unknown)
    :class (if (str/starts-with? (:name member) "[") (array-sequence-element member) t/unknown)
    (:prim :lit) (if (or (= :string (:name member)) (string? (:val member)))
                   (t/class-type "java.lang.Character") t/unknown)
    t/unknown))

(defn- elem-type [ctx ty]
  (t/with-assumption (apply t/union (map member-element (t/members (:env ctx) ty)))
                     (t/assumption ty)))

(defn- literal-string-size [member]
  (when (and (= :lit (:t member)) (string? (:val member)))
    (count (:val member))))

(defn- sequence-member-bounds [member]
  (if-some [size (or (:array-length member)
                     (if (= :nil (:t member)) 0 (literal-string-size member)))]
    [size size]
    (length/bounds member)))

(defn- positional-bounds? [member]
  (or (:items member) (length/constrained? member) (= :nil (:t member))
      (some? (:array-length member)) (some? (literal-string-size member))))

(defn- indexed
  "Element access is not checked for emptiness (as in TypeScript by default); :strict-nil makes it T | nil."
  [el]
  (if (#{:unknown :any :never :nil} (:t el)) el (assoc el :platform true)))

(defn- positional-type
  ([ctx ty i] (positional-type ctx ty i (t/members (:env ctx) ty)))
  ([ctx ty i members]
   (t/with-assumption
    (if (some positional-bounds? members)
      (apply t/union (for [m members]
                       (cond
                         (:items m) (get (:items m) i t/nil-t)
                         (= :nil (:t m)) t/nil-t
                         (positional-bounds? m)
                         (let [[lo hi] (sequence-member-bounds m)]
                           (cond
                             (or (neg? i) (and hi (>= i hi))) t/nil-t
                             (< i lo) (member-element m)
                             :else (t/union (member-element m) t/nil-t)))
                         :else (eff ctx (indexed (member-element m))))))
      (indexed (apply t/union (map member-element members))))
    (t/assumption ty))))

(defn- sequence-position-type [ctx ty i]
  (positional-type ctx ty i (map #(or (map-entry-sequence %) %) (t/members (:env ctx) ty))))

(defn- sequence-tail [ctx ty skip nullable?]
  (t/with-assumption
   (apply t/union
          (for [member (t/members (:env ctx) (eff ctx ty))]
            (if (= :never (:t member))
              t/never
              (let [items (:items member)
                    map-source? (= :map (:t member))
                    entry-map (if (or map-source? (= :map-of (:t member)))
                                (dissoc member :fresh)
                                (:entry-map member))
                    [min-count max-count] (if map-source?
                                            (let [required (count (:req member))]
                                              [required (when (:complete-keys? member)
                                                          (+ required (count (:opt member))))])
                                            (sequence-member-bounds member))
                    lo (max 0 (- min-count skip))
                    hi (when max-count (max 0 (- max-count skip)))
                    tail (cond
                           (and nullable? (= 0 hi)) t/nil-t
                           items (assoc (t/tuple (subvec items (min skip (count items)))) :kind :seq)
                           :else (cond-> (length/with-bounds {:t :coll :kind :seq
                                                            :of (if (= 0 hi)
                                                                  t/never
                                                                  (or (:of (map-entry-sequence member))
                                                                      (member-element member)))}
                                                           (if nullable? (max lo 1) lo) hi)
                                   (and entry-map (zero? skip)) (assoc :entry-map entry-map)))]
                (if (and nullable? (zero? lo)) (t/union tail t/nil-t) tail)))))
   (t/assumption ty)))

(defn- positional-rest [ctx ty i]
  (sequence-tail ctx ty i true))

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
                compiler-class (when h (env/hint-class (:file ctx) (env/tag-node pattern)))
                env (:env ctx)
                ;; the compiler believes the hint; keep the inferred type only when it agrees and says more
                use-hint? (and h (or (#{:unknown :any} (:t (t/resolve-t env ty)))
                                     (not (t/sub? env (t/remove-nil env (eff ctx ty)) h))))
                [c origin path] (if (and projection (not use-hint?)) (track-path ctx projection) [ctx])]
            (cond-> (bind c (:val pattern) (if use-hint? h ty)
                          :why (when-not use-hint? why) :hinted (boolean h) :compiler-class compiler-class)
              origin (remember-projection (:val pattern) origin path)))
     :map (destructure-map ctx pattern ty why projection)
     :vector (let [members (t/members (:env ctx) ty)
                   sequence-positions? (every? #(or (#{:vector :seq} (:kind %)) (= :nil (:t %))) members)
                   [ctx origin path] (if (or sequence-positions? (every? :items members))
                                       (track-path ctx (or projection {:path [] :type ty}))
                                       [ctx])
                   project (fn [path] (when origin {:origin origin :path path}))]
               (loop [ctx ctx, i 0, [p & more] (:children pattern)]
                 (cond (nil? p) ctx
                       (and (= :sym (:op p)) (= '& (:val p)))
                       (recur (bind-pattern ctx (first more) (positional-rest ctx ty i)) i (next more))
                       (and (= :kw (:op p)) (= :as (:val p)))
                       (bind-pattern ctx (first more) ty why (project path))
                       :else
                       (let [step (cond (every? #(contains? (:items %) i) members) i
                                        sequence-positions? {:seq-index i})]
                         (recur (bind-pattern ctx p (positional-type ctx ty i) nil
                                              (when step (project (conj path step))))
                                (inc i) more)))))
     ctx)))

;; ---- function calls ----

(defn- call-shape [ftype]
  (or (::recursive-callback-shape (meta ftype)) ftype))

(defn- matching-arities [ftype n]
  (let [arities (:arities ftype)
        fits (if (::t/dispatch (meta ftype))
               (when-let [arity (t/dispatch-arity arities n)] [arity])
               (filter #(t/arity-fits? % n) arities))]
    (if-let [shape (::recursive-callback-shape (meta ftype))]
      (when-let [implemented (seq (matching-arities shape n))]
        (or (seq fits) implemented))
      fits)))

(defn- contextual-call-target [ctx ftype arg-types]
  (or (when-let [target (::contextual-self-call (meta ftype))]
        (target ctx arg-types))
      (when-let [shape (when (::actual-call (meta ftype))
                        (::recursive-callback-shape (meta ftype)))]
        (let [covers? (fn [{:keys [params rest kwargs]}]
                        (or kwargs
                            (every? true?
                                    (map #(t/sub? (:env ctx) (eff ctx %1) %2)
                                         arg-types (concat params (repeat rest))))))]
          ;; Entry arguments describe one caller, not an invariant of recursion.
          ;; A different call still has to satisfy the implementation's hints.
          (when-not (some covers? (matching-arities ftype (count arg-types)))
            shape)))
      ftype))

(defn- keyword-parameter [m k]
  (some (fn [[declared ty]] (when (= (name declared) (name k)) ty))
        (concat (:req m) (:opt m))))

(defn- arg-problems [ctx a arg-nodes arg-types call-node]
  (let [check-arg (fn [n at pt]
                    (when at
                      (if-let [[expected actual] (::callback-arity-error (meta at))]
                        [[n ["TSP2554" (str "callback expects " expected " parameter(s) in this call, got " actual)]]]
                        (map #(vector n %) (problems ctx n at pt)))))
        k (count (:params a))
        fixed (mapcat check-arg
                      arg-nodes arg-types (:params a))
        extra (drop k (map vector arg-nodes arg-types))]
    (concat
     fixed
     (when (:rest a) (mapcat (fn [[n at]] (check-arg n at (:rest a))) extra))
     (when-let [kw (:kwargs a)]
       (let [given (into {} (for [[[kn _] [vn vt]] (partition 2 extra) :when (= :kw (:op kn))] [(:val kn) [vn vt]]))
             m (first (map-members ctx kw))]
         (concat
          (for [rk (keys (:req m)) :when (not (contains? given (keyword (name rk))))]
            [call-node ["TSP2741" (str "required key " (keyword (name rk)) " is missing")]])
          (for [[gk [vn vt]] given
                :let [exp (keyword-parameter m gk)]
                :when (and exp vt)
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

(defn- fn-lit-parameters [node]
  (let [syms (keep #(when (and (= :sym (:op %)) (simple-symbol? (:val %))) (name (:val %)))
                   (tree-seq :children :children node))
        n (reduce max 0 (keep #(cond (= "%" %) 1
                                    (re-matches #"%[1-9][0-9]*" %) (or (parse-long (subs % 1)) 21)) syms))]
    (when (<= n 20)
      {:fixed (mapv #(hash-map :op :sym :val (symbol (str "%" %))) (range 1 (inc n)))
       :rest (when (some #{"%&"} syms) {:op :sym :val '%&})})))

(defn- inline-callback? [ctx node]
  (or (= :fn-lit (:op node))
      (#{'fn 'fn*} (builtin-head ctx (dissoc node :directives :meta)))))

(defn- callback-signature [ctx expected]
  (let [members (remove #(= :nil (:t %)) (t/members (:env ctx) expected))
        ty (first members)]
    (when (and (= 1 (count members)) (= :fn (:t ty)) (seq (:arities ty))
               (not-any? :kwargs (:arities ty)))
      ty)))

(defn- deferred-callback [ctx node]
  (when-let [sym (plain-local ctx node)]
    (::deferred-callback (meta (get-in ctx [:locals sym])))))

(defn- callback-node? [ctx node]
  (or (inline-callback? ctx node) (deferred-callback ctx node)))

(defn- argument-type [ctx ar args i]
  (let [fixed (count (:params ar))]
    (cond
      (< i fixed) (nth (:params ar) i)
      (:rest ar) (:rest ar)
      (and (:kwargs ar) (odd? (- i fixed)))
      (let [key (nth args (dec i))]
        (when (= :kw (:op key))
          (keyword-parameter (first (map-members ctx (:kwargs ar))) (:val key)))))))

(defn- common-callback-signature [ctx fits args i]
  (let [signatures (mapv #(callback-signature ctx (argument-type ctx % args i)) fits)
        inputs (fn [signature] (mapv #(select-keys % [:params :rest]) (:arities signature)))]
    (when (and (seq signatures) (every? some? signatures)
               (apply = (map inputs signatures)))
      {:t :fn :arities (apply mapv (fn [& ars] (assoc (first ars) :ret (apply t/union (map :ret ars))))
                               (map :arities signatures))})))

(defn- infer-with-expected [ctx node expected]
  (let [ty (when (and expected (= 'atom (builtin-head ctx node)))
             (t/resolve-t (:env ctx) expected))]
    (infer (cond-> ctx (= :atom (:t ty)) (assoc :atom-content (:of ty))) node)))

(defn- infer-call-arguments [ctx call-node arg-nodes fits]
  (let [pending? (fn [i arg]
                   (and (callback-node? ctx arg)
                        (some #(callback-signature ctx (argument-type ctx % arg-nodes i)) fits)))
        types (mapv (fn [i arg]
                      (when-not (pending? i arg)
                        (let [params (when (= 'atom (builtin-head ctx arg))
                                       (map #(argument-type ctx % arg-nodes i) fits))]
                          (infer-with-expected ctx arg (when (and (seq params) (apply = params)) (first params))))))
                    (range) arg-nodes)
        candidates (when (some nil? types)
                     (let [possible (filter #(every? (fn [[_ [code]]] (= "TSP210-01" code))
                                                    (arg-problems ctx % arg-nodes types call-node)) fits)]
                       (or (seq possible) (when (= 1 (count fits)) fits))))]
    (mapv (fn [i arg ty]
            (or ty (if-let [expected (common-callback-signature ctx candidates arg-nodes i)]
                     (infer ctx arg expected)
                     (infer ctx arg))))
          (range) arg-nodes types)))

(defn- constrain-generic-arguments [ctx state arity arguments]
  (let [entries (mapv (fn [i expected arg] (assoc arg :i i :expected expected))
                      (range) (:params arity) arguments)
        ordinary-types (mapv (fn [{:keys [node type]}]
                               (or type (when-not (callback-node? ctx node)
                                          (or (infer ctx node) t/unknown))))
                             entries)
        constrain (fn [st {:keys [i expected]} actual callback?]
                    (solver/constrain st (eff ctx actual) expected
                                      (cond-> {:argument i} callback? (assoc :callback true))))
        ordinary-state (reduce (fn [st [entry actual]]
                                 (if actual (constrain st entry actual false) st))
                               state (map vector entries ordinary-types))
        pending (keep (fn [[entry actual]] (when-not actual (:expected entry)))
                      (map vector entries ordinary-types))
        partial (when (seq pending)
                  (callback-substitution (trusted-substitution (solver/solve (:env ctx) ordinary-state))
                                         pending))
        types (mapv (fn [{:keys [node expected]} actual]
                      (or actual
                          (if-let [params (callback-context expected partial)]
                            (infer ctx node {:t :fn :arities [{:params params :ret t/unknown}]})
                            (do (infer ctx node) t/unknown))))
                    entries ordinary-types)
        state (reduce (fn [st [entry ordinary? actual]]
                        (if ordinary? st (constrain st entry actual true)))
                      ordinary-state (map vector entries ordinary-types types))]
    [state types]))

(defn- call-generic
  "Rank-1, single fixed arity: infer ordinary arguments before contextual callbacks.
  Typed arguments are used by apply-type; node arguments are visited at most once."
  [ctx node label ftype arg-nodes typed-args]
  (let [captured (:captured-args ftype)
        state (solver/instantiate (or (:partial-target ftype) ftype))
        ars (:arities (:type state)) ar (first ars)
        n (count (or typed-args arg-nodes))
        remaining (- (count (:params ar)) (count captured))]
    (cond
      (or (not= 1 (count ars)) (:rest ar) (:kwargs ar))
      (do (when-not typed-args (run! #(infer ctx %) arg-nodes))
          (report! ctx node "TSP210-01" "generic overloads and variadic contracts are unsupported"
                   :severity (if (:strict (:opts ctx)) :error :warning))
          (assoc t/unknown :generic-failure true))
      (not= n remaining)
      (do (when-not typed-args (run! #(infer ctx %) arg-nodes))
          (report! ctx node "TSP2554" (str label " expects " remaining " argument(s), got " n))
          (assoc t/unknown :generic-failure true))
      :else
      (let [entries (concat (map #(hash-map :type %) captured)
                            (if typed-args (map #(hash-map :type %) typed-args)
                                (map #(hash-map :node %) arg-nodes)))
            [state] (constrain-generic-arguments ctx state ar entries)
            ret (generic-result ctx node label state)
            steps (get-in ftype [:body :arities 0 :composition])]
        (if (and steps (not (:generic-failure ret)))
          (or (run-composition ctx node steps ret) (assoc t/unknown :generic-failure true))
          ret)))))

(defn- closest-arity-index [arg-nodes problems]
  (let [positions (zipmap arg-nodes (range))
        call-position (count arg-nodes)]
    (->> problems
         (map-indexed vector)
         (sort-by (fn [[i issues]]
                    (let [rejected (->> issues
                                        (keep (fn [[node [code]]]
                                                (when (not= "TSP210-01" code)
                                                  (get positions node call-position))))
                                        distinct
                                        sort)]
                      [(count rejected) (mapv - rejected) (- i)])))
         ffirst)))

(defn- failed-call-result [ctx node fits arg-nodes arg-types all]
  ;; Incomplete evidence is not a mismatch. Prefer a candidate whose known
  ;; arguments fit, but still emit its uncertainty.
  (let [incomplete? (fn [problems] (every? #(= "TSP210-01" (first (second %))) problems))
        incomplete (keep-indexed (fn [i ps] (when (and (seq ps) (incomplete? ps)) i)) all)
        non-nil (mapv #(t/remove-nil (:env ctx) (eff ctx %)) arg-types)
        non-nil-problems (map #(arg-problems (assoc ctx :quiet true) % arg-nodes non-nil node) fits)
        nil-only (first (keep-indexed (fn [i ps] (when (empty? ps) i)) non-nil-problems))
        nil-compatible (keep-indexed (fn [i ps] (when (incomplete? ps) i)) non-nil-problems)
        unchecked? (some #(= "TSP210-01" (first (second %))) (mapcat identity all))
        i (or (first incomplete) nil-only (if (next fits) (closest-arity-index arg-nodes all) 0))]
    (doseq [[pn [code msg extra]] (nth all i)]
      (apply report! ctx pn code msg (mapcat identity extra)))
    (if unchecked?
      (do
        ;; A merely incomplete candidate must not hide a possible nil boundary
        ;; on another viable overload.
        (doseq [[pn [code msg extra]]
                (distinct (for [j nil-compatible
                                problem (nth all j)
                                :when (and (= "TSP2531" (first (second problem)))
                                           (not (some #{problem} (nth all i))))]
                            problem))]
          (apply report! ctx pn code msg (mapcat identity extra)))
        (let [possible (distinct (concat incomplete nil-compatible))
              returns (distinct (map :ret (if (seq possible) (map #(nth fits %) possible) fits)))]
          (if (next returns) t/unknown (first returns))))
      (:ret (nth fits i)))))

(defn- call-fn [ctx node fname ftype arg-nodes & [typed-args]]
  (if (= :all (:t ftype))
    (call-generic ctx node fname ftype arg-nodes typed-args)
    (let [n (count arg-nodes)
          initial-fits (matching-arities ftype n)
          arg-types (or typed-args (infer-call-arguments ctx node arg-nodes initial-fits))
          target (contextual-call-target ctx ftype arg-types)
          ars (:arities (call-shape target))
          fits (if (identical? ftype target) initial-fits (matching-arities target n))]
    (cond
      (empty? ars) (or (:ret ftype) t/unknown)
      (empty? fits) (do (report! ctx node "TSP2554"
                                 (str fname " expects " (str/join " or " (distinct (map #(str (t/arity-minimum %) (when (or (:rest %) (:kwargs %)) "+")) ars)))
                                      " argument(s), got " n))
                        (apply t/union (map :ret ars)))
      :else (let [problems (map #(arg-problems ctx % arg-nodes arg-types node) fits)
                  passing (keep-indexed (fn [i ps] (when (empty? ps) i)) problems)]
              (if-let [i (first passing)]
                (let [vague? (and (next fits)
                                  (some #(#{:unknown :any} (:t (t/resolve-t (:env ctx) %))) arg-types))]
                  (if (and vague? (next (distinct (map #(:ret (nth fits %)) passing))))
                    t/unknown
                    (or (specialize-call-return ctx node (nth fits (if vague? (last passing) i)) arg-types) t/unknown)))
                (failed-call-result ctx node fits arg-nodes arg-types problems)))))))

;; ---- inference ----

(defn- args-of [node] (vec (rest (:children node))))

(defn- record! [ctx node ty]
  (when (and *types* (:pos node) (not (:synthetic node)))
    (let [parameter (when (= :sym (:op node)) (get-in ctx [:contextual-params (:val node)]))]
      (swap! *types* conj (cond-> {:file (:file (:file ctx)) :pos (:pos node) :type ty :ns (:ns (:file ctx))}
                           parameter (assoc :context parameter)))))
  ty)

(defn- known-ns? [ctx ns-sym]
  (or (contains? (:nss (:env ctx)) ns-sym) (some? (core/stdlib-publics ns-sym))))

(defn- resolve-var
  "Resolve a Var name without reading its root."
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
        (if (or (contains? (:var-declarations env) local) (get-in env [:vars local]) (contains? (:macros env) local))
          {:sym local :type (var-type ctx local)}
          (let [c (symbol "clojure.core" (name r))]
            {:sym c :type (or (var-type ctx c) (get core/table c))}))))))

(defn- root-initialized? [ctx sym initializers at]
  (or (contains? (:initialized-vars ctx) sym)
      (if (and (:load-time? ctx) (= (namespace sym) (str (get-in ctx [:file :ns]))))
        (some (fn [{:keys [file pos]}]
                (let [end ((juxt :end-row :end-col) pos)
                      start ((juxt :row :col) (:pos at))]
                  (and (= file (get-in ctx [:file :file]))
                       (every? number? (concat end start))
                       (not (pos? (compare end start))))))
              initializers)
        (seq initializers))))

(defn- record-var-use! [ctx sym node kind]
  (when (and *var-uses* (not (:quiet ctx)) (not (:synthetic node)) (:pos node)
             (or (= :var kind) (not (contains? (:possible-bindings ctx) (:val node)))))
    (when-let [entry (get-in ctx [:env :vars sym])]
      (when-not (:private? entry)
        (swap! *var-uses* conj {:var sym :kind kind :file (get-in ctx [:file :file]) :pos (:pos node)})))))

(defn- read-var [ctx sym node kind]
  (let [{full :sym :as resolved} (resolve-var ctx sym node)
        {:keys [tracks-root? initializers]} (get-in ctx [:env :var-declarations full])]
    (record-var-use! ctx full node kind)
    (if (and tracks-root? (not (root-initialized? ctx full initializers node)))
      (do (when-not (contains? (:possible-bindings ctx) sym)
            (report! ctx node "TSP721-01" (str "Var " full " may be unbound at this read")
                     :severity (if (:strict (:opts ctx)) :error :warning)
                     :reasons #{:possibly-unbound-var}))
          (assoc resolved :type t/unknown))
      resolved)))

(defn- java-static?
  "Class name when `Foo/bar` refers to a Java class and not to a Clojure namespace."
  [ctx sym]
  (when-let [n (some-> (namespace sym) symbol)]
    (when-not (or (contains? (:aliases (:file ctx)) n) (known-ns? ctx n))
      (t/resolve-class (:imports (:file ctx)) n))))

(defn- compiler-static-field-class [ctx sym]
  (when-let [cname (java-static? ctx sym)]
    (java/field-class cname (name sym) true)))

(defn- java-field-type [class-name]
  (let [value-class (if (= "char" class-name) "java.lang.Character" class-name)]
    (t/platform (t/class-type value-class) (java/short-name class-name))))

(defn- class-symbol-name [ctx node]
  (let [{:keys [file env]} ctx
        sym (:val node)]
    (when (and (= :sym (:op node)) (empty? (:directives node)) (not (env/tag-node node))
               (not (t/primitives (str sym))) (not (contains? (:locals ctx) sym))
               (not (contains? (:refers file) sym))
               (not (contains? (get-in env [:ns-defs (:ns file)]) sym)))
      (t/resolve-class (:imports file) sym))))

(defn- dot-static-class [ctx node]
  (let [{:keys [file env]} ctx
        sym (:val node)]
    (when (and (= :sym (:op node)) (simple-symbol? sym) (not (t/primitives (name sym))))
      ;; The full dot form resolves a class from the namespace before looking at locals.
      (or (get (:imports file) sym)
          (when-not (or (contains? (:refers file) sym)
                        (contains? (get-in env [:ns-defs (:ns file)]) sym))
            (t/resolve-class (:imports file) sym))))))

(defn- literal-for-name-class [ctx node]
  (when (and (= :list (:op node)) (empty? (:directives node)) (not (env/tag-node node)))
    (let [[head class-name & more] (:children node)
          sym (:val head)]
      (when (and (= :sym (:op head)) (empty? (:directives head)) (not (env/tag-node head))
                 (= "forName" (name sym)) (= "java.lang.Class" (java-static? ctx sym))
                 (= :str (:op class-name)) (literal-node? class-name) (empty? more))
        (when-let [^Class cls (t/load-class (:val class-name))]
          (when-not (.isPrimitive cls) cls))))))

(defn- probe [ctx f]
  (binding [*types* nil *diags* (atom []) *trust* (atom {:cast 0 :any 0 :ignore 0})
            *ignores* (atom []) *rets* (atom (when *rets* @*rets*))]
    (f (assoc ctx :quiet true))))

(defn- quietly [ctx node]
  (probe ctx #(infer % node)))

(defn- widen [ty]
  (cond-> (case (:t ty)
            :lit (t/with-assumption (t/lit-prim (:val ty)) (t/assumption ty))
            :union (apply t/union (map widen (:types ty)))
            ty)
    (:platform ty) (assoc :platform true)))

(defn- direct-callback-context
  ([types] (direct-callback-context types true))
  ([types widen?]
   (with-meta {:t :fn :arities [{:params (if widen? (mapv #(widen (dissoc % :fresh)) types) types)
                                 :ret t/unknown}]}
     {::actual-call true})))

(defn- vector-member? [member]
  (or (= :vector (:kind member))
      (and (= :prim (:t member)) (= :vector (:name member)))))

(defn- widen-vector-growth [env ty]
  (when (= :union (:t ty))
    (let [members (t/members env ty)]
      (when (every? vector-member? members)
        (t/with-assumption
         {:t :coll :kind :vector :of (widen (apply t/union (map #(or (:of %) t/unknown) members)))}
         (t/assumption ty))))))

(defn- map-edits [ctx head args]
  (case head
    assoc (when (and (seq args) (even? (count args)))
            (let [pairs (partition 2 args)
                  keys (mapv (comp stable-map-key first) pairs)]
              (when (every? some? keys)
                (let [edits (mapv (fn [key [_ value]] [key (quietly ctx value)]) keys pairs)]
                  (when (every? (fn [[k ty]]
                                  (and (not (t/assumption ty))
                                       (or (nil? (get-in (:env ctx) [:specs k]))
                                           (t/sub? (:env ctx) (eff ctx ty) (t/ref-t k)))))
                                edits)
                    edits)))))
    dissoc (let [keys (mapv stable-map-key args)]
             (when (every? some? keys)
               (mapv (fn [key] [key nil]) keys)))
    nil))

(defn- infer-map-edits [ctx param node]
  (cond
    (= param (plain-local ctx node)) []
    (#{'assoc 'dissoc '->} (builtin-head ctx node))
    (let [[base & args] (args-of node)
          head (builtin-head ctx node)]
      (when-let [before (infer-map-edits ctx param base)]
        (if (= '-> head)
          (reduce (fn [edits step]
                    (when-let [next-edits (and edits (map-edits ctx (builtin-head ctx step) (args-of step)))]
                      (into edits next-edits)))
                  before args)
          (when-let [after (map-edits ctx head args)] (into before after)))))))

(defn- specialize-map-return [ctx {:keys [map-edits ret]} arg]
  (if (or (not map-edits) (:guards-disabled? ctx))
    ret
    (let [members (t/members (:env ctx) (eff ctx arg))]
      (if (every? #(= :map (:t %)) members)
        (let [result (apply t/union
                            (for [m members]
                              (reduce (fn [shape [k v]]
                                        (if v
                                          (assoc-map-key shape k (widen (dissoc v :fresh)))
                                          (dissoc-map-key shape k true)))
                                      (dissoc m :fresh) map-edits)))]
          (if (t/sub? (:env ctx) result ret) (t/with-assumption result (t/assumption arg)) ret))
        ret))))

(defn- specialize-call-return [ctx node arity arg-types]
  (if-let [steps (:composition arity)]
    (let [[input default] (if-let [captured (:captured-args arity)]
                            (into captured arg-types)
                            arg-types)]
      (run-composition ctx node steps input default))
    (specialize-map-return ctx arity (first arg-types))))

(defn var-type
  "Type of a var. A function with no declared return type gets it inferred from its body, once."
  [ctx sym]
  (if-let [v (get-in (:env ctx) [:vars sym])]
    (let [infer-return? (and (= :hints (:source v)) (some #(= :unknown (:t (:ret %))) (:arities (:type v))))
          shape-candidate? (and (#{:hints :sig :fdef} (:source v))
                                (= 1 (get-in ctx [:env :root-definitions sym]))
                                (not (dynamic-definition? v))
                                (= 1 (count (:arities (:type v))) (count (:arities (:parts v))))
                                (not (get-in v [:type :arities 0 :rest]))
                                (not (get-in v [:type :arities 0 :kwargs]))
                                (= 1 (count (get-in v [:type :arities 0 :params]))))]
      (cond
        (not (or infer-return? shape-candidate?)) (:type v)
        (contains? @*rets* sym) (get @*rets* sym)
        :else
        (do (swap! *rets* assoc sym (:type v)) ; a recursive call sees the declared part only
            (let [fctx {:env (:env ctx) :file (:pfile v) :opts (:opts ctx) :locals {} :quiet true}
                  ars (mapv (fn [a {:keys [params body]}]
                              (if (and (not shape-candidate?) (not= :unknown (:t (:ret a))))
                                a
                                (let [{:keys [fixed rest]} (env/split-params params)
                                      c (reduce (fn [c [p ty]] (bind-pattern c p ty)) fctx (map vector fixed (:params a)))
                                      c (if rest (bind-pattern c rest (or (:kwargs a) t/unknown)) c)
                                      a (if (and infer-return? (= :unknown (:t (:ret a))))
                                          (let [ret (binding [*types* nil *trust* (atom {:cast 0 :any 0 :ignore 0}) *ignores* (atom [])]
                                                      (infer-body c body))]
                                            (assoc a :ret (widen (dissoc ret :fresh)) :inferred true))
                                          a)
                                      param (first fixed)
                                      expr (first body)
                                      edits (when (and shape-candidate? (not rest) (= :sym (:op param)) (= 1 (count body))
                                                       (not-any? :directives (tree-seq (comp seq :children) :children expr)))
                                              (seq (infer-map-edits c (:val param) expr)))]
                                  (cond-> a edits (assoc :map-edits (vec edits))))))
                            (:arities (:type v)) (:arities (:parts v)))
                  ty (assoc (:type v) :arities ars)]
              (swap! *rets* assoc sym ty)
              ty))))
    (when-let [{:keys [file node]} (get-in ctx [:env :atom-initializers sym])]
      (or (get @*rets* sym)
          (do (swap! *rets* assoc sym t/unknown)
              (let [ty (quietly {:env (:env ctx) :file file :opts (:opts ctx) :locals {} :load-time? true} node)]
                (swap! *rets* assoc sym ty)
                ty))))))

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
       (and (= :sym (:op node))
            (or (contains? (:hinted ctx) (:val node))
                (compiler-static-field-class ctx (:val node))))
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

(def ^:private array-constructors
  {'boolean-array "[Z" 'byte-array "[B" 'char-array "[C" 'short-array "[S"
   'int-array "[I" 'long-array "[J" 'float-array "[F" 'double-array "[D"
   'object-array "[Ljava.lang.Object;"})

(defn- clone-compiler-class [class-name]
  (when-let [^Class array (some-> class-name t/load-class)]
    (when-let [^Class component (.getComponentType array)]
      ;; RT.aclone has one Object[] overload for all reference arrays.
      (if (.isPrimitive component) class-name "[Ljava.lang.Object;"))))

(defn- compiler-value-class [ctx node]
  (if-let [tag (env/tag-node node)]
    (env/hint-class (:file ctx) tag)
    (if (= :sym (:op node))
      (or (get-in ctx [:compiler-local-classes (:val node)])
          (compiler-static-field-class ctx (:val node)))
      (let [head (builtin-head ctx node)]
        (if (and (= 'aclone head) (= 2 (count (:children node))))
          (clone-compiler-class (compiler-value-class ctx (second (:children node))))
          (get array-constructors head))))))

(defn- local-callback [ctx node]
  {:ctx ctx :node node :bodies (fn-bodies node)
   :types (atom {}) :checked-branches (volatile! #{})})

(defn- callback-local-type [callback]
  (with-meta {:t :fn} {::deferred-callback callback}))

(defn- callback-scope [{:keys [ctx group-scope]}]
  (if group-scope @group-scope ctx))

(defn- bind-pairs [ctx bindings & [on-init]]
  (reduce (fn [ctx [p init]]
            (let [sym (when (= :sym (:op p)) (:val p))
                  pending (:pending-callbacks ctx)
                  defer? (and sym pending (not (env/tag-node p)) (not (env/tag-node init))
                              (empty? (:directives init)))
                  callback (when defer?
                             (or (deferred-callback ctx init)
                                 (when (inline-callback? ctx init)
                                   (let [callback (local-callback (dissoc ctx :pending-callbacks) init)]
                                     (vswap! pending conj callback)
                                     callback))))
                  ty (if callback
                       (callback-local-type callback)
                       (dissoc (infer ctx init) :fresh))
                  _ (when on-init (on-init ctx p init ty))
                  why (why-of ctx init ty)
                  captured (when (and sym (not (env/tag-node p))) (condition-capture ctx init))
                  compiler-class (when (and sym (not (env/tag-node p))) (compiler-value-class ctx init))
                  ctx' (bind-pattern ctx p ty why (stable-path ctx init))]
              (when-not callback (record! ctx p (get-in ctx' [:locals (:val p)] ty)))
              (cond-> ctx'
                captured (assoc-in [:conditions sym] captured)
                compiler-class (assoc-in [:compiler-local-classes sym] compiler-class)
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

(defn- body-initializers [ctx form]
  (case (builtin-head ctx form)
    (def defonce) (when-let [name (env/initialized-var (:file ctx) form)] [name])
    do (mapcat #(body-initializers ctx %) (args-of form))
    time (let [[expr & more] (args-of form)]
           (when (and expr (empty? more)) (body-initializers ctx expr)))
    clojure.test/with-test (when-let [definition (first (args-of form))]
                             (body-initializers ctx definition))
    nil))

(defn infer-body
  ([ctx forms] (infer-body ctx forms nil))
  ([ctx forms expected]
   (loop [[form & more] forms, ctx ctx, result t/nil-t]
     (if form
       (let [reachable? (not= :never (:t result))
             ;; Keep diagnostics for later forms without adding dead loop edges.
             inferred (infer-with-expected (cond-> ctx (not reachable?) (dissoc :recur-types))
                                           form (when-not (seq more) expected))
             result (if reachable? inferred result)
             initialized (when (not= :never (:t result))
                           (seq (body-initializers ctx form)))]
         (recur more (cond-> ctx initialized (update :initialized-vars (fnil into #{}) initialized)) result))
       result))))

(defn- fn-bodies [node]
  (if (= :fn-lit (:op node))
    (when-let [params (fn-lit-parameters node)]
      [(assoc params :body [{:op :list :children (:children node) :pos (:pos node) :synthetic true}]
                     :reader? true)])
    (let [more (rest (:children node))
          more (if (= :sym (:op (first more))) (rest more) more)
          bodies (if (= :vector (:op (first more))) [more]
                     (for [l more :when (= :list (:op l))] (:children l)))]
      (mapv (fn [[params & body]] (assoc (env/split-params params) :body body)) bodies))))

(defn- infer-fn-branch [ctx {:keys [fixed rest body reader?]} contracts]
  (let [env (:env ctx)
        file (:file ctx)
        kwargs (when (= :map (:op rest)) (env/pattern-type env file rest))
        ptypes (mapv (fn [i p]
                       (if (seq contracts)
                         (apply t/union (map #(nth (:params %) i) contracts))
                         (env/pattern-type env file p)))
                     (range) fixed)
        c (reduce (fn [c [i p ty]]
                    (let [bound (cond-> (bind-pattern c p ty)
                                  (and (seq contracts) (= :sym (:op p)))
                                  (assoc-in [:contextual-params (:val p)] {:source :callback :parameter (inc i) :type ty}))]
                      (when (seq contracts)
                        (record! bound p (get-in bound [:locals (:val p)] ty)))
                      bound))
                  ctx (map vector (range) fixed ptypes))
        c (cond-> c (and reader? (seq ptypes)) (bind '% (first ptypes)))
        rest-of (when (and rest (not kwargs))
                  (if (seq contracts)
                    (apply t/union (mapcat #(concat (drop (count fixed) (:params %))
                                                   (when-let [r (:rest %)] [r])) contracts))
                    t/unknown))
        tail {:t :coll :kind :seq :of rest-of :min-count 1}
        c (if rest
            (bind-pattern c rest (cond kwargs kwargs
                                       (not (seq contracts)) t/unknown
                                       (= t/never rest-of) t/nil-t
                                       (some #(= (count fixed) (t/arity-minimum %)) contracts) (t/union tail t/nil-t)
                                       :else tail))
            c)]
    (cond-> {:params (if (seq contracts)
                       (mapv #(or (env/hint-type file %1) %2) fixed ptypes)
                       ptypes)
             :rest rest-of
             :ret (let [ret (infer-body c body)] (if (seq contracts) (eff ctx ret) ret))}
      kwargs (assoc :kwargs kwargs))))

(defn- callback-branch-shape [ctx {:keys [fixed rest]}]
  (let [{:keys [env file]} ctx
        kwargs? (= :map (:op rest))]
    (cond-> {:params (mapv #(env/pattern-type env file %) fixed)
             :ret t/unknown}
      kwargs? (assoc :kwargs (env/pattern-type env file rest))
      (and rest (not kwargs?)) (assoc :rest t/unknown))))

(defn- recursive-callback-type [ctx bodies positional? expected]
  (let [shape (with-meta {:t :fn :arities (mapv #(callback-branch-shape ctx %) bodies)}
                {::t/dispatch positional?})]
    ;; The callback's caller supplies types for its own calls. Recursion can
    ;; also use other arities implemented by the function.
    (if expected (vary-meta expected assoc ::recursive-callback-shape shape) shape)))

(defn- infer-callback [ctx node expected & [{:keys [bodies checked-branches finishing?]}]]
  (let [bodies (or bodies (fn-bodies node))
        shapes (mapv (fn [i {:keys [fixed rest]}]
                       {:i i :params (vec (repeat (count fixed) t/unknown)) :rest (when rest t/unknown)})
                     (range) bodies)
        positional? (not-any? #(= :map (:op (:rest %))) bodies)
        targets (:arities expected)
        plans (when positional? (mapv #(t/dispatch-plan shapes %) targets))
        contracts (group-by (comp :i first) (mapcat identity plans))
        branch-selected? (cond
                           finishing? (complement @checked-branches)
                           (and checked-branches expected positional?) contracts
                           :else (constantly true))
        name-node (second (:children node))
        name (when (and (= :list (:op node)) (= :sym (:op name-node))) (:val name-node))
        self-type (when name (recursive-callback-type ctx bodies positional? expected))
        siblings (when (and name positional? (> (count bodies) 1)) (volatile! #{}))
        ;; Bound repeated exploration of different arities even on an acyclic path.
        budget (when siblings (volatile! 64))
        ctx (dissoc ctx :recur-types :load-time?)]
    (letfn [(check-branch [analysis i contracts active]
              (when (and checked-branches (not (:quiet analysis)))
                (vswap! checked-branches conj i))
              (infer-fn-branch
               (if self-type
                 (bind analysis name
                       (cond-> self-type
                         siblings (vary-meta assoc ::contextual-self-call (partial check-sibling (conj active i)))))
                 analysis)
               (nth bodies i) contracts))
            (check-sibling [active caller arg-types]
              (when-let [{:keys [i]} (t/dispatch-arity shapes (count arg-types))]
                (when (and (not (contains? active i)) (pos? @budget))
                  (vswap! budget dec)
                  (when (or (:quiet ctx) (not (:quiet caller)))
                    (vswap! siblings conj i))
                  (let [analysis (cond-> ctx (:quiet caller) (assoc :quiet true))
                        arity (check-branch analysis i (:arities (direct-callback-context arg-types)) active)]
                    {:t :fn :arities [arity]}))))]
      (let [indices (range (count bodies))
            ;; Analyze the external entry arities before deciding which siblings
            ;; still need an ordinary check.
            contextual (when siblings
                         (into {} (for [i indices :when (and (branch-selected? i) (contains? contracts i))]
                                    [i (check-branch ctx i (map second (get contracts i)) #{})])))
            arities (mapv (fn [i]
                            (or (get contextual i)
                                (if (and (branch-selected? i)
                                         (not (and siblings (or expected finishing?) (contains? @siblings i))))
                                  (check-branch ctx i (map second (get contracts i)) #{})
                                  (callback-branch-shape ctx (nth bodies i)))))
                          indices)
            ft (with-meta {:t :fn :arities arities} {::t/dispatch positional?})]
        (cond-> ft
          (and positional? (= 1 (count targets) (count shapes)) (nil? (first plans)) (not (:rest (first targets))))
          (vary-meta assoc ::callback-arity-error
                     [(count (:params (first targets))) (count (:params (first shapes)))]))))))

(defn- infer-fn [ctx node arg-types]
  (infer-callback ctx node (when arg-types (direct-callback-context (vec arg-types) false))))

(defn- same-type-metadata? [a b]
  ;; Structural equality of the types has already been established by the lookup.
  (or (identical? a b)
      (and (= (meta a) (meta b))
           (cond
             (map? a) (every? (fn [[k v]] (same-type-metadata? v (get b k))) a)
             (sequential? a) (every? true? (map same-type-metadata? a b))
             :else true))))

(defn- callback-analysis-context [ctx callback]
  (cond-> (callback-scope callback) (:quiet ctx) (assoc :quiet true)))

(defn- infer-deferred-callback [analysis {:keys [node active?] :as callback} expected]
  (if active?
    (do (vreset! active? true)
        (try
          (infer-callback analysis node expected callback)
          (finally (vreset! active? false))))
    (infer-callback analysis node expected callback)))

(defn- force-callback [ctx {:keys [node types active? bodies] :as callback} expected]
  (let [cached @types
        entry (get cached expected)]
    (cond
      (and entry (same-type-metadata? expected (:expected entry)))
      (do (when-let [escapes (seq (:escapes entry))]
            (swap! *trust* (fn [trust] (reduce #(update %1 %2 inc) trust escapes))))
          (:type entry))

      (and active? @active?)
      (recursive-callback-type (callback-scope callback) bodies
                               (not-any? #(= :map (:op (:rest %))) bodies) nil)

      :else
      (let [quiet? (:quiet ctx)
            analysis (callback-analysis-context ctx callback)
            before @*trust*
            ty (record! analysis node (infer-deferred-callback analysis callback expected))]
        (when-not quiet?
          (let [after @*trust*
                escapes (when-not (identical? before after)
                          (filterv #(> (get after % 0) (get before % 0)) [:cast :any]))]
            ;; t/satisfies must see the same evidence on a cache hit.
            (swap! types assoc expected {:expected expected :type ty :escapes escapes})))
        ty))))

(defn- finish-callback! [ctx {:keys [bodies types checked-branches] :as callback}]
  (when (< (count @checked-branches) (count bodies))
    (if (empty? @types)
      (force-callback ctx callback nil)
      (infer-deferred-callback (callback-analysis-context ctx callback)
                               (assoc callback :finishing? true) nil))))

(defn- apply-type
  "Result of calling a function type with these argument types; mismatches are reported at `at`."
  [ctx ftype arg-types at label & [require-match?]]
  (if (= :all (:t ftype))
    (call-generic ctx at label ftype nil arg-types)
    (if-let [fits (seq (matching-arities ftype (count arg-types)))]
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
      (cond (and vague? (next (distinct (map :ret ok)))) t/unknown
            (seq ok) (or (specialize-call-return ctx at ar arg-types) t/unknown)
            require-match? t/unknown
            :else (:ret ar)))
    (if (seq (:arities ftype))
      (do (report! ctx at "TSP2554" (str label " is called with " (count arg-types) " argument(s)")) t/unknown)
      t/unknown))))

(defn- positional-callback-index [ctx node]
  (when (and (= :sym (:op node))
             (empty? (:directives node)) (not (env/tag-node node))
             (not (:guards-disabled? ctx)))
    (let [core-var (get '{first clojure.core/first second clojure.core/second}
                        (t/core-name (:file ctx) (:val node)))]
      (when (and core-var (= core-var (guard-symbol ctx node)))
        (if (= 'clojure.core/first core-var) 0 1)))))

(defn- checked-sequence-position [ctx node label input index]
  (let [input (eff ctx input)]
    (if-let [bad (unsupported-sequence-member ctx input)]
      (report! ctx node "TSP2345" (str label " cannot read a position from " (t/show bad)))
      (if (= t/never input) t/never (sequence-position-type ctx input index)))))

(defn- entry-callback-index [ctx node]
  (when (and (= :sym (:op node))
             (empty? (:directives node)) (not (env/tag-node node)))
    (let [head (t/core-name (:file ctx) (:val node))]
      (when (and (#{'key 'val} head)
                 (= (symbol "clojure.core" (name head)) (guard-symbol ctx node)))
        (if (= 'key head) 0 1)))))

(defn- checked-entry-position [ctx node label input index]
  (let [env (:env ctx)
        input (eff ctx input)
        members (t/members env input)
        expected (t/class-type "java.util.Map$Entry")
        vector-literal? (and (= :vector (:op node)) (empty? (:directives node)) (not (env/tag-node node)))
        bad (some (fn [member]
                    (let [possible-entry? (or (and (= :prim (:t member)) (#{:vector :coll} (:name member)))
                                              (and (= :coll (:t member)) (#{nil :vector} (:kind member))
                                                   (not (:jvm-class member))))]
                      (when (and (not (#{:nil :never} (:t member)))
                                 (not (:map-entry? member))
                                 (or vector-literal? (not possible-entry?))
                                 (= :mismatch (:status (t/sub-result env member expected))))
                        member)))
                  members)]
    (cond
      (= t/never input) t/never
      bad (do (report! ctx node "TSP2345" (str label " expects a Map.Entry, got " (t/show bad))) t/unknown)
      (t/nilable? env input)
      (do (report! ctx node "TSP2531" (str label " may receive a nil Map.Entry")) t/unknown)
      (every? :map-entry? members) (positional-type ctx input index)
      :else t/unknown)))

(defn- entry-access [ctx node label index]
  (let [[arg :as args] (args-of node)
        inputs (mapv #(infer ctx %) args)]
    (if (= 1 (count inputs))
      (checked-entry-position ctx arg label (first inputs) index)
      (do (report! ctx node "TSP2554" (str label " expects one argument")) t/unknown))))

(defn- noncallable-scalar? [ty]
  (case (:t ty)
    :nil true
    :lit (not (ifn? (:val ty)))
    :prim (contains? #{:string :int :double :boolean} (:name ty))
    :union (every? noncallable-scalar? (:types ty))
    false))

(defn apply-fn-node [ctx fnode arg-types & [require-match?]]
  (case (:op fnode)
    :kw (read-key ctx (first arg-types) (:val fnode) fnode)
    :set (t/union (first arg-types) t/nil-t)
    (let [ty (if (callback-node? ctx fnode)
               (infer ctx fnode (direct-callback-context arg-types false))
               (infer ctx fnode))
          ty (if (= :ref (:t ty)) (t/resolve-t (:env ctx) ty) ty)
          ty (contextual-call-target ctx ty arg-types)
          index (positional-callback-index ctx fnode)
          entry-index (when-not index (entry-callback-index ctx fnode))]
      (cond
        (some? index)
        (let [label (:val fnode)]
          (if (= 1 (count arg-types))
            (or (checked-sequence-position ctx fnode label (first arg-types) index) t/unknown)
            (do (report! ctx fnode "TSP2554" (str label " expects one argument")) t/unknown)))

        (some? entry-index)
        (let [label (:val fnode)]
          (if (= 1 (count arg-types))
            (checked-entry-position ctx fnode label (first arg-types) entry-index)
            (do (report! ctx fnode "TSP2554" (str label " expects one argument")) t/unknown)))

        (and (noncallable-scalar? ty) (not-any? #{t/never} arg-types))
        (do (report! ctx fnode "TSP2345" (str "callback expects a callable value, got " (t/show ty)))
            t/unknown)

        :else (apply-type ctx ty arg-types fnode (parse/unparse fnode) require-match?)))))

(defn- function-shape [ctx ty]
  (let [ty (t/resolve-t (:env ctx) (eff ctx ty))]
    (when (and (= :fn (:t ty)) (seq (:arities ty)) (not-any? :kwargs (:arities ty)))
      ty)))

(defn- unchecked-hof! [ctx node label reason]
  (report! ctx node "TSP210-01" (str label " (" (name reason) ")")
           :severity (if (:strict (:opts ctx)) :error :warning)
           :reasons #{reason}))

(defn- check-composable! [ctx node ty label]
  (let [ty (t/resolve-t (:env ctx) ty)]
    (if (or (= :nil (:t ty))
            (#{:string :int :double :number :boolean} (:name (if (= :lit (:t ty)) (t/lit-prim (:val ty)) ty))))
      (report! ctx node "TSP2345" (str label " expects a function, got " (t/show ty)))
      (unchecked-hof! ctx node (str label ": function signature not checked")
                      (if (= :unknown (:t ty)) :unknown :unsupported-function)))))

(defn- disjoint-scalars? [env a b]
  (let [scalar (fn [ty]
                 (case (:t ty)
                   :nil :nil
                   :lit (:name (t/lit-prim (:val ty)))
                   :prim (:name ty)
                   nil))
        domains #{:nil :string :keyword :symbol :boolean :number :int :double}
        numeric #{:number :int :double}]
    (every? (fn [b]
              (let [left (scalar a) right (scalar b)]
                (or (and (= :lit (:t a) (:t b)) (not= (:val a) (:val b)))
                    (and (domains left) (domains right) (not= left right)
                         (not (and (numeric left) (numeric right)))))))
            (t/members env b))))

(defn- comp-member [ctx stage fits input]
  ;; Earlier overloads may apply to part of a broad input. Keep their results
  ;; until an overload covers the entire input, without assuming disjoint maps.
  (let [env (:env ctx)
        param #(or (first (:params %)) (:rest %))]
    (loop [[a & more] fits returns [] unchecked #{}]
      (cond
        (= :never (:t input)) {:type t/never}
        (nil? a) {:reasons unchecked}
        :else
        (let [expected (param a)
              result (t/sub-result env input expected)]
          (if (= :proved (:status result))
            (if (= :any (:t input))
              {:type (apply t/union (map :ret fits))}
              (if-let [ret (specialize-call-return ctx stage a [input])]
                {:type (apply t/union (conj returns ret))}
                {:reported? true}))
            (recur more
                   (cond-> returns (not (disjoint-scalars? env input expected)) (conj (:ret a)))
                   (into unchecked (:reasons result)))))))))

(defn- report-comp-arity! [ctx stage]
  (report! ctx stage "TSP2554" "comp: every function except the rightmost must accept one argument"))

(defn- comp-link [ctx stage ftype input]
  (if-let [fits (seq (matching-arities ftype 1))]
    (let [input (eff ctx input)
          parts (mapv #(comp-member ctx stage fits %) (t/members (:env ctx) input))]
      (if-let [failure (first (remove :type parts))]
        (when-not (:reported? failure)
          (if-let [reason (first (sort (:reasons failure)))]
            (unchecked-hof! ctx stage "comp: input compatibility not checked" reason)
            (report! ctx stage "TSP2345"
                     (str "comp: " (show-ref ctx input) " is not accepted by " (t/show ftype)))))
        (apply t/union (map :type parts))))
    (report-comp-arity! ctx stage)))

(defn- literal-set-members [node]
  (when (and (= :set (:op node)) (empty? (:directives node)) (not (env/tag-node node))
             (every? literal-node? (:children node)))
    (mapv :val (:children node))))

(defn- set-call-result [ctx values input]
  (let [env (:env ctx)
        by-value (zipmap values (map t/lit values))]
    (t/with-assumption
     (apply t/union
            (for [member (t/members env (eff ctx input))]
              (case (:t member)
                :never t/never
                :nil t/nil-t
                :lit (get by-value (:val member) t/nil-t)
                (apply t/union t/nil-t
                       (remove #(disjoint-scalars? env member %) (vals by-value))))))
     (t/assumption input))))

(defn- typed-set-call-result [ctx ty input]
  (if (= t/never input)
    t/never
    (let [env (:env ctx)
          inputs (t/members env (eff ctx input))]
      (t/with-assumption
       (apply t/union t/nil-t
              (remove (fn [stored] (every? #(disjoint-scalars? env % stored) inputs))
                      (t/members env (or (:of ty) t/unknown))))
       (or (t/assumption ty) (t/assumption input))))))

(defn- composition-projection [ctx node ty]
  (let [index (positional-callback-index ctx node)
        values (literal-set-members node)]
    (cond
      values {:kind :set :values values}
      (or (and (= :coll (:t ty)) (= :set (:kind ty))) (= (t/prim :set) (t/strip ty)))
      {:kind :typed-set :type ty}
      (and (= :lit (:t ty)) (keyword? (:val ty))) {:kind :key :key (:val ty)}
      (some? index) {:kind :position :index index :label (parse/unparse node)})))

(defn- composition-stage [ctx node ty]
  (let [ty (t/resolve-t (:env ctx) (eff ctx ty))]
    (or (composition-projection ctx node ty)
        (if (= :all (:t ty))
          {:kind :generic :type ty :label (parse/unparse node)}
          (if-let [shape (function-shape ctx ty)]
            {:kind :function :type shape}
            (check-composable! ctx node ty "comp"))))))

(defn- composition-step [ctx node {:keys [kind values key index label type]} input default]
  (case kind
    :set (set-call-result ctx values input)
    :typed-set (typed-set-call-result ctx type input)
    :key (read-key ctx (eff ctx input) key node (or default t/nil-t) nil)
    :position (checked-sequence-position ctx node (str "comp: " label) input index)
    :generic (if (= t/never input)
               t/never
               (let [ret (call-generic ctx node (str "comp: " label) type nil [input])]
                 (when-not (:generic-failure ret) ret)))
    :function (comp-link ctx node type input)))

(defn- run-composition
  ([ctx node steps input] (run-composition ctx node steps input nil))
  ([ctx node steps input default]
   (loop [[step & more] steps, ty input, default default]
     (if (and step ty)
       (recur more (composition-step ctx node step ty default) nil)
       ty))))

(def ^:private positional-composition-input
  (apply t/union t/nil-t (t/prim :coll)
         (map t/class-type ["clojure.lang.Seqable" "java.lang.Iterable" "java.lang.CharSequence" "java.util.Map"
                            "[Z" "[B" "[C" "[S" "[I" "[J" "[F" "[D" "[Ljava.lang.Object;"])))

(defn- composition-entry [projection]
  (when-let [params (case (:kind projection)
                     :position [[positional-composition-input]]
                     :key [[t/any] [t/any t/any]]
                     (:set :typed-set) [[t/any]]
                     nil)]
    {:t :fn :arities (mapv #(hash-map :params % :ret t/unknown :composition [projection]) params)}))

(defn- unary-composition-stage? [{:keys [kind type]}]
  (case kind
    :function (seq (matching-arities type 1))
    :generic (seq (matching-arities (:body type) 1))
    true))

(defn- compose-shape [ctx inner stage]
  (let [arities (:arities inner)
        deferred? (some :composition arities)
        expected (when (and inner (not deferred?) (callback-node? ctx stage))
                   {:t :fn :arities [{:params [(apply t/union (map (comp #(eff ctx %) :ret) arities))]
                                      :ret t/unknown}]})
        ty (if expected (infer ctx stage expected) (infer ctx stage))]
    (when inner
      (when-let [step (composition-stage ctx stage ty)]
        (if (and deferred? (not (unary-composition-stage? step)))
          (report-comp-arity! ctx stage)
          (let [arities (mapv (fn [arity]
                                (if (:composition arity)
                                  (update arity :composition conj step)
                                  (when-let [ret (composition-step ctx stage step (:ret arity) nil)]
                                    (assoc (dissoc arity :map-edits) :ret ret))))
                              arities)]
            (when (every? some? arities) (assoc inner :arities arities))))))))

(defn- compose-functions [ctx nodes]
  (let [[right & stages] (rseq nodes)
        ty (infer ctx right)
        resolved (t/resolve-t (:env ctx) (eff ctx ty))
        entry (composition-entry (composition-projection ctx right resolved))]
    (if-not (seq stages)
      (or entry ty)
      (let [generic? (= :all (:t resolved))
            shape (cond
                    entry entry
                    generic? (update-in (:body resolved) [:arities 0]
                                        #(assoc % :ret t/unknown :composition (or (:composition %) [])))
                    :else (or (function-shape ctx resolved) (check-composable! ctx right resolved "comp")))
            composed (reduce (partial compose-shape ctx) shape stages)]
        (if (and generic? composed)
          ;; Instantiation checks the original parameters and result first.
          ;; The public shape carries the later stages and any partial captures.
          (assoc resolved :body composed :partial-target (or (:partial-target resolved) resolved))
          composed)))))

(defn- partial-generic [ctx node ty args]
  (let [target (or (:partial-target ty) ty)
        captured (or (:captured-args ty) [])
        state (solver/instantiate target)
        [arity & overloads] (:arities (:type state))
        remaining (- (count (:params arity)) (count captured))]
    (cond
      (or (nil? arity) (seq overloads) (:rest arity) (:kwargs arity))
      (do (run! #(infer ctx %) args)
          (unchecked-hof! ctx node "partial: generic overloads and variadic contracts not checked" :unsupported-function))

      (> (count args) remaining)
      (do (run! #(infer ctx %) args)
          (report! ctx node "TSP2554" (str "partial: no arity accepts " (count args) " captured argument(s)")))

      :else
      (let [entries (concat (map #(hash-map :type %) captured) (map #(hash-map :node %) args))
            [state types] (constrain-generic-arguments ctx state arity entries)
            issue (first (filter #(= :mismatch (:status %)) (:issues (solver/solve (:env ctx) state))))]
        (cond
          (some #{t/never} types) t/never
          issue (let [i (get-in issue [:origin :argument])
                      at (if (and (nat-int? i) (<= (count captured) i))
                           (or (nth args (- i (count captured)) nil) node)
                           node)]
                  (report! ctx at "TSP2345"
                           (str "partial: " (t/show (:left issue)) " is not assignable to " (t/show (:right issue)))))
          :else
          ;; Keep the original obligations until the remaining arguments arrive.
          ;; Captures are part of type identity so a union cannot discard them.
          (-> ty
              (assoc :partial-target target :captured-args types)
              (update-in [:body :arities 0 :params] #(vec (drop (count args) %)))))))))

(defn- partial-from-shape [ctx node shape args]
  (let [n (count args)
        fits (filterv #(or (:rest %) (<= n (count (:params %)))) (:arities shape))
        inputs (infer-call-arguments ctx node args fits)
        candidates (mapv (fn [arity] [arity (vec (arg-problems ctx arity args inputs node))]) fits)
        valid (filterv (comp empty? second) candidates)
        incomplete (some (fn [[_ ps]] (when (and (seq ps) (every? #(= "TSP210-01" (first (second %))) ps)) ps))
                         candidates)
        rest-minimum (when (::t/dispatch (meta shape))
                       (some (fn [[a _]] (when (:rest a) (t/arity-minimum a))) valid))
        blocked (when rest-minimum
                  (into #{} (keep (fn [[a ps]]
                                    (when (and (seq ps) (not (:rest a))) (count (:params a)))))
                        candidates))]
    (cond
      (empty? fits) (report! ctx node "TSP2554" (str "partial: no arity accepts " n " captured argument(s)"))
      (or incomplete (empty? valid))
      (doseq [[at [code message extra]] (or incomplete (second (apply min-key (comp count second) candidates)))]
        (apply report! ctx at code message (mapcat identity extra)))
      (some #(> % rest-minimum) blocked)
      (unchecked-hof! ctx node "partial: residual dispatch not checked" :unsupported-function)
      :else
      (assoc shape :arities
             (mapv (fn [[arity _]]
                     ;; A rejected fixed branch at the variadic minimum still
                     ;; wins at runtime. The remaining rest arity starts above it.
                     (let [minimum (t/arity-minimum arity)
                           minimum (if (and (:rest arity) (contains? blocked minimum)) (inc minimum) minimum)
                           remaining (max 0 (- minimum (max n (count (:params arity)))))
                           residual (-> arity
                                        (update :params #(vec (drop n %)))
                                        (dissoc :rest-min :map-edits :composition :captured-args))
                           deferred? (and (:composition arity) (or (seq (:params residual)) (next valid)))]
                       (cond-> residual
                         deferred? (assoc :composition (:composition arity)
                                          :captured-args (into (or (:captured-args arity) []) inputs))
                         (and (not deferred?) (or (:map-edits arity) (:composition arity)))
                         (assoc :ret (or (specialize-call-return ctx node arity inputs) t/unknown))
                         (pos? remaining) (assoc :rest-min remaining))))
                   valid)))))

(defn- partial-function [ctx node f args]
  (let [ty (t/resolve-t (:env ctx) (eff ctx (infer ctx f)))]
    (if (= :all (:t ty))
      (partial-generic ctx node ty args)
      (if-let [shape (function-shape ctx ty)]
        (partial-from-shape ctx node shape args)
        (do (run! #(infer ctx %) args)
            (check-composable! ctx f ty "partial"))))))

(defn- single-map [ctx ty]
  (let [ms (t/members (:env ctx) ty)]
    (when (and (= 1 (count ms)) (= :map (:t (first ms)))) (first ms))))

(defn- check-key-value!
  "Key = type: a value stored under a qualified key must fit the spec of that key."
  [ctx k vnode vtype]
  (when (and (qualified-keyword? k) (get-in (:env ctx) [:specs k]))
    (let [actual (eff ctx vtype) expected (t/ref-t k)
          result (t/sub-result (:env ctx) actual expected)]
      (case (:status result)
        :inconclusive (let [[code msg details] (incomplete-problem ctx actual expected result)]
                        (apply report! ctx vnode code msg (mapcat identity details)))
        :mismatch (if-let [[code msg details] (nested-platform-problem ctx "TSP2322" actual expected result)]
                    (apply report! ctx vnode code msg (mapcat identity details))
                    (report! ctx vnode "TSP2322" (str (t/show actual) " is not assignable to " (show-ref ctx expected))))
        :proved nil))))

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

(defn- functional-return-type [ctx {:keys [ret clojure-comparator?]}]
  (cond
    clojure-comparator? (t/union (t/prim :number) (t/prim :boolean))
    (= "void" ret) nil
    ;; Reflector uses Clojure truthiness, while a direct adapter unboxes Boolean.
    (= "boolean" ret) (when (:java-direct? ctx) (t/prim :boolean))
    (= "char" ret) (t/class-type "java.lang.Character")
    (t/primitives ret) (t/union (t/prim :number) (t/class-type "java.lang.Character"))
    :else (t/union (t/class-type ret) t/nil-t)))

(defn- functional-input-type [cname]
  (if (= "java.lang.Object" cname) t/unknown (t/class-type cname)))

(defn- functional-parameter-problem [ctx params arity name]
  (let [env (:env ctx)
        expected (concat (:params arity) (repeat (:rest arity)))
        numeric? #(t/sub? env % (t/prim :number))]
    (some (fn [[i cname target]]
            (let [actual (functional-input-type cname)]
              (when (and target
                         (= :mismatch (:status (t/sub-result env actual target)))
                         (not (and (t/primitives cname) (t/primitives (:as target))
                                   (numeric? actual) (numeric? target))))
                ["TSP2345" (str "passes " (t/show actual) " to callback parameter " i
                               " for " name ", which expects " (t/show target))])))
          (map vector (iterate inc 1) params expected))))

(defn- functional-argument-problem [ctx fn-type {:keys [params name] :as method}]
  (let [n (count params)]
    (if-let [arity (t/dispatch-arity (:arities fn-type) n)]
      (or (functional-parameter-problem ctx params arity name)
          (when-let [expected (functional-return-type ctx method)]
            (let [actual (eff ctx (:ret arity))]
              (when (= :mismatch (:status (t/sub-result (:env ctx) actual expected)))
                ["TSP2345" (str "expects a callback returning " (t/show expected) " for " name ", got " (t/show actual))]))))
      ["TSP2554" (str "expects a callback accepting " n " argument(s) for " name)])))

(defn- java-arg-ok? [ctx a pclass]
  (let [p (t/class-type pclass) env (:env ctx)
        actual (t/resolve-t env a)
        known-fn? (or (= :fn (:t actual)) (some #(= :fn (:t %)) (:types actual)))
        num? (fn [x] (t/sub? env x (t/prim :number)))]
    (if-let [method (when known-fn? (java/functional-method pclass))]
      (every? (fn [member]
                (if (= :fn (:t member))
                  (nil? (functional-argument-problem ctx member method))
                  (t/sub? env member p)))
              (t/members env (t/remove-nil env actual)))
      (or (#{:unknown :any} (:t actual))
          (t/sub? env (if (t/primitives pclass) a (t/remove-nil env a)) p)
          (and (num? (t/remove-nil env a)) (num? p))
          (and (= "char" pclass) (= :class (:t a)))))))

(defn- java-ret [cname m]
  (let [base (t/class-type (:ret m))
        shown (java/short-name (:ret m))]
    (cond
      (java/nullable-return? cname (:name m))
      (with-meta (t/union base t/nil-t) {:null-return (str (java/short-name (:declaring m)) "." (:name m) " may return null")})
      (#{:nil :any} (:t base)) base
      :else (t/platform base shown))))

(defn- java-rejected-args [ctx alternatives params]
  (into []
        (keep-indexed (fn [i [members pclass]]
                        (when-not (some #(java-arg-ok? ctx % pclass) members) i)))
        (map vector alternatives params)))

(defn- java-unmatched-argument [ctx alternatives fits]
  (first (keep-indexed
          (fn [i members]
            (when (some (fn [member]
                          (not-any? #(java-arg-ok? ctx member (nth (:params %) i)) fits))
                        members)
              i))
          alternatives)))

(defn- java-rejection-summary [[params rejected]]
  (let [param-name #(or (some-> ^Class (t/load-class %) .getTypeName java/short-name) %)]
    (str "(" (str/join ", " (map param-name params)) ") rejects "
         (if (next rejected) "arguments " "argument ")
         (str/join ", " (map inc rejected)))))

(defn- report-java-argument-mismatch! [ctx node method-name arg-nodes arg-types fits]
  (let [alternatives (mapv #(t/members (:env ctx) %) arg-types)]
    (if-let [i (java-unmatched-argument ctx alternatives fits)]
      (let [pclass (nth (:params (first fits)) i)
            members (remove #(= :nil (:t %)) (nth alternatives i))
            methods (when (and (seq members) (every? #(= :fn (:t %)) members))
                      (distinct (keep #(java/functional-method (nth (:params %) i)) fits)))
            [code message] (when (= 1 (count methods))
                             (some #(functional-argument-problem ctx % (first methods)) members))]
        (if code
          (report! ctx (nth arg-nodes i) code (str method-name " " message))
          (report! ctx (nth arg-nodes i) "TSP2345"
                   (str (t/show (nth arg-types i)) " is not assignable to " (java/short-name pclass) ", "
                        (if (= 1 (count fits)) "the parameter" "a parameter") " of " method-name))))
      ;; A union can be accepted by different overloads at runtime.
      (let [rejected (mapv #(java-rejected-args ctx alternatives (:params %)) fits)]
        (when (every? seq rejected)
          (let [closest (->> (map vector (map :params fits) rejected)
                             (sort-by (fn [[params args]] [(count args) params]))
                             (take 2))]
            (report! ctx node "TSP2345"
                     (str "No overload of " method-name " accepts these arguments. "
                          (str/join ". " (map java-rejection-summary closest))))))))))

(defn- ambiguous-direct-callback [ctx arg-nodes arg-types fits]
  (when (and (:java-direct? ctx) (next fits))
    (let [args (mapv vector arg-nodes arg-types)
          callbacks (keep-indexed (fn [i [node ty]]
                                    (when (and (= :fn (:t ty)) (not (env/tag-node node))
                                               (inline-callback? ctx node))
                                      i)) args)]
      (when (= 1 (count callbacks))
        (let [i (first callbacks)
              same-parameters? (fn [j] (apply = (map #(nth (:params %) j) fits)))
              others-fit? (every? (fn [j]
                                    (or (= i j)
                                        (let [[node ty] (nth args j)]
                                          (and (compiler-knows? ctx node)
                                               (same-parameters? j)
                                               (java-arg-ok? ctx ty (nth (:params (first fits)) j))))))
                                  (range (count args)))]
          (when others-fit?
            (when-let [targets (seq (java/ambiguous-functional-targets (map #(nth (:params %) i) fits)))]
              [(nth arg-nodes i) targets])))))))

(defn- checked-java-overloads [ctx node label arg-nodes arg-types candidates]
  ;; Clojure calls the JVM descriptor directly, with one array argument for varargs.
  (let [fits (filterv #(= (count arg-nodes) (count (:params %))) candidates)]
    (if (empty? fits)
      (report! ctx node "TSP2554"
               (str label " takes " (str/join " or " (distinct (map #(count (:params %)) candidates)))
                    " argument(s), got " (count arg-nodes)))
      (if-let [[arg targets] (ambiguous-direct-callback ctx arg-nodes arg-types fits)]
        (report! ctx arg "TSP2345"
                 (str label " has more than one matching overload for the Clojure callback ("
                      (str/join ", " targets) "). Use Java parameter tags to select one."))
        (let [ctx (cond-> ctx (next fits) (dissoc :java-direct?))
              ok (filterv #(every? boolean (map (partial java-arg-ok? ctx) arg-types (:params %))) fits)]
          (if (seq ok)
            ok
            (do (report-java-argument-mismatch! ctx node label arg-nodes arg-types fits)
                fits)))))))

(defn- java-parameter-tags [member]
  (let [tags (some #(case (:op %)
                      :vector %
                      :map (env/attr-get % :param-tags)
                      nil)
                   (:meta member))]
    (when-not (= :nil (:op tags)) tags)))

(defn- tagged-java-overload [ctx tags label methods]
  (if-not (= :vector (:op tags))
    (report! ctx tags "TSP2345" "Java parameter tags must be a vector")
    (let [tag-classes (mapv #(if (= '_ (:val %)) '_ (env/hint-class (:file ctx) %)) (:children tags))
          matches (filter (fn [{:keys [params]}]
                            (and (= (count params) (count tag-classes))
                                 (every? true? (map #(or (= '_ %1) (= %1 %2)) tag-classes params))))
                          methods)]
      (if (= 1 (count matches))
        matches
        (report! ctx tags "TSP2345" (str "Parameter tags " (parse/unparse tags)
                                        " must identify one overload of " label))))))

(defn- possible-java-argument? [ctx ty pclass]
  (or (nil? ty)
      ;; The other arguments can rule out overloads before callback inference.
      ;; A callback's Clojure arity does not select a JVM descriptor.
      (some #(or (= :fn (:t %)) (java-arg-ok? ctx % pclass))
            (t/members (:env ctx) ty))))

(defn- java-callback-context [fits i]
  (let [methods (mapv #(java/functional-method (nth (:params %) i)) fits)]
    (when (and (seq methods) (every? some? methods) (apply = (map :params methods)))
      {:t :fn :arities [{:params (mapv functional-input-type (:params (first methods)))
                         :ret t/unknown}]})))

(defn- infer-java-arguments [ctx args candidates]
  (let [fits (filterv #(= (count args) (count (:params %))) candidates)
        pending (mapv (fn [i arg]
                        (and (seq fits) (callback-node? ctx arg)
                             (some #(java/functional-method (nth (:params %) i)) fits)))
                      (range) args)]
    (if (some identity pending)
      (let [types (mapv #(when-not %2 (infer ctx %1)) args pending)
            possible (filterv #(every? identity (map (partial possible-java-argument? ctx) types (:params %))) fits)]
        (mapv (fn [i arg ty] (or ty (infer ctx arg (java-callback-context possible i)))) (range) args types))
      (mapv #(infer ctx %) args))))

(defn- java-call [ctx node member-node cname mname static? arg-nodes field-fallback & [tags]]
  (let [ms (map #(assoc % :name mname) (java/methods-named cname mname static?))
        field-class (when (and (empty? arg-nodes)
                               (case field-fallback
                                 :no-methods (empty? ms)
                                 :no-zero-arg-method (not-any? #(empty? (:params %)) ms)
                                 false))
                      (java/field-class cname mname static?))
        shown (java/short-name cname)
        label (str shown "." mname)
        methods (when (and (not field-class) (seq ms)) (if tags (tagged-java-overload ctx tags label ms) ms))
        arg-types (infer-java-arguments ctx arg-nodes methods)]
    (cond
      (not (java/has-class? cname)) t/unknown
      field-class (java-field-type field-class)
      (empty? ms) (do (report! ctx member-node "TSP2339"
                               (str shown " has no " (when static? "static ") "method " mname
                                    (hint-suffix (symbol mname) (map symbol (java/method-names cname)))))
                      t/unknown)
      :else
      (let [candidates (when (seq methods)
                         (checked-java-overloads (cond-> ctx static? (assoc :java-direct? true))
                                                 node label arg-nodes arg-types methods))
            rets (distinct (map #(java-ret cname %) candidates))]
        (if (= 1 (count rets)) (first rets) t/unknown)))))

(defn- constructor-call [ctx node cname arg-nodes]
  (let [^Class cls (when cname (t/load-class cname))
        constructors (when cls (java/constructors cls))
        abstract? (and cls (Modifier/isAbstract (.getModifiers cls)))
        arg-types (infer-java-arguments ctx arg-nodes (when-not abstract? constructors))]
    (if cname
      (do
        (when (some? constructors)
          (let [shown (java/short-name cname)]
            (cond
              (empty? constructors)
              (report! ctx node "TSP2339" (str shown " has no public constructor"))

              abstract?
              (report! ctx node "TSP2345" (str shown " is abstract and cannot be constructed"))

              :else
              (checked-java-overloads (assoc ctx :java-direct? true)
                                      node (str "the " shown " constructor") arg-nodes arg-types constructors))))
        ;; Direct instantiation of Object produces a concrete value.
        (if (= cname "java.lang.Object") {:t :class :name cname} (t/class-type cname)))
      t/unknown)))

(defn- java-receiver-type [ctx target]
  (let [actual (infer ctx target)
        hinted (check-hint! ctx target actual "type of the receiver")
        env (:env ctx)]
    ;; Compatible hints must not erase known nullability or its provenance.
    (eff ctx (if (and hinted
                      (or (#{:unknown :any} (:t actual))
                          (not (t/sub? env (t/remove-nil env actual) hinted))))
               hinted actual))))

(defn- check-nil-receiver! [ctx target ty]
  (when (t/nilable? (:env ctx) ty)
    (report! ctx target "TSP2531"
             (str (if (= :nil (:op target)) "Receiver" (parse/unparse target))
                  (if (= :nil (:t ty)) " is nil" " may be nil")
                  (when-let [w (or (:null-return (meta ty)) (when (= :sym (:op target)) (get-in ctx [:why (:val target)])))]
                    (str ": " w))))))

(defn- qualified-instance-call [ctx node member cname args]
  (let [[target & arg-nodes] args
        mname (munge (subs (name (:val member)) 1))]
    (if-not target
      (do (report! ctx node "TSP2554" (str (:val member) " expects a receiver"))
          t/unknown)
      (let [actual (java-receiver-type ctx target)
            non-nil (t/remove-nil (:env ctx) actual)
            boxed (t/literal-number-class non-nil)]
        (check-nil-receiver! ctx target actual)
        (cond
          boxed (assignable! ctx target {:t :class :name (.getName ^Class boxed)} {:t :class :name cname})
          (class-symbol-name ctx target) (assignable! ctx target (t/class-type "java.lang.Class") (t/class-type cname))
          :else (assignable! ctx target non-nil (t/class-type cname)))
        (java-call (assoc ctx :java-direct? true) node member cname mname false arg-nodes nil (java-parameter-tags member))))))

(defn- instance-field-owner [ctx target]
  (let [ty (java-receiver-type ctx target)]
    (check-nil-receiver! ctx target ty)
    (if (class-symbol-name ctx target) "java.lang.Class" (class-of ctx ty))))

(defn- lookup-java-field [ctx node cname fname static?]
  (when (and cname (java/has-class? cname))
    (or (java/field-info cname fname static?)
        (report! ctx node "TSP2339"
                 (str (java/short-name cname) " has no public "
                      (if static? "static" "instance") " field " fname)))))

(defn- java-field-read [ctx member-node cname fname static?]
  (if-let [^Class field-type (:type (lookup-java-field ctx member-node cname fname static?))]
    (java-field-type (.getName field-type))
    t/unknown))

(defn- instance-field-call [ctx node fname]
  (let [[head target & more] (:children node)]
    (if (or (nil? target) (seq more))
      (do (run! #(infer ctx %) (args-of node))
          (report! ctx node "TSP2554" (str ".-" fname " expects one receiver"))
          t/unknown)
      (java-field-read ctx head (instance-field-owner ctx target) fname false))))

(defn- dot-field-place [node]
  (when (= :list (:op node))
    (let [[head receiver member :as children] (:children node)
          sym (:val head)
          long-dot? (= '. sym)
          raw-name (cond
                     (and long-dot? (= 3 (count children)) (= :sym (:op member)))
                     (name (:val member))

                     (and (= 2 (count children)) (= :sym (:op head)) (simple-symbol? sym)
                          (str/starts-with? (name sym) ".") (not (#{'. '..} sym)))
                     (subs (name sym) 1))]
      (when raw-name
        (let [forced? (str/starts-with? raw-name "-")]
          {:receiver receiver :long-dot? long-dot? :forced? forced?
           :field-name (if forced? (subs raw-name 1) raw-name)})))))

(defn- field-assignment-target [ctx target]
  (if-let [cname (when (= :sym (:op target)) (java-static? ctx (:val target)))]
    (lookup-java-field ctx target cname (name (:val target)) true)
    (when-let [{:keys [receiver long-dot? forced? field-name]} (dot-field-place target)]
      (let [static-class (when long-dot? (dot-static-class ctx receiver))
            cname (or static-class (instance-field-owner ctx receiver))
            static? (some? static-class)]
        (when static? (infer ctx receiver))
        (if (and cname (not forced?)
                 (some #(empty? (:params %)) (java/methods-named cname field-name static?)))
          (report! ctx target "TSP2345" "Cannot assign to the result of a Java method")
          (lookup-java-field ctx target cname field-name static?))))))

(defn- method-call [ctx node target member-node mname arg-nodes field-fallback]
  (let [tty (java-receiver-type ctx target)
        env (:env ctx)
        inferred (class-of ctx tty)
        hinted (if-let [tag (env/tag-node target)]
                 (env/hint-class (:file ctx) tag)
                 (when (= :sym (:op target))
                   (get-in ctx [:compiler-local-classes (:val target)])))
        cname (if (and hinted (not= hinted inferred) (not (t/primitives hinted))
                       (empty? (java/methods-named inferred mname false))
                       (seq (java/methods-named hinted mname false)))
                hinted inferred)
        unclassified (when-not cname (t/resolve-t env tty))]
    (cond
      (nil? target) t/unknown
      (= :nil (:t unclassified))
      (do (run! #(infer ctx %) arg-nodes)
          (check-nil-receiver! ctx target unclassified)
          t/never)
      (#{:unknown :any} (:t unclassified))
      (do (run! #(infer ctx %) arg-nodes)
          (when-not (compiler-knows? ctx target)
            (report! ctx node "TSP7020" (str "reflective call: the type of " (parse/unparse target) " is unknown")
                     :severity :warning))
          t/unknown)
      (nil? cname) (do (run! #(infer ctx %) arg-nodes) t/unknown)
      :else
      (do
        (check-nil-receiver! ctx target tty)
        (when (and (java/has-class? cname) (not (compiler-knows? ctx target)) (seq (java/methods-named cname mname false)))
          (let [hint (java/short-name cname)]
            (report! ctx node "TSP7023"
                     (str "reflective call, but " (parse/unparse target) " is known to be " hint)
                     :severity :warning
                     :fixes [{:title (str "Add hint ^" hint) :risk "safe"
                              :edits [{:range {:start [(:row (:pos target)) (:col (:pos target))]
                                               :end [(:row (:pos target)) (:col (:pos target))]}
                                       :text (str "^" hint " ")}]}])))
        (java-call (assoc ctx :java-direct? (compiler-knows? ctx target))
                   node member-node cname mname false arg-nodes field-fallback)))))

;; ---- special forms ----

(defmulti special (fn [_ctx _node head] head) :default ::none)
(defmethod special ::none [_ _ _] ::none)

(defmethod special '. [ctx node _]
  (let [[receiver raw-member & trailing :as args] (args-of node)
        grouped? (= :list (:op raw-member))
        [member & arg-nodes] (if grouped? (:children raw-member) (cons raw-member trailing))]
    (cond
      (< (count args) 2)
      (do (run! #(infer ctx %) args)
          (report! ctx node "TSP2554" ". expects a receiver and a member")
          t/unknown)

      (not= :sym (:op member))
      (do (infer ctx receiver)
          (run! #(infer ctx %) arg-nodes)
          (report! ctx (or member raw-member) "TSP2345" "Expected a Java member name")
          t/unknown)

      :else
      (let [member-name (name (:val member))
            static-class (dot-static-class ctx receiver)
            field-fallback (when-not grouped? :no-zero-arg-method)]
        (when static-class (record! ctx receiver (t/class-type "java.lang.Class")))
        (cond
          (and (not grouped?) (empty? arg-nodes) (str/starts-with? member-name "-"))
          (java-field-read ctx member (or static-class (instance-field-owner ctx receiver))
                           (munge (subs member-name 1)) (some? static-class))

          static-class
          (java-call ctx node member static-class (munge member-name) true arg-nodes field-fallback)

          :else
          (method-call ctx node receiver member (munge member-name) arg-nodes field-fallback))))))

(doseq [h '[ns comment declare require import in-ns defmacro defprotocol defrecord deftype defmulti
            definterface clojure.spec.alpha/def clojure.spec.alpha/fdef]]
  (defmethod special h [_ _ _] t/unknown))

(defmethod special 'clojure.spec.alpha/conform [ctx node _]
  (let [[spec value :as args] (args-of node)]
    (if (= 2 (count args))
      (let [form (when (or (and (= :kw (:op spec)) (literal-node? spec))
                           (#{'clojure.spec.alpha/or 'clojure.spec.alpha/spec} (builtin-head ctx spec)))
                   (parse/node->data spec))
            result (when form
                     (conform/tagged-result (:env ctx)
                                            (assoc (env/type-ctx (:file ctx) nil)
                                                   :locals (:locals ctx)
                                                   :defined-symbols (get-in ctx [:env :ns-defs (get-in ctx [:file :ns])]))
                                            form))]
        (when (and *spec-uses* (qualified-keyword? form) (not (:quiet ctx)) (not (:synthetic node)))
          (swap! *spec-uses* conj {:spec form :kind :conform :file (get-in ctx [:file :file]) :pos (:pos spec)}))
        (when-not (and result (= :list (:op spec))) (infer ctx spec))
        (if (= t/never (infer ctx value)) t/never (or result t/unknown)))
      (do (run! #(infer ctx %) args)
          (report! ctx node "TSP2554" "s/conform expects two arguments")
          t/unknown))))

(defmethod special 'clojure.spec.alpha/invalid? [ctx node _]
  (let [[value :as args] (args-of node)]
    (if (= 1 (count args))
      (let [leaves (t/members (:env ctx) (eff ctx (infer ctx value)))
            parts (map #(invalid-parts ctx %) leaves)]
        (apply t/union (concat (when (some first parts) [(t/lit true)])
                               (when (some second parts) [(t/lit false)]))))
      (do (run! #(infer ctx %) args)
          (report! ctx node "TSP2554" "s/invalid? expects one argument")
          t/unknown))))

(defn- var-reference [ctx [target & more]]
  (if (and (= :sym (:op target)) (empty? more))
    (do (record-var-use! ctx (:sym (resolve-var ctx (:val target) target)) target :var)
        (t/class-type "clojure.lang.Var"))
    t/unknown))

(defmethod special 'var [ctx node _]
  (var-reference ctx (args-of node)))

(defn- list-value-type [ctx items]
  (assoc (t/tuple items (literal-element-type ctx items))
         :kind :seq
         :jvm-class (if (seq items) "clojure.lang.PersistentList" "clojure.lang.PersistentList$EmptyList")))

(defn- quoted-data-type [ctx node]
  (let [children (:children node)]
    (case (:op node)
      (:sym :kw :str :num :bool) (t/lit (:val node))
      :nil t/nil-t
      :char (t/class-type "java.lang.Character")
      :list (list-value-type ctx (mapv #(quoted-data-type ctx %) children))
      :vector (let [items (mapv #(quoted-data-type ctx %) children)]
                (t/tuple items (literal-element-type ctx items)))
      :map (let [pairs (partition 2 children)]
             (if (every? #(#{:sym :kw :str} (:op (first %))) pairs)
               (assoc empty-map-shape :req
                      (into (array-map) (map (fn [[k v]] [(:val k) (quoted-data-type ctx v)])) pairs))
               (t/prim :map)))
      :set (t/prim :set)
      :quote (list-value-type ctx [(t/lit 'quote) (quoted-data-type ctx (first children))])
      :var (list-value-type ctx [(t/lit 'var) (quoted-data-type ctx (first children))])
      t/unknown)))

(defn- quoted-type [ctx node]
  (case (:op node)
    :sym (t/prim :symbol)
    :list (quoted-data-type ctx node)
    t/unknown))

(defmethod special 'quote [ctx node _]
  (quoted-type ctx (second (:children node))))

(defmethod special 'do [ctx node _] (infer-body ctx (args-of node)))

(defmethod special 'time [ctx node _]
  (let [[expr & more :as args] (args-of node)]
    (if (and expr (empty? more))
      (infer ctx expr)
      (do (run! #(infer ctx %) args)
          (report! ctx node "TSP2554" "time expects one expression")
          t/unknown))))

(defmethod special 'future [ctx node _]
  (infer-body (dissoc ctx :recur-types) (args-of node))
  (t/class-type "java.util.concurrent.Future"))

(defmethod special 'clojure.test/with-test [ctx node _]
  (let [[definition & body] (args-of node)]
    (if definition
      (let [ty (infer ctx definition)]
        (when-not (= t/never ty)
          (infer-body (dissoc ctx :load-time? :recur-types) body))
        ty)
      (do (report! ctx node "TSP2554" "with-test expects a definition")
          t/unknown))))

(doseq [h '[def defonce]]
  (defmethod special h [ctx node _]
    (some->> (env/definition-initializer node) (infer ctx))
    t/unknown))

(defmethod special 'let [ctx node _]
  (let [[bindings & body] (args-of node)
        pending (volatile! [])
        c (bind-pairs (assoc ctx :pending-callbacks pending) bindings)
        result (infer-body (dissoc c :pending-callbacks) body)]
    ;; Later bindings may provide context for earlier callbacks.
    (doseq [callback (rseq @pending)]
      (finish-callback! ctx callback))
    result))

(defmethod special 'letfn [ctx node _]
  (let [[bindings & body] (args-of node)
        scope (volatile! nil)
        callbacks (into []
                        (keep (fn [definition]
                                (let [[name & declarations] (:children definition)]
                                  (when (and (= :list (:op definition)) (= :sym (:op name)))
                                    (let [head {:op :sym :val 'clojure.core/fn :synthetic true}
                                          init (assoc definition :children (into [head name] declarations))
                                          callback (assoc (local-callback nil init)
                                                          :group-scope scope :active? (volatile! false))]
                                      [name callback])))))
                        (:children bindings))
        analysis (reduce (fn [c [name callback]]
                           (bind c (:val name) (callback-local-type callback)))
                         ctx callbacks)]
    (vreset! scope (dissoc analysis :pending-callbacks))
    (let [result (infer-body analysis body)]
      (doseq [[_ callback] (rseq callbacks)]
        (finish-callback! ctx callback))
      result)))

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
                                           (let [joined (widen (apply t/union ty (map #(nth % i) @calls)))]
                                             ;; Repeated vector appends may grow the shape on every back edge.
                                             (or (when (and (>= n 2) (not= ty joined))
                                                   (widen-vector-growth (:env ctx) joined))
                                                 joined)))
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
(defmethod special 'throw [ctx node _]
  (let [[exception :as args] (args-of node)]
    (if (= 1 (count args))
      ;; The JVM also accepts a null reference here and raises an NPE.
      (assignable! ctx exception (infer ctx exception)
                   (t/union (t/class-type "java.lang.Throwable") t/nil-t))
      (do (run! #(infer ctx %) args)
          (report! ctx node "TSP2554" "throw expects exactly one argument"))))
  t/never)

(defmethod special 'assert [ctx node _]
  (if (= 'assert (t/core-name (:file ctx) (:val (first (:children node)))))
    (let [[test message :as args] (args-of node)]
      (if (<= 1 (count args) 2)
        (let [[_ failure] (condition-contexts ctx test)]
          (when (and message failure)
            (infer failure message)))
        (do (run! #(infer ctx %) args)
            (report! ctx node "TSP2554" "assert expects one or two arguments")))
      ;; Clojure may omit assertions at compile time.
      t/nil-t)
    t/unknown))

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

(defn- binding-test-contexts [ctx init some?]
  (or (emptiness-contexts ctx init some?)
      (if some? (some-contexts ctx init) (truth-contexts ctx init))
      [ctx ctx]))

(defn- bind-conditional-pattern [ctx pattern init ty]
  (let [ty (dissoc ty :fresh)
        captured (when (and (= :sym (:op pattern)) (not (env/tag-node pattern))
                            (= 'find (builtin-head ctx init)))
                   (condition-capture ctx init))
        projection (when-let [{:keys [type path] :as source} (stable-path ctx init)]
                     ;; A binding can be narrowed even when its source path could not be split.
                     (when (= ty (dissoc (projected-type ctx type path) :fresh))
                       source))]
    (cond-> (bind-pattern ctx pattern ty nil projection)
      captured (assoc-in [:conditions (:val pattern)] captured))))

(defn- if-let-form [ctx node some?]
  (let [[bindings then else] (args-of node)
        [p init] (:children bindings)
        ty (eff ctx (infer ctx init))
        positive (if some? (t/remove-nil (:env ctx) ty) (truth-part (:env ctx) ty true))
        negative (if some? (nil-part (:env ctx) ty) (truth-part (:env ctx) ty false))
        [tc fc] (binding-test-contexts ctx init some?)]
    [(if (or (nil? tc) (= :never (:t positive))) t/never
         (infer (bind-conditional-pattern tc p init positive) then))
     (if (or (nil? fc) (= :never (:t negative))) t/never (if else (infer fc else) t/nil-t))]))

(defmethod special 'if-let [ctx node _] (apply t/union (if-let-form ctx node false)))
(defmethod special 'if-some [ctx node _] (apply t/union (if-let-form ctx node true)))

(defn- when-let-form [ctx node some?]
  (let [[bindings & body] (args-of node)
        [p init] (:children bindings)
        ty (eff ctx (infer ctx init))
        positive (if some? (t/remove-nil (:env ctx) ty) (truth-part (:env ctx) ty true))
        negative (if some? (nil-part (:env ctx) ty) (truth-part (:env ctx) ty false))
        [tc fc] (binding-test-contexts ctx init some?)]
    (t/union (if (or (nil? tc) (= :never (:t positive))) t/never
                 (infer-body (bind-conditional-pattern tc p init positive) body))
             (if (or (nil? fc) (= :never (:t negative))) t/never t/nil-t))))

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
        body (remove #(or (catch? %) (finally? %)) forms)
        normal (apply t/union
                      (infer-body ctx body)
                      (for [c (filter catch? forms)
                            :let [[cls sym & cbody] (args-of c)
                                  cn (t/resolve-class (:imports (:file ctx)) (:val cls))]]
                        (infer-body (bind ctx (:val sym) (if cn (t/class-type cn) t/unknown) :hinted true) cbody)))
        cleanup (mapv #(infer-body ctx (args-of %)) (filter finally? forms))]
    (if (some #(= :never (:t %)) cleanup) t/never normal)))

(defn- mentions-symbol? [node sym]
  (or (and (= :sym (:op node)) (= sym (:val node)))
      (some #(mentions-symbol? % sym) (:children node))))

(defn- threaded-step [ctx step first? previous display input hinted?]
  (let [locals (:locals ctx)
        sym (if (or (contains? locals '__threaded) (mentions-symbol? step '__threaded))
              (let [step-names (into #{} (keep #(when (= :sym (:op %)) (:val %)))
                                     (tree-seq (comp seq :children) :children step))]
                (first (remove #(or (contains? locals %) (contains? step-names %))
                               (map #(symbol (str "__threaded_" %)) (range)))))
              '__threaded)
        acc {:op :sym :val sym :pos (:pos previous) :synthetic true :display display}
        call (if (= :list (:op step))
               (let [[h & args] (:children step)]
                 (assoc step :children (vec (if first? (concat [h acc] args) (concat [h] args [acc])))))
               {:op :list :pos (:pos step) :children [step acc]})]
    (infer (bind ctx sym input :why (why-of ctx previous input) :hinted hinted?) call)))

(defn- thread [ctx node first? some?]
  (let [[x & steps] (args-of node) env (:env ctx)]
    (loop [ty (infer ctx x) prev x steps steps maybe-nil? false shown (parse/unparse x) n 0]
      (if (empty? steps)
        (if (and some? maybe-nil?) (t/union ty t/nil-t) ty)
        (let [step (first steps)
              arrow (parse/unparse (first (:children node)))
              display (if (zero? n) shown (str "(" arrow " " shown ")"))
              in (if some? (t/remove-nil env ty) ty)]
          (recur (threaded-step ctx step first? prev display in (compiler-knows? ctx prev))
                 step (rest steps) (or maybe-nil? (t/nilable? env ty))
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

(defn- close-receiver-class [ctx member init]
  (or (class-of ctx member)
      (:jvm-class member)
      (case (:t member)
        :atom "clojure.lang.Atom"
        :map (when (= :map (:op init)) "clojure.lang.APersistentMap")
        :coll (case (:op init)
                :vector "clojure.lang.PersistentVector"
                :set "clojure.lang.PersistentHashSet"
                nil)
        nil)))

(defn- check-close! [ctx binding init actual]
  (let [env (:env ctx)
        hint (when (#{:unknown :any} (:t (t/resolve-t env actual)))
               (env/hint-type (:file ctx) binding))
        members (t/members env (eff ctx (or hint actual)))
        classes (distinct (map #(close-receiver-class ctx % init)
                               (remove #(#{:any :nil :never} (:t %)) members)))]
    (when (some #(= :nil (:t %)) members)
      (report! ctx init "TSP2531" "with-open resource may be nil when close is called"))
    (doseq [cname classes
            :when (and cname (java/has-class? cname))
            :let [methods (java/methods-named cname "close" false)
                  shown (java/short-name cname)]]
      (cond
        (empty? methods)
        (report! ctx init "TSP2339" (str "with-open resource " shown " has no instance method close"))

        (not-any? #(empty? (:params %)) methods)
        (report! ctx init "TSP2554" (str "with-open resource " shown " has no zero-argument close method"))))
    (when (and (:strict (:opts ctx))
               (some #(not (some-> % java/has-class?)) classes))
      (report! ctx init "TSP210-01" "with-open resource close method is not checked"
               :reasons #{:close-method}))))

(defmethod special 'with-open [ctx node _]
  (let [[bindings & body] (args-of node)
        forms (:children bindings)]
    (if (and (= :vector (:op bindings)) (even? (count forms))
             (every? #(= :sym (:op %)) (take-nth 2 forms)))
      (let [reachable? (volatile! true)
            c (bind-pairs ctx bindings
                          (fn [c binding init actual]
                            (when @reachable?
                              (if (= t/never actual)
                                (vreset! reachable? false)
                                (check-close! c binding init actual)))))
            result (infer-body (cond-> c (not @reachable?) (dissoc :recur-types)) body)]
        (if @reachable? result t/never))
      (do (report! ctx (or bindings node) "TSP2345" "with-open expects a binding vector of symbol and initializer pairs")
          (when (= :vector (:op bindings))
            (run! #(infer ctx %) (take-nth 2 (rest forms))))
          (infer-body ctx body)))))

(doseq [h '[binding with-redefs with-local-vars]]
  (defmethod special h [ctx node _]
    (let [[bindings & body] (args-of node)]
      (run! #(infer ctx (second %)) (partition 2 (:children bindings)))
      (infer-body (cond-> ctx (= 'with-redefs h) (assoc :guards-disabled? true)) body))))

(defmethod special 'doto [ctx node _]
  (let [[x & forms] (args-of node)
        ty (infer ctx x)]
    (run! (fn [f] (run! #(infer ctx %) (rest (:children f)))) (filter #(= :list (:op %)) forms))
    ty))

(defmethod special 'as-> [ctx node _]
  (let [[x nm & forms] (args-of node)]
    (reduce (fn [ty f] (infer (bind ctx (:val nm) (dissoc ty :fresh)) f)) (infer ctx x) forms)))

(defn- conditional-thread [ctx node first?]
  (let [[x & clauses] (args-of node)
        source-local (plain-local ctx x)
        initial (infer ctx x)]
    (if (or (nil? x) (odd? (count clauses)))
      (do (report! ctx node "TSP2554" "conditional threading requires a value followed by test/step pairs")
          t/unknown)
      (loop [ty initial, previous x, hinted? (compiler-knows? ctx x), pairs (seq (partition 2 clauses))]
        (if (and pairs (not= t/never ty))
          (let [[[test step] & more] pairs
                [on off] (condition-contexts ctx test)
                ;; Before any step can run, the accumulator still aliases the original local.
                input (fn [branch] (or (when (identical? previous x) (get-in branch [:locals source-local])) ty))
                executed (if on (threaded-step on step first? previous (parse/unparse previous) (input on) hinted?) t/never)
                joined (t/union executed (if off (input off) t/never))]
            ;; Bound the number of shapes produced by independent optional steps.
            (recur (if (> (count (:types joined)) 32) t/unknown joined)
                   (if on step previous)
                   (if on (and (not off) (compiler-knows? on step)) hinted?)
                   (seq more)))
          ty)))))

(defmethod special 'cond-> [ctx node _] (conditional-thread ctx node true))
(defmethod special 'cond->> [ctx node _] (conditional-thread ctx node false))

;; maps

(defn- read-map-of [ctx source key-type not-found]
  (let [env (:env ctx)
        members (t/members env source)
        key-members (t/members env key-type)
        overlaps? (fn [keys domain] (not-every? #(disjoint-scalars? env % domain) keys))
        plain-member? #(#{:map-of :nil :never} (:t %))
        values (cond
                 (every? plain-member? members)
                 (for [{:keys [k v] :as member} members
                       :when (and (= :map-of (:t member)) (overlaps? key-members k))]
                   v)

                 (and (some :map-index members)
                      (every? #(or (plain-member? %) (:map-index %) (:complete-keys? %)) members))
                 (mapcat (fn [member]
                           (let [fields (merge (:opt member) (:req member))
                                 index (if (= :map-of (:t member)) member (:map-index member))
                                 unlisted (remove #(and (= :lit (:t %)) (contains? fields (:val %))) key-members)]
                             (cond-> (for [[k value] fields :when (overlaps? key-members (t/lit k))] value)
                               (and index (overlaps? unlisted (:k index))) (conj (:v index)))))
                         members))]
    (if values
      (t/with-assumption
       (apply t/union not-found values)
       (or (t/assumption source) (t/assumption key-type)))
      t/unknown)))

(defn- read-literal-map-key [ctx source k not-found]
  (let [{:keys [status type]} (lookup-key ctx source k)]
    (case status
      :req (if (t/nilable? (:env ctx) source) (t/union type not-found) type)
      :opt (t/union type not-found)
      (:nil :absent) not-found
      t/unknown)))

(defn- found-map-entry [key-type value-type optional?]
  (if (= t/never value-type)
    t/nil-t
    (let [entry (assoc (t/tuple [key-type value-type])
                       :map-entry? true :jvm-class "clojure.lang.IMapEntry")]
      (if optional? (t/union entry t/nil-t) entry))))

(defn- find-map-entry [ctx source key-type]
  (case (:t source)
    :nil t/nil-t
    :map (if (= :lit (:t key-type))
           (let [{:keys [status type]} (lookup-key ctx source (:val key-type))]
             (case status
               (:nil :absent) t/nil-t
               (:req :opt :unknown) (found-map-entry key-type type (not= :req status))
               t/unknown))
           (if-let [domain (map-index-domain source)]
             (recur ctx domain key-type)
             t/unknown))
    :map-of (found-map-entry key-type (read-map-of ctx source key-type t/never) true)
    t/unknown))

(defmethod special 'find [ctx node _]
  (let [args (args-of node)
        [source key-type :as types] (mapv #(eff ctx (infer ctx %)) args)
        key-type (quoted-map-key-type (second args) key-type)]
    (cond
      (not= 2 (count args))
      (do (report! ctx node "TSP2554" "find expects two arguments") t/unknown)

      (some #{t/never} types) t/never

      :else
      (let [env (:env ctx)]
        (t/with-assumption
         (apply t/union
                (for [member (t/members env source)
                      key (t/members env key-type)]
                  (find-map-entry ctx member key)))
         (or (t/assumption source) (t/assumption key-type)))))))

(defn- vector-element-at [member index]
  (cond
    (= :absent (length/position member index)) t/never
    (:items member) (nth (:items member) (int index))
    :else (or (:of member) t/unknown)))

(defn- read-vector-index [ctx vectors key-type not-found]
  (let [env (:env ctx)
        keys (t/members env key-type)]
    (apply t/union
           (for [member vectors
                 key keys]
             (cond
               (= :nil (:t member)) not-found
               (or (= :never (:t member)) (= :never (:t key))) t/never

               (= :lit (:t key))
               (if (integer? (:val key))
                 (let [index (lookup-index (:val key))
                       element (vector-element-at member index)]
                   (if (= :present (length/position member index)) element (t/union element not-found)))
                 not-found)

               (or (= :double (:name key))
                   (disjoint-scalars? env key (t/prim :int))
                   (= 0 (second (length/bounds member)))) not-found

               :else (t/union (or (:of member) t/unknown) not-found))))))

(defmethod special 'get [ctx node _]
  (let [[m k default] (args-of node)
        mty (infer ctx m)]
    (if (= :kw (:op k))
      (if default
        (read-key ctx mty (:val k) k (infer ctx default) t/unknown)
        (read-key ctx mty (:val k) k))
      (let [source (eff ctx mty)
            key-type (quoted-map-key-type k (eff ctx (infer ctx k)))
            not-found (if default (infer ctx default) t/nil-t)
            env (:env ctx)
            members (t/members env source)
            keys (when (some #(= :map (:t %)) members)
                   (finite-map-keys env key-type))
            assumption (or (t/assumption source) (t/assumption key-type))]
        (cond
          (some #{t/never} [source key-type not-found])
          t/never

          keys
          (t/with-assumption
           (apply t/union (map #(read-literal-map-key ctx source % not-found) keys))
           assumption)

          (and (some vector-member? members) (every? #(or (vector-member? %) (#{:nil :never} (:t %))) members))
          (t/with-assumption (read-vector-index ctx members key-type not-found) assumption)

          :else
          (read-map-of ctx source key-type not-found))))))

(defn- string-index-present? [value index]
  (and (string? value) (<= 0 index (dec (count value)))))

(defn- numeric-path-value [ctx ty key index]
  (let [env (:env ctx)
        vector-type (t/prim :vector)]
    (apply t/union
           (for [member (t/members env ty)]
             (case (:t member)
               :coll (case (:kind member)
                       :vector (vector-element-at member index)
                       :set (:of member)
                       ;; A kindless coll-of can be a map whose entries fit the
                       ;; element spec. get returns the value of an entry there.
                       nil (if (some #(or (t/sub? env % vector-type)
                                         (not= :mismatch (:status (t/sub-result env vector-type %))))
                                     (t/members env (:of member)))
                             t/unknown (:of member))
                       t/never)
               :map (if-let [index-type (:map-index member)]
                      (or (get (:req member) key) (get (:opt member) key)
                          (t/map-of-entry env index-type key) t/never)
                      (if (:complete-keys? member) t/never t/unknown))
               :map-of (read-map-of ctx member (t/lit key) t/never)
               :lit (if (string-index-present? (:val member) index)
                      (t/class-type "java.lang.Character") t/never)
               :prim (case (:name member)
                       :string (if (neg? index) t/never (t/class-type "java.lang.Character"))
                       (:map :vector :set :coll) t/unknown
                       t/never)
               :any t/any
               (:class :unknown :var :rigid :ivar) t/unknown
               t/never)))))

(defn- literal-path-key? [value]
  (or (keyword? value) (string? value) (symbol? value) (integer? value)))

(defn- literal-path-step [ctx source node value]
  (if (integer? value)
    (let [index (lookup-index value)]
      {:type (numeric-path-value ctx source value index)
       :optional? (not (every? (fn [member]
                                 (case (:t member)
                                   :coll (and (= :vector (:kind member))
                                              (= :present (length/position member index)))
                                   :lit (string-index-present? (:val member) index)
                                   false))
                               (t/members (:env ctx) source)))})
    (key-info ctx source value node true)))

(defn- computed-path-step [ctx source node key-type]
  (let [env (:env ctx)
        keys (t/members env key-type)]
    (cond
      (every? #(#{:map-of :nil :never} (:t %)) (t/members env source))
      {:type (read-map-of ctx source key-type t/never) :optional? true}

      (every? #(and (= :lit (:t %)) (literal-path-key? (:val %))) keys)
      (let [steps (mapv #(literal-path-step ctx source node (:val %)) keys)]
        {:type (apply t/union (map :type steps))
         :optional? (boolean (some :optional? steps))})

      (some :map-index (t/members env source))
      {:type (read-map-of ctx source key-type t/never) :optional? true})))

(defn- path-value [ctx ty path steps not-found]
  (loop [ty ty, steps (seq steps), missing? false]
    (if (and steps (not= :never (:t ty)))
      (let [[[key key-type] & more] steps
            current (eff ctx ty)
            key-type (quoted-map-key-type key (eff ctx key-type))]
        (if-let [{:keys [type optional?]} (if (and (literal-node? key) (literal-path-key? (:val key)))
                                         (literal-path-step ctx current key (:val key))
                                         (computed-path-step ctx current (or key path) key-type))]
          (recur (t/with-assumption type (or (t/assumption current) (t/assumption key-type)))
                 more (or missing? optional?))
          t/unknown))
      (if missing? (t/union ty not-found) ty))))

(defn- path-key-nodes [path]
  (when (and (= :vector (:op path)) (empty? (:directives path)) (not (env/tag-node path)))
    (:children path)))

(defmethod special 'get-in [ctx node _]
  (let [[m path default & extra :as arguments] (args-of node)
        mty (infer ctx m)]
    (if (or (nil? path) (seq extra))
      (do (run! #(infer ctx %) (rest arguments))
          (report! ctx node "TSP2554" (str "get-in is called with " (count arguments) " argument(s)"))
          t/unknown)
      (let [path-type (eff ctx (infer ctx path))
            not-found (if default (infer ctx default) t/nil-t)
            key-nodes (path-key-nodes path)]
        (if (some #{t/never} [mty path-type not-found])
          t/never
          (t/with-assumption
           (apply t/union
                  (for [{:keys [items] :as member} (t/members (:env ctx) path-type)]
                    (cond
                      (= :nil (:t member)) mty
                      (= :never (:t member)) t/never
                      (some #{t/never} items) t/never
                      items (path-value ctx mty path (map vector (concat key-nodes (repeat nil)) items) not-found)
                      :else t/unknown)))
           (t/assumption path-type)))))))

(defn- assoc-map-key [m k ty]
  (-> m (dissoc :fresh) (assoc-in [:req k] ty) (update :opt dissoc k)))

(defn- dissoc-map-key [shape key track-absence?]
  (cond-> (-> shape (update :req dissoc key) (update :opt dissoc key))
    (and track-absence? (not (:complete-keys? shape))) (assoc-in [:opt key] t/never)))

(defn- assoc-key-type [ctx ty k value]
  (let [members (t/members (:env ctx) ty)]
    (if (every? #(#{:map :nil} (:t %)) members)
      (apply t/union (map #(assoc-map-key (if (= :nil (:t %)) empty-map-shape %) k value) members))
      ty)))

(defn- forget-map-keys [ctx ty]
  (apply t/union (for [member (t/members (:env ctx) ty)]
                   (cond (= :map (:t member)) (t/prim :map)
                         (or (:items member) (length/constrained? member))
                         (assoc (dissoc member :items :min-count :max-count :entry-map) :of t/unknown)
                         :else member))))

(defn- literal-map-key-type? [ty]
  (some? (selector-literal-key ty)))

(defn- map-path-key-type? [ty]
  (or (literal-map-key-type? ty)
      (and (= :prim (:t ty)) (#{:keyword :string :symbol} (:name ty)))))

(defn- fixed-map-key-sequences [ctx path path-type supported-key?]
  (let [env (:env ctx)
        key-nodes (concat (path-key-nodes path) (repeat path))
        choices (fn [node ty]
                  (let [keys (t/members env (quoted-map-key-type node (eff ctx ty)))]
                    (when (every? supported-key? keys)
                      (mapv (fn [key-type] [node key-type]) keys))))
        paths (mapv (fn [member]
                      (if (= :nil (:t member))
                        []
                        (when-let [items (:items member)] (mapv choices key-nodes items))))
                    (t/members env path-type))]
    (when (every? #(and (some? %) (every? seq %)) paths)
      (bounded-combinations paths))))

(defn- map-path-entry [ctx member key-type]
  (let [env (:env ctx)
        k (:val key-type)
        m (if (= :nil (:t member)) empty-map-shape member)]
    (case (:t m)
      :map-of
      [(if (some? k)
         (t/union (or (t/map-of-entry env m k) t/never) t/nil-t)
         (read-map-of ctx m key-type t/nil-t))
       (fn [value]
         (cond-> (update m :v t/union value)
           (not (t/sub? env key-type (:k m))) (update :k t/union key-type)))]

      :map
      (if (some? k)
        [(cond
           (contains? (:req m) k) (get (:req m) k)
           (contains? (:opt m) k) (t/union (get (:opt m) k) t/nil-t)
           (:map-index m) (t/union (or (t/map-of-entry env (:map-index m) k) t/never) t/nil-t)
           (:complete-keys? m) t/nil-t
           (get-in env [:specs k]) (t/union (t/ref-t k) t/nil-t)
           :else t/unknown)
         #(assoc-map-key m k %)]
        (if-let [domain (map-index-domain m)]
          (recur ctx domain key-type)
          (when (and (:complete-keys? m) (empty? (:req m)) (empty? (:opt m)))
            [t/nil-t (fn [value] {:t :map-of :k key-type :v value})]))))))

(defn- path-writes
  "Find the previous leaf and rebuild each possible root after a map-path write."
  [ctx root keys budget]
  (let [env (:env ctx)
        unchecked [t/unknown (constantly t/unknown)]]
    (letfn [(walk [ty [[key key-type] & more]]
              (let [k (:val key-type)
                    members (vec (take (inc @budget) (t/members env (eff ctx ty))))]
                (if (neg? (vswap! budget - (count members)))
                  [unchecked]
                  (into []
                        (mapcat
                         (fn [member]
                           (case (:t member)
                             (:map :nil :map-of)
                             (if-let [[previous store] (map-path-entry ctx member key-type)]
                               (if (seq more)
                                 (for [[leaf rebuild] (walk previous more)]
                                   [leaf (fn [value]
                                           (let [child (rebuild value)]
                                             (when-not (= :unknown (:t child)) (check-key-value! ctx k key child))
                                             (store child)))])
                                 [[previous store]])
                               [unchecked])
                             :never []
                             (:any :unknown :class :var :rigid :ivar) [unchecked]
                             (if (and (= :prim (:t member)) (= :map (:name member)))
                               [unchecked]
                               (do (report! ctx key "TSP2339" (str "cannot write " (or k (t/show key-type)) " into " (t/show member)))
                                   []))))
                        members)))))]
      (walk root keys))))

(defn- write-map-paths [ctx mty selector-type paths value arg-types update?]
  (let [budget (volatile! map-path-budget)
        writes (vec (for [keys paths
                          [previous rebuild] (path-writes ctx mty keys budget)]
                      [(:val (second (peek keys))) previous rebuild]))
        vty (if (and update? (seq writes))
              (apply-fn-node ctx value (into [(apply t/union (map second writes))] arg-types))
              (infer ctx value))]
    (cond
      (some #{t/never} (into [mty selector-type vty] arg-types)) t/never
      (nil? paths) t/unknown
      :else
      (let [env (:env ctx)
            stored (into {} (for [k (distinct (map first writes))]
                              (let [spec (when (get-in env [:specs k]) (t/ref-t k))]
                                (check-key-value! ctx k value vty)
                                [k (if (and spec (not (t/sub? env (eff ctx vty) spec)))
                                     spec
                                     (dissoc vty :fresh))])))]
        (t/with-assumption
         (apply t/union (map (fn [[k _ rebuild]] (rebuild (get stored k))) writes))
         (or (t/assumption mty) (t/assumption selector-type)))))))

(defn- write-path [ctx node update?]
  (let [[m path value & args :as arguments] (args-of node)
        mty (infer ctx m)]
    (if (or (nil? value) (and (not update?) (seq args)))
      (do (run! #(infer ctx %) (rest arguments))
          (report! ctx node "TSP2554" (str (if update? "update-in" "assoc-in") " is called with " (count arguments) " argument(s)"))
          t/unknown)
      (let [path-type (eff ctx (infer ctx path))
            paths (fixed-map-key-sequences ctx path path-type map-path-key-type?)
            paths (when (every? seq paths) paths)
            arg-types (mapv #(infer ctx %) args)]
        (write-map-paths ctx mty path-type paths value arg-types update?)))))

(defmethod special 'assoc-in [ctx node _] (write-path ctx node false))
(defmethod special 'update-in [ctx node _] (write-path ctx node true))

(defn- assoc-entries [ctx base entries]
  (reduce (fn [ty [[k v] [kt vt]]]
            (when (= :kw (:op k)) (check-key-value! ctx (:val k) v vt))
            (let [literal-key (if (= :kw (:op k)) (:val k) (stable-map-key k))
                  env (:env ctx)
                  members (t/members env ty)
                  keys (when (and (nil? literal-key) (every? #(#{:map :nil} (:t %)) members))
                         (finite-map-keys env (eff ctx kt)))
                  add (fn [base shape]
                        (cond
                          (and (nil? literal-key) (:map-index shape))
                          (let [domain (map-index-domain shape)] (recur domain domain))

                          (= :map-of (:t shape))
                          (if (and (t/sub? env kt (:k shape)) (t/sub? env vt (:v shape)))
                            base
                            (-> shape (update :k t/union (widen kt)) (update :v t/union (widen vt))))
                          (= :vector (:kind shape))
                          (let [index (:val kt)
                                present? (and (= :lit (:t kt)) (integer? index)
                                              (= :present (length/position shape index)))]
                            (assignable! ctx k kt (t/prim :int))
                            (if (and (:items shape) present?)
                              (t/tuple (assoc (:items shape) (int index) vt))
                              (-> (cond-> (dissoc shape :items) (not present?) (dissoc :max-count))
                                  (update :of t/union (widen vt)))))
                          (= (t/prim :vector) shape)
                          (do (assignable! ctx k kt (t/prim :int)) base)
                          (some? literal-key)
                          (assoc-key-type ctx base literal-key (key-type ctx literal-key vt))
                          (= :nil (:t shape)) (t/prim :map)
                          :else (forget-map-keys ctx base)))]
              (cond
                keys
                (do (doseq [key keys] (check-key-value! ctx key v vt))
                    (if (<= (* (count keys) (count members)) map-path-budget)
                      (t/with-assumption
                       (apply t/union (map #(assoc-key-type ctx ty % (key-type ctx % vt)) keys))
                       (or (t/assumption ty) (t/assumption kt)))
                      (forget-map-keys ctx ty)))

                (and (next members)
                     (some #(or (= :map-of (:t %)) (= :vector (:kind %)) (= (t/prim :vector) %)
                                (and (nil? literal-key) (:map-index %))) members))
                (apply t/union (map #(add % %) members))

                :else (add ty (first members)))))
          base entries))

(defmethod special 'assoc [ctx node _]
  (let [[m & kvs] (args-of node)
        base (infer ctx m)
        pairs (partition 2 kvs)
        types (mapv (fn [[k v]]
                      (let [vt (infer ctx v)
                            kt (if (= :kw (:op k)) (t/lit (:val k)) (quoted-map-key-type k (infer ctx k)))]
                        [kt vt]))
                    pairs)]
    (assoc-entries ctx base (map vector pairs types))))

(defn- dissoc-map-keys [member keys track-absence?]
  (if (= :map (:t member))
    (reduce (fn [shape k]
              (dissoc-map-key shape k (or track-absence? (:map-index shape) (string? k) (symbol? k))))
            (dissoc member :fresh) keys)
    member))

(defmethod special 'dissoc [ctx node _]
  (let [[m & ks] (args-of node)
        mty (infer ctx m)
        env (:env ctx)
        members (t/members env mty)
        map-shapes? (every? #(#{:map :nil} (:t %)) members)
        keys (mapv #(if (= :kw (:op %)) (:val %) (stable-map-key %)) ks)]
    (if (and map-shapes? (every? some? keys))
      (apply t/union (map #(dissoc-map-keys % keys false) members))
      (let [key-types (mapv #(eff ctx (infer ctx %)) ks)
            choices (when map-shapes?
                      (mapv (fn [key ty] (if key [key] (finite-map-keys env ty))) keys key-types))
            variants (when (and choices (every? seq choices))
                       (bounded-combinations [(into [members] choices)]))]
        (cond
          (some #{t/never} key-types) t/never
          variants (t/with-assumption
                    (apply t/union (map (fn [[member & keys]] (dissoc-map-keys member keys true)) variants))
                    (or (t/assumption mty) (some t/assumption key-types)))
          :else (apply t/union (map #(or (map-index-domain %) (forget-map-keys ctx %)) members)))))))

(defn- map-update-paths [ctx source node key-type]
  (if-let [key (stable-map-key node)]
    [[[node (t/with-assumption (t/lit key) (t/assumption key-type))]]]
    (let [env (:env ctx)
          members (t/members env (eff ctx source))]
      (when (every? #(#{:map :map-of :nil :never} (:t %)) members)
        (let [keys (vec (take (inc map-path-budget) (t/members env (eff ctx key-type))))
              supported-key? (if (some #(and (= :map (:t %)) (not (:map-index %))) members)
                               literal-map-key-type? map-path-key-type?)]
          (when (and (<= 1 (count keys) map-path-budget) (every? supported-key? keys))
            (mapv (fn [key] [[node key]]) keys)))))))

(defmethod special 'update [ctx node _]
  (let [[m k f & args] (args-of node)
        mty (infer ctx m)
        arg-types (mapv #(infer ctx %) args)]
    (if (= :kw (:op k))
      (let [cur (read-key ctx mty (:val k) k)
            ret (apply-fn-node ctx f (into [cur] arg-types))]
        (check-key-value! ctx (:val k) f ret)
        (assoc-key-type ctx mty (:val k) (key-type ctx (:val k) ret)))
      (let [key-type (infer ctx k)]
        (if-let [paths (when f (map-update-paths ctx mty k key-type))]
          (write-map-paths ctx mty key-type paths f arg-types true)
          (do (infer ctx f) (forget-map-keys ctx mty)))))))

(defmethod special 'merge [ctx node _]
  (let [tys (map #(infer ctx %) (args-of node))
        ms (map #(single-map ctx %) tys)]
    (if (and (seq ms) (every? some? ms))
      (if (and (next ms) (some :map-index ms))
        (t/prim :map)
        (reduce (fn [a b]
                  (let [req (merge (reduce-kv (fn [r k v]
                                               (if (contains? r k)
                                                 (update r k t/union v)
                                                 r))
                                             (:req a) (:opt b))
                                   (:req b))
                        opt (merge-with t/union (:opt a) (:opt b))]
                    {:t :map :req req :opt (apply dissoc opt (keys req))}))
                ms))
      t/unknown)))

(defn- project-map-keys [ctx source keys]
  (reduce (fn [shape [node key-type]]
            (let [k (:val key-type)
                  {:keys [type optional? declared?]} (key-info ctx source k node)]
              (if (= t/never type)
                shape
                (assoc-in shape [(if (or optional? (not declared?)) :opt :req) k] type))))
          empty-map-shape keys))

(defmethod special 'select-keys [ctx node _]
  (let [args (args-of node)
        [mty keys-type :as types] (mapv #(eff ctx (infer ctx %)) args)]
    (cond
      (not= 2 (count args))
      (do (report! ctx node "TSP2554" "select-keys expects two arguments") t/unknown)

      (some #{t/never} types) t/never

      :else
      (if-let [paths (fixed-map-key-sequences ctx (second args) keys-type literal-map-key-type?)]
        (t/with-assumption
         (apply t/union (map #(project-map-keys ctx mty %) paths))
         (or (t/assumption mty) (t/assumption keys-type)))
        t/unknown))))

;; collections

(defn- coll-of [kind ty] {:t :coll :kind kind :of (widen (dissoc ty :fresh))})

(defn- unsupported-sequence-member [ctx ty]
  (some (fn [member]
          (when (#{:int :double :number :boolean :keyword :symbol :uuid :inst}
                 (:name (if (= :lit (:t member)) (t/lit-prim (:val member)) member)))
            member))
        (t/members (:env ctx) ty)))

(defn- report-invalid-sequence-args! [ctx head args sources]
  (let [invalid (keep (fn [[arg source]]
                        (when-let [member (unsupported-sequence-member ctx source)]
                          [arg member]))
                      (map vector args sources))]
    (doseq [[arg member] invalid]
      (report! ctx arg "TSP2345" (str head " cannot sequence " (t/show member))))
    (boolean (seq invalid))))

(defn- unary-sequence-source [ctx node head]
  (let [args (args-of node)]
    (if (= 1 (count args))
      (let [arg (first args)
            source (infer ctx arg)]
        (if-let [bad (unsupported-sequence-member ctx (eff ctx source))]
          (report! ctx arg "TSP2345" (str head " cannot sequence " (t/show bad)))
          source))
      (do (run! #(infer ctx %) args)
          (report! ctx node "TSP2554" (str head " expects one argument"))))))

(defn- unordered-collection? [member]
  (or (#{:map :map-of} (:t member))
      (#{:map :set} (or (:kind member) (:name member)))))

(defn- spread-element-type [ctx member]
  (if (array-component member)
    (array-read-type member)
    (elem-type ctx member)))

(defn- spread-tail-types [ctx tail members]
  (let [form (if (= :quote (:op tail)) (first (:children tail)) tail)]
    (if (and (= :list (:op form)) (empty? (:children form)) (empty? (:directives tail)))
      [[]]
      (mapv (fn [member]
              (cond
                (= :nil (:t member)) []
                (:items member) (:items member)
                :else (let [[lo hi] (sequence-member-bounds member)]
                        (when (and (= lo hi) (<= lo 64))
                          (vec (repeat lo (spread-element-type ctx member)))))))
            members))))

(defn- finite-spreads [spreads members]
  (if (every? some? spreads)
    spreads
    (let [choices (map (fn [items member]
                         (if items [items]
                             (when (= :coll (:t member))
                               (let [[lo hi] (length/bounds member)]
                                 (when (and hi (<= hi 64))
                                   (map #(vec (repeat % (:of member))) (range lo (inc hi))))))))
                       spreads members)]
      (when (every? some? choices)
        (let [spreads (vec (take 65 (mapcat identity choices)))]
          (when (<= (count spreads) 64) spreads))))))

(defn- spread-tail-nodes [tail items]
  (if (and (= :vector (:op tail)) (empty? (:directives tail)) (not (env/tag-node tail))
           (= (count items) (count (:children tail))))
    (:children tail)
    (mapv (fn [i ty]
            (cond-> {:op :spread :pos (:pos tail) :synthetic true
                     :display (str (parse/unparse tail) " at index " i)}
              (and (= :lit (:t ty)) (keyword? (:val ty))) (assoc :op :kw :val (:val ty))))
          (range) items)))

(defn- checkable-spread-contract? [ty]
  (or (= :all (:t ty))
      (some (fn [{:keys [params rest kwargs]}]
              (or (not rest) kwargs
                  (some #(not (#{:unknown :any} (:t %))) (conj params rest))))
            (:arities ty))))

(defn- spread-seqable? [ty]
  (case (:t ty)
    (:nil :never :coll :map :map-of) true
    :lit (string? (:val ty))
    :prim (#{:coll :seq :vector :set :map :string} (:name ty))
    :class (when-let [^Class cls (t/load-class (:name ty))]
             (or (.isArray cls)
                 (some #(.isAssignableFrom ^Class % cls)
                       [clojure.lang.Seqable java.lang.Iterable java.lang.CharSequence java.util.Map])))
    false))

(defn- spread-call-candidates [ctx shape prefix-size members spreads]
  (let [arities (:arities shape)]
    (when (and (= :fn (:t shape)) (seq arities) (not-any? :kwargs arities)
               (every? spread-seqable? members))
      ;; Above the last fixed arity and every variadic minimum, only
      ;; homogeneous rest parameters are added. One call covers that region.
      (let [limit (max 1 (- (inc (apply max (map t/arity-minimum arities))) prefix-size))]
        (when (<= limit 64)
          (let [choices (map (fn [member items]
                               (if items
                                 [{:items items :exact? true}]
                                 (let [element (spread-element-type ctx member)
                                       [lo hi] (if (= t/never element) [0 0] (sequence-member-bounds member))
                                       low (min lo limit)
                                       high (min (or hi limit) limit)]
                                   (map (fn [n] {:items (vec (repeat n element))})
                                        (range low (inc high))))))
                             members spreads)
                candidates (vec (take 65 (mapcat identity choices)))]
            (when (and (<= (count candidates) 64)
                       (every? #(seq (matching-arities shape (+ prefix-size (count (:items %)))))
                               candidates))
              candidates)))))))

(defmethod special 'apply [ctx node _]
  (let [[f & args] (args-of node)]
    (if (empty? args)
      (do (when f (infer ctx f))
          (report! ctx node "TSP2554" "apply expects a function and a final sequence argument")
          t/unknown)
      (let [prefix (vec (butlast args))
            tail (last args)
            prefix-types (mapv #(infer ctx %) prefix)
            tail-type (infer ctx tail)
            tail-members (t/members (:env ctx) (eff ctx tail-type))
            spreads (spread-tail-types ctx tail tail-members)
            known-spreads (finite-spreads spreads tail-members)
            exact? (some? known-spreads)
            callback? (and exact? (callback-node? ctx f))
            spread-context (fn [items] (direct-callback-context (into prefix-types items) false))
            ftype (when-not callback? (t/resolve-t (:env ctx) (infer ctx f)))
            shape (call-shape ftype)
            candidates (when-not exact? (spread-call-candidates ctx shape (count prefix) tail-members spreads))
            invalid-tail (unsupported-sequence-member ctx tail-type)]
        (cond
          (some #(= :never (:t %)) (into [ftype tail-type] prefix-types))
          (do (when callback? (infer ctx f (some-> (first known-spreads) spread-context)))
              t/never)

          invalid-tail
          (do (report! ctx tail "TSP2345" (str "apply cannot spread " (t/show invalid-tail)))
              t/unknown)

          exact?
          (let [label (str "apply " (parse/unparse f))]
            (apply t/union
                   (map
                    (fn [items]
                      (let [ty (if callback?
                                 (t/resolve-t (:env ctx) (infer ctx f (spread-context items)))
                                 ftype)]
                        (cond
                          (= :never (:t ty)) t/never
                          (or (= :all (:t ty)) (seq (:arities ty)))
                          (call-fn ctx node label ty (into prefix (spread-tail-nodes tail items))
                                   (into prefix-types items))
                          :else
                          (do (when-not (= :any (:t ty)) (check-composable! ctx f ty "apply"))
                              t/unknown))))
                    known-spreads)))

          candidates
          (let [label (str "apply " (parse/unparse f))
                element-node {:op :spread :pos (:pos tail) :synthetic true
                              :display (str "element of " (parse/unparse tail))}
                ;; A representative call without a prefix cannot specialize a
                ;; map edit for an arbitrarily positioned spread element.
                general (if (seq prefix) shape (update shape :arities #(mapv (fn [a] (dissoc a :map-edits)) %)))]
            (t/with-assumption
             (apply t/union
                    (for [{:keys [items exact?]} candidates]
                      (let [nodes (if exact? (spread-tail-nodes tail items) (repeat (count items) element-node))]
                        (call-fn ctx node label (if exact? shape general) (into prefix nodes)
                                 (into prefix-types items)))))
             (t/assumption tail-type)))

          :else
          (do (when (or (:strict (:opts ctx)) (checkable-spread-contract? ftype))
                (report! ctx tail "TSP410-01" "apply cannot verify the length or positions of the final argument"
                         :severity (if (:strict (:opts ctx)) :error :warning) :reasons #{:apply-tail}))
              t/unknown))))))

(defmethod special 'lazy-seq [ctx node _]
  (let [forms (args-of node)
        body (infer-body (dissoc ctx :recur-types) forms)]
    (when-let [bad (unsupported-sequence-member ctx body)]
      (report! ctx (last forms) "TSP2345" (str "lazy-seq cannot realize a sequence from " (t/show bad))))
    (coll-of :seq (if (= t/never body) t/never (elem-type ctx body)))))

(doseq [[h i] {'first 0 'second 1}]
  (defmethod special h [ctx node _]
    (if-let [source (unary-sequence-source ctx node h)]
      (if (= t/never source) t/never (sequence-position-type ctx source i))
      t/unknown)))

(defmethod special 'key [ctx node _]
  (entry-access ctx node 'key 0))

(defmethod special 'val [ctx node _]
  (entry-access ctx node 'val 1))

(defmethod special 'ffirst [ctx node _]
  (if-let [source (unary-sequence-source ctx node 'ffirst)]
    (if (= t/never source)
      t/never
      (or (checked-sequence-position ctx (first (args-of node)) "ffirst"
                                     (sequence-position-type ctx source 0) 0)
          t/unknown))
    t/unknown))

(defmethod special 'rand-nth [ctx node _]
  (if-let [source (unary-sequence-source ctx node 'rand-nth)]
    (let [arg (first (args-of node))
          members (t/members (:env ctx) (eff ctx source))
          known-empty? #(and (not= :nil (:t %)) (= 0 (second (sequence-member-bounds %))))]
      (when-let [bad (first (filter unordered-collection? members))]
        (report! ctx arg "TSP2345" (str "rand-nth cannot index " (t/show bad))))
      (when (some #(and (not (unordered-collection? %)) (known-empty? %)) members)
        (report! ctx arg "TSP330-02" "rand-nth cannot index an empty collection"))
      (t/with-assumption
       (apply t/union
              (for [member members]
                (cond
                  (#{:nil :never} (:t member)) member
                  (or (unordered-collection? member) (known-empty? member)) t/never
                  (:items member) (apply t/union (:items member))
                  :else (member-element member))))
       (t/assumption source)))
    t/unknown))

(defn- infer-nth-positions [ctx index members values default-type]
  (let [candidates (mapv (fn [value] [value (checked-int-index value)]) values)
        positions (vec (distinct (keep second candidates)))
        accesses (for [i positions, member members]
                   [i member (when (= :coll (:t member)) (length/position member i))])]
    (when-let [[bad] (first (filter (comp nil? second) candidates))]
      (report! ctx index "TSP330-02" (str "index " bad " cannot be represented as a JVM int")))
    (when-let [[i member] (and (nil? default-type) (first (filter #(= :absent (last %)) accesses)))]
      (report! ctx index "TSP330-02"
               (str "index " i
                    (if (and (= 1 (count values)) (every? #(= :absent (last %)) accesses)) " is" " may be")
                    " out of bounds for length " (length/describe member))))
    (if (seq positions)
      (apply t/union
             (for [[i member presence] accesses]
               (cond
                 (= :never (:t member)) t/never
                 (= :nil (:t member)) (or default-type t/nil-t)
                 (= :absent presence) (or default-type t/never)
                 (:items member) (nth (:items member) i)
                 (= :present presence) (:of member)
                 :else (t/union (elem-type ctx member) (or default-type t/never)))))
      t/unknown)))

(defmethod special 'nth [ctx node _]
  (let [[c index default & extra :as args] (args-of node)
        ty (eff ctx (infer ctx c))
        it (eff ctx (infer ctx index))
        default-type (when default (infer ctx default))
        env (:env ctx)
        members (t/members env ty)
        indices (t/members env it)]
    (cond
      (or (nil? index) (seq extra))
      (do (run! #(infer ctx %) extra)
          (report! ctx node "TSP2554" (str "nth expects 2 or 3 arguments, got " (count args)))
          t/unknown)

      (= :mismatch (:status (t/sub-result env it (t/prim :number))))
      (do (assignable! ctx index it (t/prim :number))
          t/unknown)

      (some #{t/never} [ty it default-type]) t/never

      (every? #(and (= :lit (:t %)) (number? (:val %))) indices)
      (t/with-assumption
       (infer-nth-positions ctx index members (vec (distinct (map :val indices))) default-type)
       (or (t/assumption ty) (t/assumption it)))

      :else (apply t/union (or default-type t/never)
                   (map (fn [m]
                          (cond (= :nil (:t m)) (or default-type t/nil-t)
                                (and (= :coll (:t m)) (= 0 (second (length/bounds m)))) t/never
                                :else (elem-type ctx m)))
                        members)))))

(defn- array-argument-status [ty]
  (case (:t ty)
    (:any :never) :proved
    (:unknown :var :rigid :ivar) :inconclusive
    :nil :nil
    :class (if-let [^Class cls (t/load-class (:name ty))]
             (cond
               (.isArray cls) :proved
               (.isAssignableFrom cls (t/load-class "[B")) :inconclusive
               :else :mismatch)
             :inconclusive)
    :mismatch))

(defn- checked-array-members [ctx node head source]
  (let [members (t/members (:env ctx) source)
        by-status (group-by array-argument-status members)]
    (cond
      (seq (:mismatch by-status))
      (report! ctx node "TSP2345"
               (str head " expects a JVM array, got " (t/show (first (:mismatch by-status)))))

      (seq (:nil by-status))
      (report! ctx node "TSP2531" (str head " argument may be nil"))

      (seq (:inconclusive by-status))
      (report! ctx node "TSP210-01" (str head " argument is not known to be a JVM array")
               :severity (if (:strict (:opts ctx)) :error :warning) :reasons #{:unknown}))
    members))

(defn- array-clone-type [ctx array head source]
  (let [members (checked-array-members ctx array head (eff ctx source))]
    (t/with-assumption
     (apply t/union (map #(dissoc % :platform)
                         (remove #(#{:nil :mismatch} (array-argument-status %)) members)))
     (t/assumption source))))

(defmethod special 'aclone [ctx node _]
  (let [[array :as args] (args-of node)
        [source] (mapv #(infer ctx %) args)]
    (if (= 1 (count args))
      (array-clone-type ctx array 'aclone source)
      (do (report! ctx node "TSP2554" "aclone expects one JVM array")
          t/unknown))))

(defn- known-array-lengths [members]
  (when (and (seq members) (every? :array-length members))
    (into #{} (map :array-length) members)))

(defmethod special 'alength [ctx node _]
  (let [[arg :as args] (args-of node)
        [source :as types] (mapv #(eff ctx (infer ctx %)) args)]
    (cond
      (not= 1 (count types))
      (do (report! ctx node "TSP2554" "alength expects one argument") t/unknown)

      (= t/never source) t/never

      :else
      (let [members (checked-array-members ctx arg 'alength source)]
        (if-let [lengths (known-array-lengths members)]
          (t/with-assumption (apply t/union (map #(t/lit (int %)) (sort lengths)))
                             (t/assumption source))
          (t/prim :int))))))

(defn- array-component ^Class [member]
  (when (= :class (:t member))
    (some-> ^Class (t/load-class (:name member)) .getComponentType)))

(defn- array-read-type [member]
  (case (:t member)
    (:nil :never) t/never
    :any t/any
    :class (if-let [component (array-component member)]
             (let [name (.getName component)
                   elem (t/class-type (if (= "char" name) "java.lang.Character" name))]
               (if (.isPrimitive component) elem (t/union elem t/nil-t)))
             t/unknown)
    t/unknown))

(def ^:private array-index-cast-type
  (t/union (t/prim :number) (t/class-type "java.lang.Character")))

(defn- numeric-literals [env node ty]
  (if (and (= :char (:op node)) (= ty (t/class-type "java.lang.Character")))
    [(:val node)]
    (filter number? (map :val (t/members env ty)))))

(defn- checked-array-indices [ctx indices index-types inline?]
  (let [last-index (dec (count indices))]
    (mapv (fn [idx ty i]
            (let [char-allowed? (or inline? (< i last-index))
                  literals (numeric-literals (:env ctx) idx ty)
                  candidates (mapv (fn [v] [v (checked-int-index v)])
                                   (if char-allowed? literals (filter number? literals)))]
              (assignable! ctx idx ty (if char-allowed? array-index-cast-type (t/prim :number)))
              {:node idx :candidates candidates :library-cast? (not inline?)
               :complete? (= (count candidates) (count (t/members (:env ctx) ty)))}))
          indices index-types (range))))

(defn- array-index-out-of-bounds? [member i]
  (and (some? i) (array-component member)
       (or (neg? i) (some-> (:array-length member) (<= i)))))

(defn- checked-array-bounds [ctx members {:keys [node candidates library-cast? complete?]}]
  ;; The fixed arity inlines int at the call site, where unchecked math
  ;; can change overflow behavior. The library's own casts are checked.
  (let [overflow (when library-cast? (first (filter (comp nil? second) candidates)))
        outside (first (for [[value i] candidates, member members
                             :when (array-index-out-of-bounds? member i)]
                         [value i member]))
        always-fails? (and complete? (seq candidates)
                           (every? (fn [[_ i]]
                                     (if (nil? i)
                                       library-cast?
                                       (every? #(array-index-out-of-bounds? % i) members)))
                                   candidates))]
    (cond
      overflow
      (report! ctx node "TSP330-02" (str "array index " (pr-str (first overflow)) " cannot be represented as a JVM int"))

      outside
      (let [[value i member] outside]
        (report! ctx node "TSP330-02"
                 (str "array index " (pr-str value)
                      (when (not= value i) (str " (converted to " i ")"))
                      (if always-fails? " is" " may be") " out of bounds"
                      (when-let [n (:array-length member)] (str " for length " n))))))
    (if always-fails? [] members)))

(defmethod special 'aget [ctx node _]
  (let [[array & indices :as args] (args-of node)
        [source & index-types :as types] (mapv #(eff ctx (infer ctx %)) args)]
    (cond
      (< (count types) 2)
      (do (report! ctx node "TSP2554" "aget expects an array and at least one index") t/unknown)

      (some #{t/never} types) t/never

      :else
      (let [indices (checked-array-indices ctx indices index-types (= 2 (count args)))]
        (t/with-assumption
         (loop [current source, receiver array, [idx & more] indices]
           (let [members (checked-array-members ctx receiver 'aget current)
                 element (apply t/union (map array-read-type (checked-array-bounds ctx members idx)))]
             (if (and (seq more) (not (#{:unknown :any :never} (:t element))))
               (recur element (:node idx) more)
               element)))
         (t/assumption source))))))

(defn- fixed-array-component? [^Class component]
  (if (.isArray component)
    (recur (.getComponentType component))
    (Modifier/isFinal (.getModifiers component))))

(defn- unchecked-array-store [ctx message]
  ["TSP210-01" message {:severity (if (:strict (:opts ctx)) :error :warning)
                         :reasons #{:unsupported}}])

(defn- boxed-store-problems [ctx head value ty component-type class-name]
  (let [env (:env ctx)
        expected (t/union {:t :class :name class-name} t/nil-t)
        unchecked [(unchecked-array-store ctx (str head " cannot verify the boxed value class for " class-name))]]
    (mapcat (fn [actual]
              (let [literal (:val actual)
                    boxed (t/literal-number-class actual)]
                (cond
                  boxed
                  (problems ctx value
                            {:t :class :name (.getName ^Class boxed) :as (t/show actual)}
                            expected)

                  (and (:ambiguous-number-box? actual) (t/sub? env (t/lit-prim literal) component-type))
                  unchecked

                  ;; Numeric predicates do not identify the boxed JVM class.
                  (and (= :prim (:t actual)) (t/sub? env component-type actual))
                  unchecked

                  :else (problems ctx value actual expected))))
            (t/members env ty))))

(def ^:private primitive-array-stores
  {'boolean [boolean #{"boolean"}]
   'byte    [byte    #{"byte" "short" "int" "long" "float" "double"}]
   'char    [char    #{"char" "int" "long" "float" "double"}]
   'short   [short   #{"short" "int" "long" "float" "double"}]
   'int     [int     #{"int" "long" "float" "double"}]
   'long    [long    #{"long" "float" "double"}]
   'float   [float   #{"float" "double"}]
   'double  [double  #{"double"}]})

(def ^:private primitive-boxes
  {'boolean Boolean 'byte Byte 'char Character 'short Short
   'int Integer 'long Long 'float Float 'double Double})

(def ^:private unboxed
  (into {} (map (fn [[name boxed]] [boxed name])) primitive-boxes))

(defn- reflected-aset-match? [component boxed]
  ;; Clojure 1.12's Reflector retries with widened boxed arguments.
  (let [widened (case (unboxed boxed)
                  (byte short int) Long
                  float Double
                  boxed)]
    (or (Reflector/paramArgTypeMatch component boxed)
        (Reflector/paramArgTypeMatch component widened))))

(defn- inline-aset-status [^Class component boxed compiler-class static-array? literal]
  (let [candidate (or compiler-class (some-> (unboxed boxed) str t/load-class))
        compiled? (and candidate (Reflector/paramArgTypeMatch component candidate))
        reflected? (reflected-aset-match? component boxed)
        overflow? (and compiled? (= Integer/TYPE component) (= Long boxed) (number? literal)
                       (not (<= Integer/MIN_VALUE literal Integer/MAX_VALUE)))]
    (cond
      overflow? :overflow-risk
      (or reflected? (and compiled? static-array? compiler-class)) :proved
      compiled? :inconclusive
      :else :mismatch)))

(defn- compiler-primitive-operand ^Class [ctx value]
  (let [head (builtin-head ctx value)
        args (args-of value)]
    (cond
      (and (= 1 (count args)) (primitive-boxes head))
      (t/load-class (str head))

      (and (= 'aget head) (= 2 (count args)))
      (when-let [^Class component (some-> ^Class (some-> (compiler-value-class ctx (first args)) t/load-class)
                                          .getComponentType)]
        (when (.isPrimitive component) component))

      (and (= :sym (:op value)) (empty? (:directives value)))
      (let [class-name (if-let [sym (plain-local ctx value)]
                         (get-in ctx [:compiler-local-classes sym])
                         (when-not (env/tag-node value)
                           (compiler-static-field-class ctx (:val value))))]
        (when (t/primitives class-name) (t/load-class class-name))))))

(defn- primitive-aset-problems [ctx value ty ^Class component inline? static-array?]
  (let [env (:env ctx)
        name (.getName component)
        primitive (compiler-primitive-operand ctx value)
        primitive-box (when primitive (primitive-boxes (symbol (.getName primitive))))
        expected (case name
                   "boolean" (t/prim :boolean)
                   "char" (t/class-type "java.lang.Character")
                   (if (and (not inline?) (contains? (second (primitive-array-stores 'char)) name))
                     array-index-cast-type
                     (t/prim :number)))]
    (mapcat (fn [member]
              (let [literal (:val member)
                    boxed (or primitive-box
                              (when (= :lit (:t member))
                                (if (number? literal) (t/literal-number-class member) (class literal)))
                              (when (= :class (:t member)) (t/load-class (:name member)))
                              (when (= :boolean (:name member)) Boolean))
                    compiler-class (cond
                                     primitive primitive
                                     (and (= :num (:op value)) (#{Long Double} boxed))
                                     (t/load-class (str (unboxed boxed)))
                                     (#{:num :bool :char} (:op value)) boxed)
                    status (cond
                             (#{:any :never} (:t member)) :proved
                             (= :nil (:t member)) :mismatch
                             (nil? boxed) (if (= :mismatch (:status (t/sub-result env member expected)))
                                            :mismatch :inconclusive)
                             inline? (inline-aset-status component boxed compiler-class static-array? literal)
                             (contains? (second (primitive-array-stores (unboxed boxed))) name) :proved
                             :else :mismatch)]
                (case status
                  :proved nil
                  :mismatch [["TSP2345" (str "aset cannot store " (if primitive-box (.getSimpleName ^Class primitive-box)
                                                                   (show-ref ctx member))
                                             " in " name "[]")]]
                  :overflow-risk [(unchecked-array-store ctx "aset int conversion may overflow with checked math")]
                  :inconclusive [(unchecked-array-store ctx (str "aset cannot verify the JVM value type for " name "[]"))])))
            (t/members env ty))))

(defn- reference-store-problems [ctx head value ty ^Class component]
  (let [name (.getName component)
        component-type (t/class-type name)]
    (if (#{:int :double} (:name component-type))
      (boxed-store-problems ctx head value ty component-type name)
      (problems ctx value ty (t/union component-type t/nil-t)))))

(defn- array-store-problems [ctx value ty member inline? compiler-array]
  (when-let [component (array-component member)]
    (if (.isPrimitive component)
      (primitive-aset-problems ctx value ty component inline? (= compiler-array (:name member)))
      (or (seq (reference-store-problems ctx 'aset value ty component))
          (when (and (not (:exact-array? member))
                     (not (fixed-array-component? component))
                     (not (#{:nil :any} (:t (t/resolve-t (:env ctx) ty)))))
            [(unchecked-array-store ctx (str "aset runtime array component may be narrower than " (.getName component)))])))))

(defn- array-store-target [ctx head array source indices]
  (loop [current source, receiver array, [idx & more] indices]
    (let [members (checked-array-bounds ctx (checked-array-members ctx receiver head current) idx)]
      (if (and (seq more) (seq members))
        (recur (apply t/union (map array-read-type members)) (:node idx) more)
        [receiver members]))))

(defn- aset-inline-result [receivers value-type]
  (if (some #(some-> (array-component %) .isPrimitive) receivers)
    (apply t/union
           (for [member receivers :when (not (#{:nil :never} (:t member)))]
             (if-let [component (array-component member)]
               (if (.isPrimitive component) (array-read-type member) value-type)
               t/unknown)))
    value-type))

(defn- report-store-problem! [ctx value issues]
  (when-let [[code message extra] (or (first (remove #(= "TSP210-01" (first %)) issues)) (first issues))]
    (apply report! ctx value code message (mapcat identity extra))))

(defn- check-array-store-value! [ctx value value-type receivers inline? compiler-array]
  (let [ty (eff ctx value-type)]
    (report-store-problem! ctx value (mapcat #(array-store-problems ctx value ty % inline? compiler-array) receivers))))

(defmethod special 'aset [ctx node _]
  (let [args (args-of node)
        types (mapv #(infer ctx %) args)
        source (eff ctx (first types))]
    (cond
      (< (count args) 3)
      (do (report! ctx node "TSP2554" "aset expects an array, at least one index and a value") t/unknown)

      (some #{t/never} types) t/never

      :else
      (let [indices (subvec args 1 (dec (count args)))
            index-types (mapv #(eff ctx %) (subvec types 1 (dec (count types))))
            inline? (= 1 (count indices))
            value (peek args)
            value-type (peek types)
            compiler-array (when inline? (compiler-value-class ctx (first args)))
            indices (checked-array-indices ctx indices index-types inline?)
            [_ receivers] (array-store-target ctx 'aset (first args) source indices)]
        (if (seq receivers)
          (do (check-array-store-value! ctx value value-type receivers inline? compiler-array)
              (t/with-assumption (if inline? (aset-inline-result receivers value-type) value-type)
                                 (or (t/assumption value-type) (t/assumption source))))
          t/never)))))

(defn- array-loop-binding-problem [index target]
  (if-let [malformed (some #(when-not (and (= :sym (:op %)) (simple-symbol? (:val %))) %)
                           [index target])]
    [malformed "expects a local symbol for this binding"]
    (when (= (:val index) (:val target))
      [index "requires distinct index and result bindings"])))

(defmethod special 'amap [ctx node _]
  (let [[array index target body :as args] (args-of node)
        source (when array (infer ctx array))
        [bad-binding message] (array-loop-binding-problem index target)]
    (cond
      (not= 4 (count args))
      (do (report! ctx node "TSP2554" "amap expects an array, an index name, a result name and an expression")
          t/unknown)

      bad-binding
      (do (report! ctx bad-binding "TSP2345" (str "amap " message))
          t/unknown)

      (= t/never source) t/never

      :else
      (let [clone-type (array-clone-type ctx array 'amap source)]
        (when-not (or (= t/never clone-type)
                      (= #{0} (known-array-lengths (t/members (:env ctx) clone-type))))
          (let [target-sym (:val target)
                index-sym (:val index)
                compiler-array (when-not (env/tag-node target)
                                 (clone-compiler-class (compiler-value-class ctx array)))
                target-ctx (cond-> (bind-pattern ctx target clone-type)
                             compiler-array (assoc-in [:compiler-local-classes target-sym] compiler-array)
                             compiler-array (update :hinted conj target-sym))
                body-ctx (-> (bind-pattern target-ctx index (t/prim :int))
                             (assoc-in [:compiler-local-classes index-sym] "long")
                             (dissoc :recur-types))
                target-type (record! target-ctx target (get-in target-ctx [:locals target-sym]))
                _ (record! body-ctx index (get-in body-ctx [:locals index-sym]))
                body-type (infer body-ctx body)]
            (check-array-store-value! body-ctx body body-type (t/members (:env ctx) (eff ctx target-type))
                                      true (get-in target-ctx [:compiler-local-classes target-sym]))))
        clone-type))))

(defn- numeric-coercion-error [coerce value]
  (try
    (coerce value)
    nil
    (catch IllegalArgumentException e (ex-message e))
    (catch ArithmeticException e (ex-message e))))

(defn- check-primitive-store-value! [ctx head value actual coerce]
  (when-not (= 'aset-boolean head)
    (let [expected (if (#{'aset-float 'aset-double} head) (t/prim :number) array-index-cast-type)]
      (assignable! ctx value actual expected)
      (when (t/sub? (:env ctx) actual expected)
        (when-let [message (some #(numeric-coercion-error coerce %)
                                 (numeric-literals (:env ctx) value actual))]
          (report! ctx value "TSP2345" (str head " cannot coerce the value (" message ")")))))))

;; Array.setX accepts the JVM widening conversions from X to the component.
(doseq [[cast [coerce components]] primitive-array-stores
        :let [head (symbol (str "aset-" cast))]]
  (defmethod special head [ctx node _]
    (let [[array :as args] (args-of node)
          [source :as types] (mapv #(infer ctx %) args)]
      (cond
        (< (count args) 3)
        (do (report! ctx node "TSP2554" (str head " expects an array, at least one index and a value"))
            t/unknown)

        (some #{t/never} types) t/never

        :else
        (let [indices (subvec args 1 (dec (count args)))
              index-types (mapv #(eff ctx %) (subvec types 1 (dec (count types))))
              value-type (peek types)
              indices (checked-array-indices ctx indices index-types false)
              [target receivers] (array-store-target ctx head array (eff ctx source) indices)]
          (if (seq receivers)
            (do (when-let [bad (some #(when-let [component (array-component %)]
                                      (when-not (components (.getName component)) %))
                                   receivers)]
                  (report! ctx target "TSP2345" (str head " cannot write to " (t/show bad))))
                (check-primitive-store-value! ctx head (peek args) (eff ctx value-type) coerce)
                (t/with-assumption value-type (or (t/assumption value-type) (t/assumption source))))
            t/never))))))

(defn- literal-array-size [member]
  (let [value (:val member)]
    ;; The constructors have different numeric casts. An in-range
    ;; nonnegative size truncates to the same int in both paths.
    (when (and (= :lit (:t member)) (number? value) (<= 0 value Integer/MAX_VALUE))
      (int value))))

(defn- exact-sequence-length [member]
  (let [n (case (:t member)
            :nil 0
            :coll (let [[lo hi] (length/bounds member)] (when (= lo hi) lo))
            :class (:array-length member)
            nil)]
    (when (and (integer? n) (<= 0 n Integer/MAX_VALUE)) n)))

(defn- shared-length [lengths]
  (when (and (some? (first lengths)) (apply = lengths))
    (first lengths)))

(doseq [[head class-name] array-constructors]
  (defmethod special head [ctx node _]
    (let [[size :as args] (args-of node)
          [source :as types] (mapv #(eff ctx (infer ctx %)) args)
          primitive? (not= 'object-array head)
          expected-size (t/prim :number)]
      (cond
        (not (or (= 1 (count types)) (and primitive? (= 2 (count types)))))
        (do (report! ctx node "TSP2554" (str head " expects " (if primitive? "one or two arguments" "one argument")))
            t/unknown)

        (some #{t/never} types) t/never

        :else
        (let [members (t/members (:env ctx) source)
              sized? (= 2 (count types))
              n (shared-length (map #(or (literal-array-size %)
                                         (when-not sized? (exact-sequence-length %)))
                                    members))]
          (if sized?
            (assignable! ctx size source expected-size)
            (when-let [bad (some #(when-not (t/sub? (:env ctx) % expected-size)
                                  (unsupported-sequence-member ctx %))
                                 members)]
              (report! ctx size "TSP2345" (str head " expects a size or a seqable value, got " (t/show bad)))))
          (cond-> (t/class-type class-name)
            (not primitive?) (assoc :exact-array? true)
            (some? n) (assoc :array-length n)
            (some? n) (t/with-assumption (t/assumption source))))))))

(def ^:private primitive-type-fields
  (into {"java.lang.Void" Void/TYPE}
        (map (fn [[primitive ^Class boxed]] [(.getName boxed) (t/load-class (name primitive))]))
        primitive-boxes))

(defn- array-component-class [ctx node]
  (or (some-> (class-symbol-name ctx node) t/load-class)
      (when (and (= :sym (:op node)) (empty? (:directives node)) (not (env/tag-node node))
                 (= "TYPE" (name (:val node))))
        (get primitive-type-fields (java-static? ctx (:val node))))
      (literal-for-name-class ctx node)))

(defn- dimension-error [value]
  (or (numeric-coercion-error int value)
      (when (neg? (int value))
        (str "negative array dimension " (pr-str value)))))

(defmethod special 'make-array [ctx node _]
  (let [[component & dimensions :as args] (args-of node)
        [source & sizes :as types] (mapv #(eff ctx (infer ctx %)) args)]
    (cond
      (< (count args) 2)
      (do (report! ctx node "TSP2554" "make-array expects a class and at least one dimension")
          t/unknown)

      (some #{t/never} types) t/never

      :else
      (let [^Class cls (array-component-class ctx component)
            component-rank (if cls (count (take-while #{\[} (.getName cls))) 0)
            remaining-rank (- 255 component-rank)]
        (when-not cls
          (assignable! ctx component source (t/class-type "java.lang.Class")))
        (doseq [[dim size] (map vector dimensions sizes)]
          (assignable! ctx dim size array-index-cast-type)
          (when-let [message (some dimension-error (numeric-literals (:env ctx) dim size))]
            (report! ctx dim "TSP2345" (str "make-array cannot use this dimension (" message ")"))))
        (cond
          (= Void/TYPE cls)
          (do (report! ctx component "TSP2345" "make-array cannot use void as its component")
              t/unknown)

          (> (count dimensions) remaining-rank)
          (do (report! ctx (nth dimensions remaining-rank) "TSP2345"
                       "make-array exceeds the JVM limit of 255 array dimensions")
              t/unknown)

          cls
          (let [^Class array-class (nth (iterate #(.arrayType ^Class %) cls) (count dimensions))
                n (shared-length (map literal-array-size (t/members (:env ctx) (first sizes))))]
            (cond-> (assoc (t/class-type (.getName array-class)) :exact-array? true)
              (some? n) (assoc :array-length n)
              (some? n) (t/with-assumption (t/assumption (first sizes)))))

          :else t/unknown)))))

(defn- known-object-class ^Class [ctx form member]
  (if-let [primitive (when form (compiler-primitive-operand ctx form))]
    (primitive-boxes (symbol (.getName primitive)))
    (case (:t member)
      :nil Object
      :lit (let [v (:val member)] (if (number? v) (t/literal-number-class member) (class v)))
      :class (when-let [^Class cls (t/load-class (:name member))]
               (cond
                 (.isPrimitive cls) (primitive-boxes (symbol (.getName cls)))
                 (or (:exact-array? member) (fixed-array-component? cls)) cls))
      :prim (when-not (#{:int :double :number} (:name member))
              (when-let [^Class cls (some-> (t/prim->class (:name member)) t/load-class)]
                (when (Modifier/isFinal (.getModifiers cls)) cls)))
      nil)))

(defn- boxed-java-value-type [ctx form ty]
  (apply t/union
         (for [member (t/members (:env ctx) ty)]
           (if-let [^Class cls (when-not (= :nil (:t member)) (known-object-class ctx form member))]
             (cond-> {:t :class :name (.getName cls)}
               (= :lit (:t member)) (assoc :as (t/show member)))
             member))))

(defmethod special 'set! [ctx node _]
  (let [[target value :as args] (args-of node)]
    (if (not= 2 (count args))
      (do (run! #(infer ctx %) args)
          (report! ctx node "TSP2554" "set! expects a target and a value")
          t/unknown)
      (let [{^Class field-type :type :keys [final?]} (field-assignment-target ctx target)
            value-type (infer ctx value)]
        (when field-type
          (record! ctx target (java-field-type (.getName field-type)))
          (cond
            final? (report! ctx target "TSP2345" "Cannot assign to a final JVM field")

            (.isPrimitive field-type)
            (assignable! ctx value value-type
                         (case (.getName field-type)
                           "boolean" (t/prim :boolean)
                           "char" (t/class-type "java.lang.Character")
                           (t/prim :number)))

            :else
            (report-store-problem!
             ctx value
             (reference-store-problems ctx 'set! value (boxed-java-value-type ctx value (eff ctx value-type))
                                       field-type))))
        value-type))))

(defn- inferred-array-type [ctx node ^Class component n]
  (try
    (let [^Class cls (or component Object)]
      (cond-> (t/class-type (.getName (.arrayType cls)))
        component (assoc :exact-array? true)
        (some? n) (assoc :array-length n)))
    (catch IllegalArgumentException _
      (report! ctx node "TSP2345" "into-array exceeds the JVM limit of 255 array dimensions")
      t/never)))

(defn- array-input-vector-forms [arg]
  (when (and (= :vector (:op arg)) (empty? (:directives arg)) (not (env/tag-node arg)))
    (:children arg)))

(defn- array-input-length [member]
  (if (and (= :lit (:t member)) (string? (:val member)))
    (.length ^String (:val member))
    (exact-sequence-length member)))

(defn- array-input-element [ctx member]
  (if-let [component (array-component member)]
    (let [name (.getName component)
          exact-class {:t :class :name name}]
      (cond
        (.isPrimitive component) exact-class
        (#{:int :double} (t/class->prim name)) (t/union exact-class t/nil-t)
        :else (array-read-type member)))
    (elem-type ctx member)))

(defn- implicit-into-array-member [ctx arg member]
  (let [env (:env ctx)
        items (:items member)
        forms (array-input-vector-forms arg)
        n (array-input-length member)]
    (if (= 0 n)
      (inferred-array-type ctx arg Object 0)
      (let [element (when-not items (array-input-element ctx member))
            first-type (if items (first items) element)
            classes (distinct (cond-> (mapv #(known-object-class ctx (first forms) %) (t/members env first-type))
                                (and (nil? n) (zero? (first (length/bounds member)))) (conj Object)))
            tail (cond
                   items (map-indexed (fn [i item] [(get forms (inc i)) item]) (rest items))
                   (= 1 n) []
                   :else [[nil element]])]
        (doseq [[form ty] tail]
          (let [actual (boxed-java-value-type ctx form (eff ctx ty))]
            (report-store-problem!
             ctx (or form arg)
             (mapcat #(reference-store-problems ctx 'into-array (or form arg) actual %)
                     (filter #(and % (not= Object %)) classes)))))
        (apply t/union (map #(inferred-array-type ctx arg % n) classes))))))

(defn- into-array-primitive-problems [ctx node form ty component]
  (let [env (:env ctx)
        expected (if (= "boolean" component) (t/prim :boolean) array-index-cast-type)]
    (mapcat (fn [member]
              (if-let [boxed (known-object-class ctx form member)]
                (when-not (contains? (second (primitive-array-stores (unboxed boxed))) component)
                  [["TSP2345" (str "into-array cannot store " (show-ref ctx member) " in " component "[]")]])
                (or (seq (problems ctx node member expected))
                    (when-not (#{:any :never} (:t member))
                      [(unchecked-array-store ctx (str "into-array cannot verify the JVM value type for " component "[]"))]))))
            (t/members env ty))))

(defn- explicit-into-array-elements [ctx arg members]
  (let [forms (array-input-vector-forms arg)]
    (mapcat (fn [member]
              (when (not= 0 (array-input-length member))
                (if-let [items (:items member)]
                  (map vector (concat forms (repeat nil)) items)
                  [[nil (array-input-element ctx member)]])))
            members)))

(defn- check-reference-into-array! [ctx arg members component]
  (doseq [[form ty] (explicit-into-array-elements ctx arg members)
          :let [node (or form arg)
                actual (boxed-java-value-type ctx form (eff ctx ty))]
          :when (not= :unknown (:t actual))]
    (report-store-problem! ctx node (reference-store-problems ctx 'into-array node actual component))))

(defn- check-primitive-into-array! [ctx arg members ^Class component]
  (let [name (.getName component)
        ;; RT.seqToTypedArray uses checked Clojure casts for these five components.
        coerce (when (#{"byte" "short" "int" "char" "float"} name)
                 (first (primitive-array-stores (symbol name))))
        expected (if (= "float" name) (t/prim :number) array-index-cast-type)]
    (doseq [[form ty] (explicit-into-array-elements ctx arg members)
            :let [node (or form arg)
                  actual (eff ctx ty)]]
      (if coerce
        (do (assignable! ctx node actual expected)
            (when (t/sub? (:env ctx) actual expected)
              (when-let [message (some #(numeric-coercion-error coerce %)
                                       (numeric-literals (:env ctx) form actual))]
                (report! ctx node "TSP2345" (str "into-array cannot coerce the value (" message ")")))))
        (report-store-problem! ctx node (into-array-primitive-problems ctx node form actual name))))))

(def ^:private into-array-input
  (t/union t/nil-t
           (t/class-type "clojure.lang.Seqable") (t/class-type "java.lang.Iterable")
           (t/class-type "java.lang.CharSequence") (t/class-type "java.util.Map")))

(defmethod special 'into-array [ctx node _]
  (let [[component :as args] (args-of node)
        types (mapv #(eff ctx (infer ctx %)) args)
        source (peek types)]
    (cond
      (not (#{1 2} (count args)))
      (do (report! ctx node "TSP2554" "into-array expects one or two arguments") t/unknown)

      (some #{t/never} types) t/never

      :else
      (let [env (:env ctx)
            arg (peek args)
            members (t/members env source)
            explicit? (= 2 (count args))
            ^Class cls (when explicit? (array-component-class ctx component))
            expected-class (t/class-type "java.lang.Class")
            bad (some #(when (and (not (array-component %))
                                  (= :mismatch (:status (t/sub-result env % into-array-input))))
                         %)
                      members)]
        (when (and explicit? (not cls)
                   (= :mismatch (:status (t/sub-result env (first types) expected-class))))
          (assignable! ctx component (first types) expected-class))
        (cond
          bad
          (do (report! ctx arg "TSP2345" (str "into-array cannot sequence " (t/show bad)))
              t/unknown)

          (not explicit?)
          (t/with-assumption (apply t/union (map #(implicit-into-array-member ctx arg %) members))
                             (t/assumption source))

          (= Void/TYPE cls)
          (do (report! ctx component "TSP2345" "into-array cannot use void as its component")
              t/never)

          cls
          (let [n (shared-length (map array-input-length members))]
            (if (.isPrimitive cls)
              (check-primitive-into-array! ctx arg members cls)
              (check-reference-into-array! ctx arg members cls))
            (cond-> (inferred-array-type ctx component cls n)
              (some? n) (t/with-assumption (t/assumption source))))

          :else t/unknown)))))

(def ^:private to-array-input
  (t/union t/nil-t (t/prim :string)
           (t/class-type "java.lang.Iterable") (t/class-type "java.util.Map")))

(defn- converted-object-array [n]
  (cond-> (assoc (t/class-type (array-constructors 'object-array)) :exact-array? true)
    (some? n) (assoc :array-length n)))

(defn- list-expression-length [ctx arg]
  (when (and (empty? (:directives arg)) (not (env/tag-node arg)))
    (let [head (builtin-head ctx arg)
          quoted (case (:op arg)
                   :quote (first (:children arg))
                   :list (when (= 'quote head) (second (:children arg)))
                   nil)]
      (cond
        (= 'list head) (count (args-of arg))
        (= :list (:op quoted)) (count (:children quoted))))))

(defn- to-array-member [ctx member]
  (if-let [component (array-component member)]
    ;; RT.toArray returns reference arrays directly and boxes primitive arrays.
    {:type (if (.isPrimitive component)
             (converted-object-array (:array-length member))
             (dissoc member :platform))}
    (let [object-array (t/class-type (array-constructors 'object-array))]
      (if (= :any (:t member))
        {:type object-array}
        (let [result (t/sub-result (:env ctx) member to-array-input)]
          (case (:status result)
            :proved {:type (converted-object-array
                            (if (and (= :lit (:t member)) (string? (:val member)))
                              (.length ^String (:val member))
                              (exact-sequence-length member)))}
            :inconclusive {:type object-array :reasons (:reasons result)}
            ;; list? names IPersistentList, which does not require Iterable.
            :mismatch (if (= "clojure.lang.IPersistentList" (:name member))
                        {:type (converted-object-array nil) :reasons #{:unsupported}}
                        {:bad member})))))))

(defn- array-conversion-results [ctx form source]
  (if-some [n (list-expression-length ctx form)]
    [{:type (converted-object-array n)}]
    (mapv #(to-array-member ctx %) (t/members (:env ctx) source))))

(defn- report-array-conversion! [ctx node label results]
  (let [bad (some :bad results)
        unchecked (into #{} (mapcat :reasons) results)]
    (cond
      bad (report! ctx node "TSP2345"
                   (str label " expects nil, a JVM array, an Iterable, a Map or a string, got " (t/show bad)))
      (seq unchecked) (report! ctx node "TSP210-01" (str label " input type is not checked")
                               :severity (if (:strict (:opts ctx)) :error :warning)
                               :reasons unchecked))))

(defmethod special 'to-array [ctx node _]
  (let [[arg :as args] (args-of node)
        [source :as types] (mapv #(eff ctx (infer ctx %)) args)]
    (cond
      (not= 1 (count types))
      (do (report! ctx node "TSP2554" "to-array expects one argument") t/unknown)

      (= t/never source) t/never

      :else
      (let [results (array-conversion-results ctx arg source)]
        (report-array-conversion! ctx arg "to-array" results)
        (t/with-assumption (apply t/union (keep :type results)) (t/assumption source))))))

(defn- array-rows-result [env member known-list?]
  (cond
    known-list? {:status :proved}
    (= "clojure.lang.IPersistentList" (:name member)) {:status :inconclusive :reasons #{:unsupported}}
    :else (t/sub-result env member (t/class-type "java.util.Collection"))))

(defn- check-array-rows! [ctx arg member forms n]
  (when (and (not= :any (:t member)) (not= 0 n) (not= 0 (second (length/bounds member))))
    (if-let [items (:items member)]
      (doseq [[i ty] (map-indexed vector items)
              :let [form (get forms i)]]
        (report-array-conversion! ctx (or form arg) "to-array-2d row"
                                  (array-conversion-results ctx form (eff ctx ty))))
      (report-array-conversion! ctx arg "to-array-2d row"
                                (array-conversion-results ctx nil (eff ctx (elem-type ctx member)))))))

(defmethod special 'to-array-2d [ctx node _]
  (let [[arg :as args] (args-of node)]
    (if (not= 1 (count args))
      (do (run! #(infer ctx %) args)
          (report! ctx node "TSP2554" "to-array-2d expects one argument")
          t/unknown)
      (let [list? (= 'list (builtin-head ctx arg))
            forms (cond
                    list? (args-of arg)
                    (= :vector (:op arg)) (:children arg))
            source (eff ctx
                        (if list?
                          (let [items (mapv #(dissoc (infer ctx %) :fresh) forms)]
                            (record! ctx arg (if (some #{t/never} items) t/never
                                                (assoc (t/tuple items) :kind :seq))))
                          (infer ctx arg)))
            env (:env ctx)
            n (list-expression-length ctx arg)
            members (remove #(= :never (:t %)) (t/members env source))
            results (mapv #(vector % (array-rows-result env % (some? n))) members)
            rejected (keep (fn [[m r]] (when (= :mismatch (:status r)) m)) results)
            bad (first (remove #(= :nil (:t %)) rejected))
            unchecked (into #{} (comp (map second) (mapcat :reasons)) results)
            accepted (keep (fn [[m r]] (when (= :proved (:status r)) m)) results)]
        (cond
          bad
          (report! ctx arg "TSP2345" (str "to-array-2d expects a java.util.Collection, got " (t/show bad)))
          (seq rejected)
          (report! ctx arg "TSP2531" "to-array-2d collection may be nil")
          (seq unchecked)
          (report! ctx arg "TSP210-01" "to-array-2d collection type is not checked"
                   :severity (if (:strict (:opts ctx)) :error :warning) :reasons unchecked))
        (doseq [member accepted]
          (check-array-rows! ctx arg member forms n))
        (if (or (seq accepted) (seq unchecked))
          (let [row-count (or n (when-not (seq unchecked) (shared-length (map exact-sequence-length accepted))))]
            (t/with-assumption
             (cond-> (assoc (t/class-type "[[Ljava.lang.Object;") :exact-array? true)
               (some? row-count) (assoc :array-length row-count))
             (t/assumption source)))
          t/never)))))

(doseq [[h [offset nullable?]] {'seq [0 true] 'next [1 true] 'rest [1 false]}]
  (defmethod special h [ctx node _]
    (if-let [source (unary-sequence-source ctx node h)]
      (sequence-tail ctx source offset nullable?)
      t/unknown)))

(defmethod special 'list [ctx node _]
  (let [items (mapv #(dissoc (infer ctx %) :fresh) (args-of node))]
    (if (some #{t/never} items)
      t/never
      (list-value-type ctx items))))

(defn- distinct-positions [items]
  (when (and items (every? #(#{:lit :nil} (:t %)) items))
    (second
     (reduce (fn [[seen result] item]
               (let [value (:val item)]
                 (if (contains? seen value)
                   [seen result]
                   [(conj seen value) (conj result item)])))
             [#{} []] items))))

(defn- unique-sequence [ctx source kind]
  (t/with-assumption
   (apply t/union
          (for [member (t/members (:env ctx) source)]
            (if (= t/never member)
              t/never
              (let [member (or (map-entry-sequence member) member)
                    items (distinct-positions (:items member))
                    [lo hi] (sequence-member-bounds member)
                    lower (cond items (count items) (= :set (:kind member)) lo :else (min 1 lo))
                    upper (if items (count items) hi)
                    element (cond (= 0 upper) t/never
                                  items (apply t/union items)
                                  :else (member-element member))]
                (cond-> (length/with-bounds (coll-of kind element) lower upper)
                  (and items (= :seq kind)) (assoc :items items))))))
   (t/assumption source)))

(defmethod special 'distinct [ctx node _]
  (if (empty? (args-of node))
    t/unknown
    (if-let [source (unary-sequence-source ctx node 'distinct)]
      (if-let [bad (some #(when (unordered-collection? %) %) (t/members (:env ctx) (eff ctx source)))]
        (do (report! ctx (first (args-of node)) "TSP2345"
                     (str "distinct cannot read the first position from " (t/show bad)))
            t/unknown)
        (unique-sequence ctx source :seq))
      t/unknown)))

(defmethod special 'set [ctx node _]
  (if-let [source (unary-sequence-source ctx node 'set)]
    (unique-sequence ctx source :set)
    t/unknown))

(defn- slice-limit [ctx amount]
  (let [value (:val (t/resolve-t (:env ctx) amount))]
    (when (number? value)
      (cond
        (not (pos? value)) 0
        (<= value 1) 1
        (integer? value) value))))

(defn- check-sequence-slice [ctx node head transducer? transform]
  (let [[amount source :as args] (args-of node)
        [n ty :as types] (mapv #(eff ctx (infer ctx %)) args)]
    (cond
      (and transducer? (= 1 (count types))) t/unknown
      (not= 2 (count types))
      (do (report! ctx node "TSP2554" (str head (if transducer? " expects 1 or 2 arguments" " expects 2 arguments")))
          t/unknown)
      (some #{t/never} types) t/never
      (= :mismatch (:status (t/sub-result (:env ctx) n (t/prim :number))))
      (do (assignable! ctx amount n (t/prim :number)) t/unknown)
      :else
      (let [limit (slice-limit ctx n)]
        (if (and (= 'take head) (= 0 limit))
          (length/with-bounds (coll-of :seq t/never) 0 0)
          (if-let [bad (unsupported-sequence-member ctx ty)]
            (do (report! ctx source "TSP2345" (str head " cannot sequence " (t/show bad))) t/unknown)
            (t/with-assumption (transform ty limit) (or (t/assumption n) (t/assumption ty)))))))))

(defn- limited-sequence [ctx from-end? source limit]
  (apply t/union
         (for [member (t/members (:env ctx) source)
               :let [member (or (map-entry-sequence member) member)
                     [lo hi] (sequence-member-bounds member)
                     upper (if limit (if hi (min hi limit) limit) hi)
                     items (when (and limit (:items member))
                             (let [items (:items member)]
                               (if from-end?
                                 (subvec items (- (count items) (int upper)))
                                 (subvec items 0 (int upper)))))
                     element (cond (= 0 upper) t/never
                                   items (apply t/union items)
                                   :else (member-element member))
                     lower (if limit (min lo limit) 0)
                     result (cond-> (length/with-bounds (coll-of :seq element) lower upper)
                              items (assoc :items items))]]
           (cond
             (not from-end?) result
             (= 0 upper) t/nil-t
             (pos? lower) result
             :else (t/union (length/with-bounds result 1 upper) t/nil-t)))))

(defmethod special 'take [ctx node _]
  (check-sequence-slice ctx node 'take true (partial limited-sequence ctx false)))

(defn- skipped-sequence [ctx source limit nullable?]
  (if (some? limit)
    (sequence-tail ctx source limit nullable?)
    (apply t/union
           (for [member (t/members (:env ctx) source)
                 :let [member (or (map-entry-sequence member) member)
                       [_ hi] (sequence-member-bounds member)]]
             (cond
               (not nullable?) (length/with-bounds (coll-of :seq (if (= 0 hi) t/never (member-element member))) 0 hi)
               (= 0 hi) t/nil-t
               :else (t/union t/nil-t (length/with-bounds (coll-of :seq (member-element member)) 1 hi)))))))

(defmethod special 'drop [ctx node _]
  (check-sequence-slice ctx node 'drop true
                        (fn [source limit] (skipped-sequence ctx source limit false))))

(defmethod special 'nthrest [ctx node _]
  (let [[source amount :as args] (args-of node)
        [ty n :as types] (mapv #(eff ctx (infer ctx %)) args)]
    (cond
      (not= 2 (count types))
      (do (report! ctx node "TSP2554" "nthrest expects two arguments") t/unknown)

      (some #{t/never} types) t/never

      (= :mismatch (:status (t/sub-result (:env ctx) n (t/prim :number))))
      (do (assignable! ctx amount n (t/prim :number)) t/unknown)

      :else
      (let [limit (slice-limit ctx n)
            raw (:val (t/resolve-t (:env ctx) n))
            positive? (and (number? raw) (pos? raw))]
        (cond
          (= 0 limit) (t/with-assumption ty (t/assumption n))
          (and (not positive?) (unsupported-sequence-member ctx ty)) ty
          (report-invalid-sequence-args! ctx 'nthrest [source] [ty]) t/unknown
          :else
          (t/with-assumption
           (cond->> (skipped-sequence ctx ty limit false)
             (not positive?) (t/union ty))
           (or (t/assumption n) (t/assumption ty))))))))

(defmethod special 'nthnext [ctx node _]
  (let [[source amount :as args] (args-of node)
        [ty n :as types] (mapv #(eff ctx (infer ctx %)) args)]
    (cond
      (not= 2 (count types))
      (do (report! ctx node "TSP2554" "nthnext expects two arguments") t/unknown)

      (some #{t/never} types) t/never
      (report-invalid-sequence-args! ctx 'nthnext [source] [ty]) t/unknown

      :else
      (let [members (map #(or (map-entry-sequence %) %) (t/members (:env ctx) ty))]
        (if (and (every? #(or (= :vector (:kind %))
                              (pos? (first (sequence-member-bounds %)))) members)
                 (= :mismatch (:status (t/sub-result (:env ctx) n (t/prim :number)))))
          (do (assignable! ctx amount n (t/prim :number)) t/unknown)
          (t/with-assumption (skipped-sequence ctx ty (slice-limit ctx n) true)
                             (or (t/assumption n) (t/assumption ty))))))))

(defmethod special 'take-last [ctx node _]
  (check-sequence-slice ctx node 'take-last false (partial limited-sequence ctx true)))

(defmethod special 'split-at [ctx node _]
  (check-sequence-slice
   ctx node 'split-at false
   (fn [source limit]
     (apply t/union
            (for [member (t/members (:env ctx) source)]
              (t/tuple [(limited-sequence ctx false member limit)
                        (skipped-sequence ctx member limit false)]))))))

(defn- partition-count [size step]
  (when (some? size)
    (cond
      (zero? size) 0
      (and step (pos? step)) (inc (quot (dec size) step)))))

(defmethod special 'take-nth [ctx node _]
  (let [[amount coll :as args] (args-of node)
        [n source :as types] (mapv #(eff ctx (infer ctx %)) args)]
    (cond
      (not (#{1 2} (count types)))
      (do (report! ctx node "TSP2554" "take-nth expects 1 or 2 arguments") t/unknown)

      (some #{t/never} types) t/never
      (= 1 (count types)) t/unknown
      (report-invalid-sequence-args! ctx 'take-nth [coll] [source]) t/unknown

      :else
      (let [env (:env ctx)
            raw (:val (t/resolve-t env n))
            step (slice-limit ctx n)
            members (map #(or (map-entry-sequence %) %) (t/members env source))]
        (when (and (some #(not= 0 (second (sequence-member-bounds %))) members)
                   (= :mismatch (:status (t/sub-result env n (t/prim :number)))))
          (assignable! ctx amount n (t/prim :number)))
        (t/with-assumption
         (apply t/union
                (for [member members
                      :let [[lo hi] (sequence-member-bounds member)
                            items (:items member)
                            selected (when (and items step (pos? step))
                                       (into [] (keep-indexed #(when (zero? (rem %1 step)) %2)) items))
                            lower (or (partition-count lo step) (min 1 lo))
                            upper (or (partition-count hi step)
                                      (when (and (number? raw) (pos? raw)) hi))
                            element (cond
                                      (= 0 upper) t/never
                                      selected (apply t/union selected)
                                      (and (= 0 step) (seq items)) (first items)
                                      :else (member-element member))]]
                  (cond-> (length/with-bounds (coll-of :seq element) lower upper)
                    selected (assoc :items selected))))
         (or (t/assumption n) (t/assumption source)))))))

(defn- partition-all-type [ctx source size step]
  (t/with-assumption
   (apply t/union
          (for [member (t/members (:env ctx) source)
                :let [[lo hi] (if (= :nil (:t member)) [0 0] (length/bounds member))
                      minimum (or (partition-count lo step) (min 1 lo))
                      maximum (partition-count hi step)
                      chunk (cond
                              (= 0 hi) t/never
                              (and size (not (pos? size))) (length/with-bounds (coll-of :seq t/never) 0 0)
                              :else (length/with-bounds (coll-of :seq (elem-type ctx member))
                                                        (if size 1 0)
                                                        (cond (and size hi) (min size hi) size size :else hi)))]]
            (length/with-bounds (coll-of :seq chunk) minimum maximum)))
   (t/assumption source)))

(defmethod special 'partition-all [ctx node _]
  (let [[n step-or-coll coll :as args] (args-of node)
        types (mapv #(infer ctx %) args)
        arity (count args)]
    (cond
      (not (#{1 2 3} arity))
      (do (report! ctx node "TSP2554" "partition-all expects 1, 2 or 3 arguments") t/unknown)

      (some #{t/never} types) t/never

      :else
      (let [env (:env ctx)
            value (fn [ty]
                    (let [v (:val (t/resolve-t env ty))]
                      (when (integer? v) v)))]
        (assignable! ctx n (first types) (t/prim :number))
        (when coll
          (assignable! ctx step-or-coll (second types) (t/prim :number)))
        (if (= 1 arity)
          t/unknown
          (let [source (eff ctx (peek types))
                size (value (first types))
                step (if coll (value (second types)) size)]
            (if-let [invalid (unsupported-sequence-member ctx source)]
              (do (report! ctx (last args) "TSP2345" (str "partition-all cannot sequence " (t/show invalid)))
                  t/unknown)
              (partition-all-type ctx source size step))))))))

(defn- interposed-length [size]
  (when (some? size)
    (if (zero? size) 0 (dec' (*' 2 size)))))

(defn- interposed-element [element separator upper]
  (cond
    (or (= 0 upper) (= t/never separator)) t/never
    (= 1 upper) element
    :else (t/union element separator)))

(defmethod special 'interpose [ctx node _]
  (let [[_ coll :as args] (args-of node)
        [separator-type source :as types] (mapv #(eff ctx (infer ctx %)) args)]
    (cond
      (not (#{1 2} (count args)))
      (do (report! ctx node "TSP2554" "interpose expects 1 or 2 arguments") t/unknown)

      (some #{t/never} types) t/never
      (nil? coll) t/unknown

      :else
      (if-let [invalid (unsupported-sequence-member ctx source)]
        (do (report! ctx coll "TSP2345" (str "interpose cannot sequence " (t/show invalid))) t/unknown)
        (t/with-assumption
         (apply t/union
                (for [member (t/members (:env ctx) source)
                      :let [element (elem-type ctx member)
                            [lo hi] (if (= t/never element) [0 0] (length/bounds member))
                            output (interposed-element element separator-type hi)]]
                  (length/with-bounds (coll-of :seq output) (interposed-length lo) (interposed-length hi))))
         (t/assumption source))))))

(defn- last-position-type [ctx member unbounded-result]
  (let [[lo hi] (sequence-member-bounds member)
        element (elem-type ctx member)]
    (cond
      (or (= 0 hi) (= t/never element)) t/nil-t
      (:items member) (peek (:items member))
      (pos? lo) element
      (nil? hi) (unbounded-result element)
      :else (t/union element t/nil-t))))

(defmethod special 'peek [ctx node _]
  (let [args (args-of node)
        [source :as types] (mapv #(eff ctx (infer ctx %)) args)]
    (if (not= 1 (count types))
      (do (report! ctx node "TSP2554" "peek expects 1 argument") t/unknown)
      (t/with-assumption
       (apply t/union
              (for [member (t/members (:env ctx) source)]
                (cond
                  (#{:nil :never} (:t member)) member
                  (vector-member? member) (last-position-type ctx member #(t/union % t/nil-t))
                  :else (indexed (elem-type ctx member)))))
       (t/assumption source)))))

(defmethod special 'last [ctx node _]
  (let [args (args-of node)
        [source :as types] (mapv #(eff ctx (infer ctx %)) args)]
    (cond
      (not= 1 (count types))
      (do (report! ctx node "TSP2554" "last expects 1 argument") t/unknown)

      (report-invalid-sequence-args! ctx 'last args types) t/unknown

      :else
      (t/with-assumption
       (apply t/union
              (for [member (t/members (:env ctx) source)]
                (if (#{:nil :never} (:t member))
                  member
                  (last-position-type ctx member indexed))))
       (t/assumption source)))))

(defn- trim-tail-type [ctx kind member n]
  (let [element (elem-type ctx member)
        [lo hi] (if (= t/never element) [0 0] (sequence-member-bounds member))
        minimum (if n (max 0 (-' lo n)) 0)
        maximum (if (and n hi) (max 0 (-' hi n)) hi)
        items (when (and n (:items member)) (subvec (:items member) 0 (int maximum)))
        output (cond (= 0 maximum) t/never
                     items (apply t/union items)
                     :else element)]
    (cond-> (length/with-bounds (coll-of kind output) minimum maximum)
      items (assoc :items items))))

(defmethod special 'butlast [ctx node _]
  (let [args (args-of node)
        [source :as types] (mapv #(eff ctx (infer ctx %)) args)]
    (cond
      (not= 1 (count types))
      (do (report! ctx node "TSP2554" "butlast expects 1 argument") t/unknown)

      (report-invalid-sequence-args! ctx 'butlast args types) t/unknown

      :else
      (t/with-assumption
       (apply t/union
              (for [member (t/members (:env ctx) source)]
                (if (= :never (:t member))
                  t/never
                  (let [prefix (trim-tail-type ctx :seq member 1)
                        [lo hi] (length/bounds prefix)]
                    (cond
                      (= 0 hi) t/nil-t
                      (pos? lo) prefix
                      :else (t/union (length/with-bounds prefix 1 hi) t/nil-t))))))
       (t/assumption source)))))

(defmethod special 'pop [ctx node _]
  (let [args (args-of node)
        [source :as types] (mapv #(eff ctx (infer ctx %)) args)]
    (if (not= 1 (count types))
      (do (report! ctx node "TSP2554" "pop expects 1 argument") t/unknown)
      (let [members (t/members (:env ctx) source)
            empty-vector? #(and (vector-member? %) (= 0 (second (length/bounds %))))]
        (when (some empty-vector? members)
          (report! ctx (first args) "TSP330-02"
                   (if (every? empty-vector? members) "pop cannot remove from an empty vector"
                       "pop may receive an empty vector")))
        (t/with-assumption
         (apply t/union
                (for [member members]
                  (cond
                    (#{:nil :never} (:t member)) member
                    (empty-vector? member) t/never
                    (vector-member? member) (trim-tail-type ctx :vector member 1)
                    :else t/unknown)))
         (t/assumption source))))))

(defmethod special 'drop-last [ctx node _]
  (let [[n-node coll :as args] (args-of node)
        types (mapv #(eff ctx (infer ctx %)) args)]
    (cond
      (not (#{1 2} (count args)))
      (do (report! ctx node "TSP2554" "drop-last expects 1 or 2 arguments") t/unknown)

      (some #{t/never} types) t/never

      :else
      (let [source (peek types)
            amount (if coll (:val (t/resolve-t (:env ctx) (first types))) 1)
            n (when (integer? amount) (max 0 amount))]
        (when coll
          (assignable! ctx n-node (first types) (t/prim :number)))
        (if-let [invalid (unsupported-sequence-member ctx source)]
          (do (report! ctx (peek args) "TSP2345" (str "drop-last cannot sequence " (t/show invalid))) t/unknown)
          (t/with-assumption
           (apply t/union
                  (map #(trim-tail-type ctx :seq % n) (t/members (:env ctx) source)))
           (t/assumption source)))))))

(defn- sequence-summary [ctx source]
  (let [parts (for [member (t/members (:env ctx) source)
                    :let [element (elem-type ctx member)
                          [lo hi] (if (= t/never element) [0 0] (sequence-member-bounds member))]]
                {:lower lo :upper hi
                 :element (if (= 0 hi) t/never element)})]
    {:lower (apply min (map :lower parts))
     :upper (when (every? :upper parts) (apply max (map :upper parts)))
     :element (t/with-assumption (apply t/union (map :element parts)) (t/assumption source))}))

(defmethod special 'interleave [ctx node _]
  (let [args (args-of node)
        sources (mapv #(eff ctx (infer ctx %)) args)]
    (cond
      (empty? sources) (length/with-bounds (coll-of :seq t/never) 0 0)
      (some #{t/never} sources) t/never
      (report-invalid-sequence-args! ctx 'interleave args sources) t/unknown

      :else
      (let [normalized (mapv #(apply t/union (map (fn [m] (or (map-entry-sequence m) m))
                                                (t/members (:env ctx) %)))
                             sources)
            parts (mapv #(sequence-summary ctx %) normalized)
            width (count sources)
            lo (*' width (apply min (map :lower parts)))
            hi (when-let [limits (seq (keep :upper parts))] (*' width (apply min limits)))
            positions (mapv :items normalized)
            items (when (every? some? positions) (vec (apply interleave positions)))
            element (cond
                      (= 0 hi) t/never
                      items (apply t/union items)
                      :else (apply t/union (map :element parts)))]
        (t/with-assumption
         (cond-> (length/with-bounds (coll-of :seq element) lo hi)
           items (assoc :items items))
         (some t/assumption sources))))))

(defmethod special 'partition-by [ctx node _]
  (let [[f coll :as args] (args-of node)]
    (case (count args)
      1 (do (infer ctx f) t/unknown)
      2 (let [source (eff ctx (infer ctx coll))]
          (cond
            (= t/never source) (do (infer ctx f) t/never)
            (report-invalid-sequence-args! ctx 'partition-by [coll] [source])
            (do (infer ctx f) t/unknown)

            :else
            (let [{:keys [lower upper element]} (sequence-summary ctx source)
                  group (if (= 0 upper)
                          (do (infer ctx f) t/never)
                          (do (apply-fn-node ctx f [element])
                              (length/with-bounds (coll-of :seq element) 1 upper)))]
              (t/with-assumption
               (length/with-bounds (coll-of :seq group) (min 1 lower) upper)
               (t/assumption source)))))
      (do (run! #(infer ctx %) args)
          (report! ctx node "TSP2554" "partition-by expects 1 or 2 arguments")
          t/unknown))))

(defmethod special 'doall [ctx node _]
  (let [args (args-of node)
        types (mapv #(eff ctx (infer ctx %)) args)
        source (peek types)]
    (cond
      (not (#{1 2} (count args)))
      (do (report! ctx node "TSP2554" "doall expects one or two arguments") t/unknown)

      (some #{t/never} types) t/never

      (report-invalid-sequence-args! ctx 'doall [(peek args)] [source]) t/unknown

      :else
      (do (when (and (= 2 (count args)) (not= 0 (:upper (sequence-summary ctx source))))
            (assignable! ctx (first args) (first types) (t/prim :number)))
          source))))

(defn- concatenated-sequences [ctx sources]
  (let [parts (mapv #(sequence-summary ctx %) sources)]
    (t/with-assumption
     (length/with-bounds (coll-of :seq (apply t/union (map :element parts)))
                         (reduce +' 0 (map :lower parts))
                         (when (every? :upper parts) (reduce +' 0 (map :upper parts))))
     (some t/assumption sources))))

(defmethod special 'concat [ctx node _]
  (let [args (args-of node)
        sources (mapv #(eff ctx (infer ctx %)) args)]
    (cond
      (some #{t/never} sources) t/never
      (report-invalid-sequence-args! ctx 'concat args sources) t/unknown
      :else (concatenated-sequences ctx sources))))

(defn- flatten-kind [member]
  (case (:t member)
    :coll (case (:kind member)
            :vector :sequence
            :set :leaf
            (if-let [cls (some-> (:jvm-class member) t/load-class)]
              (if (.isAssignableFrom clojure.lang.Sequential cls) :sequence :maybe-sequence)
              :maybe-sequence))
    :prim (case (:name member)
            :vector :sequence
            (:seq :coll) :maybe-sequence
            :leaf)
    :class (if-let [^Class cls (t/load-class (:name member))]
             (cond
               (.isAssignableFrom clojure.lang.Sequential cls) :sequence
               (or (.isArray cls) (Modifier/isFinal (.getModifiers cls))) :leaf
               :else :unknown)
             :unknown)
    (:nil :lit :map :map-of :atom) :leaf
    :unknown))

(defn- flattened-sequence [ctx source]
  (let [env (:env ctx)
        budget (volatile! 256)
        empty-result (length/with-bounds (coll-of :seq t/never) 0 0)
        unknown-result (coll-of :seq t/unknown)]
    (letfn [(children [member depth]
              (if-let [items (:items member)]
                (concatenated-sequences ctx (mapv #(walk % false depth) items))
                (let [[lo hi] (length/bounds member)]
                  (if (= 0 hi)
                    empty-result
                    (let [{:keys [element lower upper]} (sequence-summary ctx (walk (or (:of member) t/unknown) false depth))
                          maximum (cond
                                    (= 0 upper) 0
                                    (and hi upper) (*' hi upper))]
                      (length/with-bounds (coll-of :seq element) (*' lo lower) maximum))))))
            (walk [ty root? depth]
              (let [members (vec (take (inc @budget) (t/members env (eff ctx ty))))]
                (if (or (> depth 32) (neg? (vswap! budget - (count members))))
                  unknown-result
                  (t/with-assumption
                   (apply t/union
                          (for [member members
                                :let [leaf (if root? empty-result
                                               (length/with-bounds (coll-of :seq member) 1 1))]]
                            (case (flatten-kind member)
                              :sequence (children member (inc depth))
                              :maybe-sequence (if root?
                                                (t/union empty-result (children member (inc depth)))
                                                unknown-result)
                              :leaf leaf
                              unknown-result)))
                   (t/assumption ty)))))]
      (walk source true 0))))

(defmethod special 'flatten [ctx node _]
  (let [args (args-of node)
        [source :as types] (mapv #(eff ctx (infer ctx %)) args)]
    (cond
      (not= 1 (count types))
      (do (report! ctx node "TSP2554" "flatten expects one argument") t/unknown)

      (= t/never source) t/never

      :else (flattened-sequence ctx source))))

(def ^:private comp-identity
  (t/parse-type {:fail #(throw (ex-info "Invalid comp identity signature" {:form %}))}
                '(t/all [a] [a :-> a])))

(defmethod special 'comp [ctx node _]
  (let [stages (args-of node)]
    (if (seq stages)
      (or (compose-functions ctx stages) t/unknown)
      comp-identity)))

(defmethod special 'partial [ctx node _]
  (let [[f & args] (args-of node)]
    (cond
      (nil? f) (do (report! ctx node "TSP2554" "partial expects at least one argument") t/unknown)
      (empty? args) (infer ctx f)
      :else (or (partial-function ctx node f args) t/unknown))))

(doseq [h '[keep mapcat]]
  (defmethod special h [ctx node _]
    (let [[f & colls] (args-of node)
          els (mapv #(elem-type ctx (infer ctx %)) colls)]
      (if (empty? colls)
        (do (infer ctx f) t/unknown)
        (let [ret (apply-fn-node ctx f els)
              ret (case h keep (t/remove-nil (:env ctx) ret) mapcat (elem-type ctx ret) ret)]
          (coll-of :seq ret))))))

(doseq [[head kind] {'map :seq 'mapv :vector 'pmap :seq}]
  (defmethod special head [ctx node _]
    (let [[f & colls] (args-of node)
          sources (mapv #(eff ctx (infer ctx %)) colls)]
      (cond
        (and f (= 'map head) (empty? colls))
        (do (infer ctx f) t/unknown)

        (empty? colls)
        (do (when f (infer ctx f))
            (report! ctx node "TSP2554" (if (= 'map head)
                                         "map expects at least one argument"
                                         (str (name head) " expects a function and at least one collection")))
            t/unknown)

        (some #{t/never} sources)
        (do (infer ctx f) t/never)

        (report-invalid-sequence-args! ctx head colls sources)
        (do (infer ctx f) t/unknown)

        :else
        (let [parts (mapv #(sequence-summary ctx %) sources)
              minimum (apply min (map :lower parts))
              known-maxima (seq (keep :upper parts))
              maximum (when known-maxima (apply min known-maxima))
              element (if (= 0 maximum)
                        (do (infer ctx f) t/never)
                        (apply-fn-node ctx f (mapv :element parts)))]
          (t/with-assumption
           (length/with-bounds (coll-of kind element) minimum maximum)
           (some t/assumption sources)))))))

(defn- apply-indexed-callback [ctx callback element]
  (case (:op callback)
    ;; A keyword lookup on the integer index returns its default argument.
    :kw (do (infer ctx callback) element)
    (:set :vector) (do (infer ctx callback)
                        (report! ctx callback "TSP2554" (str (parse/unparse callback) " is called with 2 argument(s)"))
                        t/unknown)
    (apply-fn-node ctx callback [(t/prim :int) element])))

(defmethod special 'map-indexed [ctx node _]
  (let [[f coll :as args] (args-of node)]
    (case (count args)
      1 (do (infer ctx f) t/unknown)
      2 (let [source (eff ctx (infer ctx coll))
              invalid (unsupported-sequence-member ctx source)]
          (cond
            (= t/never source) (do (infer ctx f) t/never)
            invalid (do (infer ctx f)
                        (report! ctx coll "TSP2345" (str "map-indexed cannot sequence " (t/show invalid)))
                        t/unknown)
            :else (let [{:keys [lower upper element]} (sequence-summary ctx source)
                        result (if (= 0 upper)
                                 (do (infer ctx f) t/never)
                                 (apply-indexed-callback ctx f element))]
                    (t/with-assumption
                     (length/with-bounds (coll-of :seq result) lower upper)
                     (t/assumption source)))))
      (do (run! #(infer ctx %) args)
          (report! ctx node "TSP2554" "map-indexed expects 1 or 2 arguments")
          t/unknown))))

(defn- keep-result-type [ctx {:keys [lower upper]} result items]
  (let [env (:env ctx)
        element (t/remove-nil env result)
        nonnil? #(= :proved (:status (t/sub-result env % t/some-t)))
        selected (when (and items (every? #(or (= t/nil-t %) (nonnil? %)) items))
                   (into [] (remove #{t/nil-t}) items))
        lo (cond selected (count selected) (= t/never element) 0 (nonnil? result) lower :else 0)
        hi (cond selected (count selected) (= t/never element) 0 :else upper)]
    (cond-> (length/with-bounds (coll-of :seq element) lo hi)
      selected (assoc :items selected))))

(defn- check-keep [ctx node head indexed?]
  (let [[f coll :as args] (args-of node)]
    (case (count args)
      1 (do (infer ctx f) t/unknown)
      2 (let [source (eff ctx (infer ctx coll))]
          (cond
            (= t/never source) (do (infer ctx f) t/never)
            (report-invalid-sequence-args! ctx head [coll] [source]) (do (infer ctx f) t/unknown)

            :else
            (let [normalized (apply t/union (map #(or (map-entry-sequence %) %) (t/members (:env ctx) source)))
                  {:keys [element upper] :as shape} (sequence-summary ctx normalized)
                  identity? (and (not indexed?) (= 'clojure.core/identity (guard-symbol ctx f))
                                 (empty? (:directives f)) (not (env/tag-node f)))
                  result (cond
                           (= 0 upper) (infer ctx f)
                           identity? (do (infer ctx f) element)
                           indexed? (apply-indexed-callback ctx f element)
                           :else (apply-fn-node ctx f [element]))]
              (if (and (= 0 upper) (= t/never result))
                t/never
                (t/with-assumption (keep-result-type ctx shape (if (= 0 upper) t/nil-t (eff ctx result))
                                                     (when identity? (:items normalized)))
                                   (or (t/assumption source) (t/assumption result)))))))
      (do (run! #(infer ctx %) args)
          (report! ctx node "TSP2554" (str head " expects 1 or 2 arguments"))
          t/unknown))))

(defmethod special 'keep [ctx node _]
  (check-keep ctx node 'keep false))

(defmethod special 'keep-indexed [ctx node _]
  (check-keep ctx node 'keep-indexed true))

(defn- filter-splitter [ctx f]
  (let [env (:env ctx)]
    (if-let [summary (get-in env [:guards (guard-symbol ctx f)])]
      (when (empty? (:directives f)) #(guards/apply-summary env summary %))
      (let [descriptor (deferred-callback ctx f)
            node (or (:node descriptor) f)
            captured (if descriptor (callback-scope descriptor) ctx)
            plan (cond
                   (seq (:directives node)) nil
                   (= :sym (:op node))
                   (let [parameter (gensym "guard-value")]
                     (guards/compile-body (guard-compiler ctx) [parameter]
                                          [{:op :list :children [node {:op :sym :val parameter}]}]))
                   (and (inline-callback? captured node)
                        (or (= :fn-lit (:op node)) (not= :sym (:op (second (:children node))))))
                   (let [[branch :as bodies] (fn-bodies node)]
                     (when (and (= 1 (count bodies) (count (:fixed branch))) (not (:rest branch))
                                (= :sym (:op (first (:fixed branch)))))
                       (let [parameter (:val (first (:fixed branch)))]
                         (guards/compile-body (guard-compiler captured)
                                              (cond-> [parameter] (:reader? branch) (conj '%))
                                              (:body branch))))))]
        (when (guards/useful? plan) #(guards/split env plan %))))))

(defn- apply-predicate [ctx f el narrow?]
  (let [before (when *diags* (count @*diags*))
        ret (apply-fn-node ctx f [el])
        split (when (and narrow? (not (:guards-disabled? ctx))
                         (or (nil? before) (= before (count @*diags*))))
                (filter-splitter ctx f))
        [yes no] (if split (split el) [el el])]
    [ret yes no split]))

(doseq [[head kind] {'filter :seq 'remove :seq 'filterv :vector}]
  (defmethod special head [ctx node _]
    (let [[f coll :as args] (args-of node)
          arity (count args)]
      (cond
        (and (= arity 1) (= kind :seq))
        (do (infer ctx f) t/unknown)

        (not= arity 2)
        (do (run! #(infer ctx %) args)
            (report! ctx node "TSP2554" (str head (if (= kind :seq) " expects 1 or 2 arguments" " expects 2 arguments")))
            t/unknown)

        :else
        (let [source (eff ctx (infer ctx coll))]
          (if (= t/never source)
            (do (infer ctx f) t/never)
            (if-let [invalid (unsupported-sequence-member ctx source)]
              (do (infer ctx f)
                  (report! ctx coll "TSP2345" (str head " cannot sequence " (t/show invalid)))
                  t/unknown)
              (let [{:keys [element upper]} (sequence-summary ctx source)
                    selected (if (= 0 upper)
                               (do (infer ctx f) t/never)
                               (let [[_ yes no] (apply-predicate ctx f element true)]
                                 (if (= 'remove head) no yes)))
                    maximum (if (= t/never selected) 0 upper)]
                (t/with-assumption
                 (length/with-bounds {:t :coll :kind kind :of (dissoc selected :fresh)} 0 maximum)
                 (t/assumption source))))))))))

(defn- sequence-with-kind [ctx source kind reorder-items]
  (t/with-assumption
   (apply t/union
          (for [member (t/members (:env ctx) source)
                :let [element (elem-type ctx member)
                      [lo hi] (if (= t/never element) [0 0] (sequence-member-bounds member))
                      result (length/with-bounds (coll-of kind (if (= 0 hi) t/never element)) lo hi)]]
            (if-let [items (and reorder-items (:items member))]
              (assoc result :items (reorder-items items))
              result)))
   (t/assumption source)))

(defmethod special 'shuffle [ctx node _]
  (let [[arg :as args] (args-of node)
        [source :as types] (mapv #(eff ctx (infer ctx %)) args)]
    (cond
      (not= 1 (count types))
      (do (report! ctx node "TSP2554" "shuffle expects one argument") t/unknown)

      (= t/never source) t/never

      :else
      (let [env (:env ctx)
            expected (t/class-type "java.util.Collection")
            bad (some (fn [member]
                        (when (and (not (and (= :prim (:t member)) (= :coll (:name member))))
                                   (not (and (= :coll (:t member)) (nil? (:kind member))))
                                   (= :mismatch (:status (t/sub-result env member expected))))
                          member))
                      (t/members env source))]
        (if bad
          (do (report! ctx arg "TSP2345" (str "shuffle needs a java.util.Collection, got " (t/show bad)))
              t/unknown)
          (sequence-with-kind ctx source :vector nil))))))

(defmethod special 'vec [ctx node _]
  (let [args (args-of node)
        [source :as types] (mapv #(eff ctx (infer ctx %)) args)]
    (cond
      (not= 1 (count types))
      (do (report! ctx node "TSP2554" "vec expects 1 argument") t/unknown)

      (= t/never source) t/never

      (report-invalid-sequence-args! ctx 'vec args types) t/unknown

      :else (sequence-with-kind ctx source :vector identity))))

(defmethod special 'reverse [ctx node _]
  (let [args (args-of node)
        [source :as types] (mapv #(eff ctx (infer ctx %)) args)]
    (cond
      (not= 1 (count types))
      (do (report! ctx node "TSP2554" "reverse expects 1 argument") t/unknown)

      (= t/never source) t/never

      :else
      (if-let [invalid (unsupported-sequence-member ctx source)]
        (do (report! ctx (first args) "TSP2345" (str "reverse cannot sequence " (t/show invalid))) t/unknown)
        (sequence-with-kind ctx source :seq (comp vec rseq))))))

(doseq [[head minimum] {'sort 1 'sort-by 2}]
  (defmethod special head [ctx node _]
    (let [args (args-of node)
          arity (count args)]
      (if-not (<= minimum arity (inc minimum))
        (do (run! #(infer ctx %) args)
            (report! ctx node "TSP2554" (str head " expects " minimum " or " (inc minimum) " arguments"))
            t/unknown)
        (let [key-fn (when (= 'sort-by head) (first args))
              comparator (when (= arity (inc minimum)) (infer ctx (nth args (dec minimum))))
              source-node (peek args)
              source (eff ctx (infer ctx source-node))
              invalid (unsupported-sequence-member ctx source)]
          (cond
            (or (= t/never source) (= t/never comparator))
            (do (when key-fn (infer ctx key-fn)) t/never)

            invalid
            (do (when key-fn (infer ctx key-fn))
                (report! ctx source-node "TSP2345" (str head " cannot sequence " (t/show invalid)))
                t/unknown)

            :else
            (do (when key-fn (apply-fn-node ctx key-fn [(elem-type ctx source)]))
                (sequence-with-kind ctx source :seq nil))))))))

(defn- leading-run-bounds [member {:keys [can-pass? can-fail? split]}]
  (let [[lo hi] (sequence-member-bounds member)]
    (cond
      (not can-pass?) [0 0]
      (not can-fail?) [lo hi]
      (and split (:items member))
      (let [splits (map split (:items member))]
        [(count (take-while #(= t/never (second %)) splits))
         (count (take-while #(not= t/never (first %)) splits))])
      :else [0 hi])))

(defn- check-while-sequence [ctx node head transducer? transform]
  (let [[f source :as args] (args-of node)]
    (cond
      (and transducer? (= 1 (count args))) (do (infer ctx f) t/unknown)
      (= 2 (count args))
      (let [ty (eff ctx (infer ctx source))]
        (cond
          (= t/never ty) (do (infer ctx f) t/never)
          (report-invalid-sequence-args! ctx head [source] [ty]) (do (infer ctx f) t/unknown)
          :else
          (let [members (map #(or (map-entry-sequence %) %) (t/members (:env ctx) ty))
                normalized (apply t/union members)
                {:keys [element upper]} (sequence-summary ctx normalized)]
            (if (= 0 upper)
              (if (= t/never (infer ctx f)) t/never
                  (t/with-assumption (transform normalized {:can-pass? false :can-fail? false})
                                     (t/assumption ty)))
              (let [[ret yes no split] (apply-predicate ctx f element true)
                    ret (eff ctx ret)
                    can-pass? (and (not= t/never yes) (not= t/never (truth-part (:env ctx) ret true)))
                    can-fail? (and (not= t/never no) (not= t/never (truth-part (:env ctx) ret false)))
                    predicate {:can-pass? can-pass? :can-fail? can-fail? :split split :yes yes}]
                (t/with-assumption (apply t/union (map #(transform % predicate) members))
                                   (or (t/assumption ty) (t/assumption ret))))))))
      :else
      (do (run! #(infer ctx %) args)
          (report! ctx node "TSP2554" (str head (if transducer? " expects 1 or 2 arguments" " expects two arguments")))
          t/unknown))))

(defn- take-while-type [member {:keys [yes] :as predicate}]
  (if (= t/never member)
    t/never
    (let [[lo hi] (leading-run-bounds member predicate)
          items (when (and (:items member) (= lo hi) (pos? hi)) (subvec (:items member) 0 (int lo)))
          element (cond (= 0 hi) t/never
                        items (apply t/union items)
                        :else yes)]
      (cond-> (length/with-bounds (coll-of :seq element) lo hi)
        items (assoc :items items)))))

(defn- drop-while-type [ctx member {:keys [can-fail?] :as predicate}]
  (cond
    (= t/never member) t/never
    (not can-fail?) (length/with-bounds (coll-of :seq t/never) 0 0)
    :else
    (let [[skip-min skip-max] (leading-run-bounds member predicate)]
      (if (= skip-min skip-max)
        (sequence-tail ctx member skip-min false)
        (let [[lo hi] (sequence-member-bounds member)
              lower (if skip-max (max 0 (-' lo skip-max)) 0)
              upper (when hi (max 0 (-' hi skip-min)))]
          (length/with-bounds (coll-of :seq (if (= 0 upper) t/never (member-element member)))
                              lower upper))))))

(defmethod special 'take-while [ctx node _]
  (check-while-sequence ctx node 'take-while true take-while-type))

(defmethod special 'drop-while [ctx node _]
  (check-while-sequence ctx node 'drop-while true (partial drop-while-type ctx)))

(defmethod special 'split-with [ctx node _]
  (check-while-sequence
   ctx node 'split-with false
   (fn [member predicate]
     (t/tuple [(take-while-type member predicate)
               (drop-while-type ctx member predicate)]))))

(defn- check-collection-callback [ctx node head narrow? finish]
  (let [[f source :as args] (args-of node)]
    (if (not= 2 (count args))
      (do (run! #(infer ctx %) args)
          (report! ctx node "TSP2554" (str head " expects two arguments"))
          t/unknown)
      (let [ty (eff ctx (infer ctx source))]
        (cond
          (= t/never ty) (do (infer ctx f) t/never)
          (report-invalid-sequence-args! ctx head [source] [ty]) (do (infer ctx f) t/unknown)
          :else
          (let [normalized (apply t/union
                                  (map #(or (map-entry-sequence %) %) (t/members (:env ctx) ty)))
                {:keys [upper element] :as shape} (sequence-summary ctx normalized)]
            (if (= 0 upper)
              (if (= t/never (infer ctx f)) t/never
                  (t/with-assumption (finish (assoc shape :ret t/never)) (t/assumption ty)))
              (let [[ret yes no] (apply-predicate ctx f element narrow?)
                    result (finish (assoc shape :ret (eff ctx ret) :yes yes :no no))]
                (t/with-assumption result (or (t/assumption ty) (t/assumption ret)))))))))))

(defn- some-result [env {:keys [lower ret yes no]}]
  (let [value (if (= t/never yes) t/never (truth-part env ret true))]
    (if (and (pos? lower)
             (or (= t/never no) (= t/never (truth-part env ret false))))
      value
      (t/union value t/nil-t))))

(defmethod special 'some [ctx node _]
  (check-collection-callback ctx node 'some true (partial some-result (:env ctx))))

(defn- every-result [env {:keys [lower ret yes no]}]
  (cond
    (= t/never ret) (if (pos? lower) t/never (t/lit true))
    (or (= t/never no) (= t/never (truth-part env ret false))) (t/lit true)
    (and (pos? lower) (or (= t/never yes) (= t/never (truth-part env ret true)))) (t/lit false)
    :else (t/prim :boolean)))

(defmethod special 'every? [ctx node _]
  (check-collection-callback ctx node 'every? true (partial every-result (:env ctx))))

(defmethod special 'not-every? [ctx node _]
  (check-collection-callback
   ctx node 'not-every? true
   (fn [shape]
     (let [result (every-result (:env ctx) shape)]
       (if (= :lit (:t result)) (t/lit (not (:val result))) result)))))

(defmethod special 'not-any? [ctx node _]
  (let [env (:env ctx)]
    (check-collection-callback
     ctx node 'not-any? true
     (fn [shape]
       (let [found (some-result env shape)]
         (cond
           (= t/never found) t/never
           (= t/nil-t found) (t/lit true)
           (or (#{:unknown :any} (:t found)) (t/nilable? env found)) (t/prim :boolean)
           :else (t/lit false)))))))

(defmethod special 'run! [ctx node _]
  (check-collection-callback
   ctx node 'run! false
   (fn [{:keys [lower ret]}]
     (if (and (pos? lower) (= t/never ret)) t/never t/nil-t))))

(defmethod special 'group-by [ctx node _]
  (check-collection-callback
   ctx node 'group-by false
   (fn [{:keys [element lower upper ret]}]
     (if (= t/never ret)
       (if (pos? lower) t/never empty-map-shape)
       {:t :map-of :k (widen ret)
        :v (length/with-bounds (coll-of :vector element) 1 upper)}))))

(defn- check-key-extremum [ctx node head]
  (let [[f & values] (args-of node)
        candidates (mapv #(infer ctx %) values)]
    (cond
      (empty? candidates)
      (do (when f (infer ctx f))
          (report! ctx node "TSP2554" (str head " expects a key function and at least one value"))
          t/unknown)

      (some #{t/never} candidates) (do (infer ctx f) t/never)

      (= 1 (count candidates))
      (if (= t/never (infer ctx f)) t/never (first candidates))

      :else
      (let [value (apply t/union candidates)
            key-type (apply-fn-node ctx f [value])]
        (when (= :mismatch (:status (t/sub-result (:env ctx) (eff ctx key-type) (t/prim :number))))
          (assignable! ctx f key-type (t/prim :number)))
        (if (= t/never key-type) t/never value)))))

(defmethod special 'max-key [ctx node _]
  (check-key-extremum ctx node 'max-key))

(defmethod special 'min-key [ctx node _]
  (check-key-extremum ctx node 'min-key))

(defn- literal-collection-count [node]
  (cond
    (#{:vector :set} (:op node)) (count (:children node))
    (and (= :quote (:op node)) (= :list (:op (first (:children node)))))
    (count (:children (first (:children node))))))

(defn- reduced-type? [ty]
  (and (= :class (:t ty)) (= "clojure.lang.Reduced" (:name ty))))

(defn- reduced-payload [ty]
  (get ty :reduced-value t/unknown))

(defmethod special 'reduced [ctx node _]
  (let [types (mapv #(infer ctx %) (args-of node))]
    (cond
      (not= 1 (count types))
      (do (report! ctx node "TSP2554" "reduced expects one argument") t/unknown)

      (= t/never (first types)) t/never

      :else (assoc (t/class-type "clojure.lang.Reduced") :reduced-value (dissoc (first types) :fresh)))))

(defn- reducer-var [ctx f]
  (when (and (empty? (:directives f)) (not (env/tag-node f)))
    (guard-symbol ctx f)))

(def ^:private collection-reducer-type
  {:t :fn :arities [{:params [] :rest t/unknown :ret t/unknown}]})

(defn- reduction-function [ctx f calls]
  (let [expected {:t :fn :arities (mapv #(hash-map :params % :ret t/unknown) calls)}
        ty (if (callback-node? ctx f) (infer ctx f expected) (infer ctx f))
        reducer (when (= :unknown (:t ty))
                  (get '{clojure.core/conj conj clojure.core/into into} (reducer-var ctx f)))]
    (cond
      (= :ref (:t ty)) (t/resolve-t (:env ctx) ty)
      reducer (with-meta collection-reducer-type {::collection-reducer reducer})
      :else ty)))

(defn- map-conj-domain [ty]
  (case (:t ty)
    :map-of (select-keys ty [:t :k :v])
    :map (or (map-index-domain ty)
             (if (:complete-keys? ty)
               (let [entries (remove (comp #{t/never} val) (concat (:req ty) (:opt ty)))]
                 {:t :map-of :k (apply t/union (map (comp t/lit key) entries))
                  :v (apply t/union (map val entries))})
               {:t :map-of :k t/unknown :v t/unknown}))
    :prim (when (= :map (:name ty)) {:t :map-of :k t/unknown :v t/unknown})
    nil))

(defn- conj-map-input [ctx f ty]
  (let [[lo hi] (length/bounds ty)
        vector? (vector-member? ty)]
    (or (map-conj-domain ty)
        (cond
          (or (= :nil (:t ty)) (= (t/lit "") ty))
          {:t :map-of :k t/never :v t/never}
          (and vector? (= lo hi 2))
          (let [[k v] (or (:items ty) (repeat 2 (:of ty)))] {:t :map-of :k k :v v})
          (or (= :lit (:t ty)) (contains? scalar-primitives (:name ty))
              (and vector? (or (> lo 2) (and hi (< hi 2)))))
          (do (report! ctx f "TSP2345" (str "conj cannot add " (t/show ty) " to a map")) t/never)
          :else nil))))

(defn- conj-map-step [ctx f target items]
  (let [branches (mapv #(mapv (partial conj-map-input ctx f) (t/members (:env ctx) %)) items)
        domains (cons target (remove #{t/never} (mapcat identity branches)))]
    (cond
      (some #(every? #{t/never} %) branches) t/never
      (some nil? domains) t/unknown
      :else {:t :map-of :k (apply t/union (map :k domains)) :v (apply t/union (map :v domains))})))

(defn- conj-reducer-member [ctx f target items]
  (let [kind (or (:kind target) (:name target))
        domain (map-conj-domain target)]
    (cond
      (= t/never target) t/never
      (vector-member? target) (append-to-vectors ctx target [target] (t/tuple items))
      (= :nil (:t target)) (length/with-bounds (coll-of :seq (apply t/union items)) 1 nil)
      (#{:set :seq} kind)
      (length/with-bounds (coll-of kind (apply t/union (elem-type ctx target) items)) 1 nil)
      domain (conj-map-step ctx f domain items)
      (or (= :lit (:t target)) (contains? scalar-primitives (:name target)))
      (do (report! ctx f "TSP2345" (str "conj cannot add elements to " (t/show target))) t/never)
      :else t/unknown)))

(defn- conj-reducer-step [ctx f [target & items]]
  (cond
    (nil? target) (t/tuple [])
    (empty? items) target
    :else (let [items (mapv #(dissoc % :fresh) items)]
            (t/with-assumption
             (apply t/union (map #(conj-reducer-member ctx f % items)
                                 (t/members (:env ctx) (eff ctx target))))
             (or (t/assumption target) (some t/assumption items))))))

(defn- reduction-start [ctx f seed]
  (if (= 'clojure.core/conj (reducer-var ctx f))
    (t/with-assumption (or (map-conj-domain seed) seed) (t/assumption seed))
    seed))

(defn- into-reducer-step [ctx f args]
  (case (count args)
    0 (coll-of :vector t/never)
    1 (first args)
    2 (let [[target source] (mapv #(eff ctx %) args)
            members (t/members (:env ctx) target)]
        (if (every? vector-member? members)
          (checked-vector-into ctx f target members source)
          t/unknown))
    t/unknown))

(defn- reduction-result [ctx f ty args]
  (case (::collection-reducer (meta ty))
    conj (conj-reducer-step ctx f args)
    into (into-reducer-step ctx f args)
    (if (or (= :all (:t ty)) (seq (:arities ty)))
      (eff ctx (apply-type ctx ty args f (parse/unparse f)))
      t/unknown)))

(defn- reduction-step [ctx f ty args]
  (let [ret (reduction-result ctx f ty args)
        members (t/members (:env ctx) ret)]
    (if (some reduced-type? members)
      (let [{stopped true continuing false} (group-by reduced-type? members)
            assumed (t/assumption ret)]
        [(t/with-assumption (apply t/union continuing) assumed)
         (t/with-assumption (apply t/union (map reduced-payload stopped)) assumed)])
      [ret t/never])))

(defn- join-accumulator [ctx acc step]
  (if (and (= :coll (:t acc) (:t step)) (= (:kind acc) (:kind step)))
    (update (dissoc acc :items :min-count :max-count :entry-map) :of t/union (:of step))
    (let [joined (t/union acc step)]
      (or (widen-vector-growth (:env ctx) joined) joined))))

(defn- array-reduction-invariant [ctx seed infer-step]
  (loop [acc seed, iterations 0]
    (let [step (probe ctx #(infer-step % acc))]
      (when-not (= :unknown (:t step))
        ;; Join even nominally equal classes. A later value can lose the
        ;; exact runtime component of a covariant JVM array accumulator.
        (let [joined (join-accumulator ctx acc (widen (dissoc step :fresh)))]
          (cond
            (= acc joined) acc
            (< iterations 7) (recur joined (inc iterations))))))))

(defn- array-reduction-result [ctx index target init body lengths]
  (let [index-sym (:val index)
        target-sym (:val target)
        index-ctx (-> (bind-pattern ctx index (t/lit 0))
                      (assoc-in [:compiler-local-classes index-sym] "long"))
        initial (infer index-ctx init)
        infer-step (fn [c acc]
                     (let [c (bind-pattern c target (dissoc acc :fresh))]
                       (record! c index (get-in c [:locals index-sym]))
                       (record! c target (get-in c [:locals target-sym]))
                       (infer c body)))]
    (cond
      (or (= t/never initial) (= #{0} lengths)) initial

      (and lengths (every? #{0 1} lengths))
      (let [step (infer-step (dissoc index-ctx :recur-types) initial)]
        (when (= :unknown (:t step))
          (unchecked-hof! ctx body "areduce accumulator is not checked" :accumulator))
        (if (contains? lengths 0) (t/union initial step) step))

      :else
      (let [body-ctx (-> index-ctx
                         (update-in [:locals index-sym] widen)
                         (dissoc :recur-types))
            seed (widen (dissoc initial :fresh))
            stable (array-reduction-invariant body-ctx seed infer-step)
            step (infer-step body-ctx (or stable seed))]
        (when (or (nil? stable) (= :unknown (:t step)))
          (unchecked-hof! ctx body "areduce accumulator is not checked" :accumulator))
        (if stable (widen (t/union seed (dissoc step :fresh))) t/unknown)))))

(defmethod special 'areduce [ctx node _]
  (let [[array index target init body :as args] (args-of node)
        source (when array (infer ctx array))
        [bad-binding message] (array-loop-binding-problem index target)]
    (cond
      (not= 5 (count args))
      (do (report! ctx node "TSP2554"
                   "areduce expects an array, an index name, an accumulator name, an initial value and an expression")
          t/unknown)

      bad-binding
      (do (report! ctx bad-binding "TSP2345" (str "areduce " message))
          t/unknown)

      (= t/never source) t/never

      :else
      (let [members (checked-array-members ctx array 'areduce (eff ctx source))
            receivers (remove #(#{:nil :mismatch} (array-argument-status %)) members)]
        (if (seq receivers)
          (t/with-assumption
           (array-reduction-result ctx index target init body (known-array-lengths receivers))
           (t/assumption source))
          t/never)))))

(defn- reduction-accumulator [ctx f seed inputs empty-call?]
  (loop [acc seed iterations 0]
    (let [args (into [acc] inputs)
          calls (cond-> [] empty-call? (conj []) true (conj args))
          step (probe ctx #(first (reduction-step % f (reduction-function % f calls) args)))]
      (cond
        (t/sub? (:env ctx) step acc) acc
        (= :unknown (:t step)) nil
        (= iterations 7) nil
        :else (recur (join-accumulator ctx acc (widen (dissoc step :fresh))) (inc iterations))))))

(defn- map-entry-sequence [member]
  (let [source (or (:entry-map member) member)
        closed? (and (= :map (:t source)) (:complete-keys? source))]
    (when (or closed? (= :map-of (:t source)))
      (let [entries (when closed? (remove (comp #{t/never} val) (concat (:req source) (:opt source))))
            [lo hi] (if (and closed? (= :map (:t member)))
                      [(count (:req source)) (count entries)]
                      (length/bounds member))
            element (if closed?
                      (apply t/union (map #(found-map-entry (t/lit (key %)) (val %) false) entries))
                      (found-map-entry (:k source) (:v source) false))]
        (length/with-bounds {:t :coll :kind :seq :of element} lo hi)))))

(defn- reduce-over-sequence [ctx f initial collection source]
  (let [members (t/members (:env ctx) source)
        seq-source (t/with-assumption (apply t/union (map #(or (map-entry-sequence %) %) members))
                                      (t/assumption source))
        {:keys [lower upper element]} (sequence-summary ctx seq-source)
        [lower upper] (if-some [n (literal-collection-count collection)] [n n] [lower upper])
        maximum-steps (when upper (- upper (if initial 0 1)))
        may-be-empty? (zero? lower)
        may-be-singleton? (and (<= lower 1) (not= 0 upper))
        step? (or (nil? maximum-steps) (pos? maximum-steps))
        repeat? (or (nil? maximum-steps) (> maximum-steps 1))
        start (reduction-start ctx f (if initial (widen (dissoc initial :fresh)) element))
        empty-call? (and (not initial) may-be-empty?)
        invariant (if repeat? (reduction-accumulator ctx f start [element] empty-call?) start)
        step-args [(or invariant start) element]
        calls (cond-> [] empty-call? (conj []) step? (conj step-args))
        ty (if (seq calls) (reduction-function ctx f calls) (infer ctx f))
        [step stopped] (when step? (reduction-step ctx f ty step-args))
        base (if initial
               (if may-be-empty? initial t/never)
               (t/union (if empty-call? (reduction-result ctx f ty []) t/never)
                        (if may-be-singleton? element t/never)))]
    (when (or (and step? (or (nil? invariant) (= :unknown (:t step)))
                   (or (empty? (:arities ty)) (seq (matching-arities ty 2))))
              (and empty-call? (not (or (= :all (:t ty)) (seq (:arities ty))))))
      (report! ctx f "TSP210-01" "reduce accumulator is not checked"
               :severity (if (:strict (:opts ctx)) :error :warning)
               :reasons #{:accumulator}))
    (t/with-assumption
     (cond
       (not step?) base
       (nil? invariant) t/unknown
       :else (t/union base step stopped))
     (t/assumption source))))

(defmethod special 'reduce [ctx node _]
  (let [[f a b & more :as args] (args-of node)]
    (if (or (< (count args) 2) (seq more))
      (do (run! #(infer ctx %) args)
          (report! ctx node "TSP2554" "reduce expects 2 or 3 arguments")
          t/unknown)
      (let [[init c] (if b [a b] [nil a])
            initial (when init (infer ctx init))
            source (eff ctx (infer ctx c))]
        (cond
          (some #{t/never} [initial source]) (do (infer ctx f) t/never)
          (report-invalid-sequence-args! ctx 'reduce [c] [source]) (do (infer ctx f) t/unknown)
          :else (reduce-over-sequence ctx f initial c source))))))

(defn- kv-reduction-input [member]
  (let [opaque {:bounds [0 nil] :key t/unknown :value t/unknown}]
    (case (:t member)
      (:nil :never) {:bounds [0 0]}
      :map-of {:bounds (length/bounds member) :key (:k member) :value (:v member)}
      :map (if (:complete-keys? member)
             (let [entries (concat (:req member) (:opt member))]
               {:bounds [(count (:req member)) (count entries)]
                :key (apply t/union (map (comp t/lit key) entries))
                :value (apply t/union (map val entries))})
             (let [domain (map-index-domain member)]
               {:bounds [(count (:req member)) nil]
                :key (get domain :k t/unknown)
                :value (get domain :v t/unknown)}))
      :coll (if (= :vector (:kind member))
              {:bounds (length/bounds member) :key (t/prim :int) :value (:of member)}
              opaque)
      :prim (cond-> opaque (= :vector (:name member)) (assoc :key (t/prim :int)))
      opaque)))

(doseq [[head part] {'keys :key 'vals :value}]
  (defmethod special head [ctx node _]
    (let [args (args-of node)
          [source :as types] (mapv #(eff ctx (infer ctx %)) args)]
      (if (not= 1 (count types))
        (do (report! ctx node "TSP2554" (str head " expects 1 argument")) t/unknown)
        (t/with-assumption
         (apply t/union
                (for [member (t/members (:env ctx) source)]
                  (case (:t member)
                    (:nil :never) member
                    (let [{[lo hi] :bounds :as entries} (when (#{:map :map-of} (:t member))
                                                        (kv-reduction-input member))
                          element (if (or (= :map-of (:t member)) (:complete-keys? member) (:map-index member))
                                    (part entries) t/any)
                          minimum (or lo 0)]
                      (if (= 0 hi)
                        t/nil-t
                        (let [items (length/with-bounds {:t :coll :kind :seq :of element} (max 1 minimum) hi)]
                          (if (zero? minimum) (t/union items t/nil-t) items)))))))
         (t/assumption source))))))

(defmethod special 'reduce-kv [ctx node _]
  (let [[f init c :as args] (args-of node)]
    (if (not= 3 (count args))
      (do (run! #(infer ctx %) args)
          (report! ctx node "TSP2554" "reduce-kv expects 3 arguments")
          t/unknown)
      (let [initial (infer ctx init)
            ct (eff ctx (infer ctx c))
            entries (mapv kv-reduction-input (t/members (:env ctx) ct))
            active (remove #(= 0 (second (:bounds %))) entries)]
        (cond
          (or (= :never (:t initial)) (= :never (:t ct)))
          (do (infer ctx f) t/never)

          (empty? active)
          (do (infer ctx f) initial)

          :else
          (let [may-be-empty? (some #(zero? (first (:bounds %))) entries)
                repeat? (some #(let [maximum (second (:bounds %))] (or (nil? maximum) (> maximum 1))) active)
                inputs (mapv (fn [part]
                               (t/with-assumption (apply t/union (map part active)) (t/assumption ct)))
                             [:key :value])
                start (reduction-start ctx f (widen (dissoc initial :fresh)))
                invariant (if repeat? (reduction-accumulator ctx f start inputs false) start)
                step-args (into [(or invariant start)] inputs)
                ty (reduction-function ctx f [step-args])
                [step stopped] (reduction-step ctx f ty step-args)
                result (t/union step stopped)]
            (when (and (or (nil? invariant) (= :unknown (:t step)))
                       (or (empty? (:arities ty)) (seq (matching-arities ty 3))))
              (unchecked-hof! ctx f "reduce-kv accumulator is not checked" :accumulator))
            (cond
              (nil? invariant) t/unknown
              may-be-empty? (t/union initial result)
              :else result)))))))

(defn- append-to-vectors [ctx target vectors source]
  (let [{:keys [lower upper element]} (sequence-summary ctx source)]
    (t/with-assumption
     (if (= 0 upper)
       (dissoc target :fresh)
       (apply t/union
              (for [vector-type vectors
                    :let [[target-min target-max] (length/bounds vector-type)
                          elements (t/union (elem-type ctx vector-type) element)
                          result (length/with-bounds (coll-of :vector elements)
                                                     (+' target-min lower)
                                                     (when (and target-max upper) (+' target-max upper)))]]
                (cond-> result
                  (and (:items vector-type) (:items source))
                  (assoc :items (into (:items vector-type) (:items source)))))))
     (or (t/assumption target) (t/assumption source)))))

(defn- checked-vector-into [ctx at target vectors source]
  (t/with-assumption
   (apply t/union
          (for [member (t/members (:env ctx) source)]
            (cond
              (= t/never member) t/never
              (unsupported-sequence-member ctx member)
              (do (report! ctx at "TSP2345" (str "into cannot sequence " (t/show member))) t/never)
              :else (append-to-vectors ctx target vectors (or (map-entry-sequence member) member)))))
   (or (t/assumption target) (t/assumption source))))

(defmethod special 'conj [ctx node _]
  (let [[c & xs] (args-of node)
        cty (infer ctx c)
        xts (map #(dissoc (infer ctx %) :fresh) xs)
        members (t/members (:env ctx) cty)
        m (first (remove #(= :nil (:t %)) members))]
    (cond
      (nil? c) (length/with-bounds (t/tuple []) 0 0)
      (= t/never cty) t/never
      (empty? xs) cty
      (some :map-index members) (t/prim :map)
      (every? vector-member? members)
      (let [items (vec xts)]
        (if (some #{t/never} items)
          t/never
          (append-to-vectors ctx cty members (t/tuple items))))
      (= :coll (:t m)) (assoc (dissoc m :items :min-count :max-count :entry-map)
                             :of (apply t/union (:of m) (map widen xts)))
      :else cty)))

(defn- transducer-element [ctx ty]
  (t/with-assumption
   (apply t/union
          (for [member (t/members (:env ctx) ty)]
            (case (:t member)
              :never t/never
              :map (if (and (:complete-keys? member) (empty? (:req member)) (empty? (:opt member)))
                     t/never (t/prim :vector))
              (elem-type ctx member))))
   (t/assumption ty)))

(defn- transducer-output [element upper]
  (if (or (= t/never element) (= 0 upper))
    {:element t/never :upper 0}
    {:element element :upper upper}))

(defn- transducer-source [ctx node source]
  (let [upper (reduce (fn [maximum member]
                        (let [bound (case (:t member)
                                      :never 0
                                      :map (when (:complete-keys? member)
                                             (+ (count (:req member)) (count (:opt member))))
                                      (or (:array-length member) (second (sequence-member-bounds member))))]
                          (if (some? bound) (max maximum bound) (reduced nil))))
                      0 (t/members (:env ctx) source))]
    (transducer-output (transducer-element ctx source)
                       (or upper (list-expression-length ctx node)))))

(defn- literal-transducer-limit [node]
  (let [n (:val node)]
    (when (and (literal-node? node) (number? n))
      (cond
        (not (pos? n)) 0
        (<= n 1) 1
        (integer? n) n))))

(defn- flatten-transducer-element [ctx node message input]
  (let [input (eff ctx input)]
    (if-let [bad (unsupported-sequence-member ctx input)]
      (do (report! ctx node "TSP2345" (str message (t/show bad)))
          t/unknown)
      (transducer-element ctx input))))

(defn- predicate-transducer-element [ctx head f input]
  (let [[ret yes no] (apply-predicate ctx f input true)
        truthy? (contains? #{'filter 'take-while} head)]
    (if (= t/never (truth-part (:env ctx) (eff ctx ret) truthy?))
      t/never
      (case head
        (filter take-while) yes
        remove no
        ;; Once dropping ends, later inputs pass without a predicate call.
        drop-while (if (= t/never no) t/never input)))))

(defn- partition-all-transducer-output [ctx size {:keys [element upper]}]
  (let [amount (infer ctx size)
        value (:val (t/resolve-t (:env ctx) (eff ctx amount)))
        capacity (when (number? value)
                   (try
                     (Math/toIntExact (long value))
                     (catch ArithmeticException _ nil)
                     (catch IllegalArgumentException _ nil)))]
    (assignable! ctx size amount (t/prim :number))
    (cond
      (and (number? value) (or (nil? capacity) (neg? capacity)))
      (do (report! ctx size "TSP2345" "partition-all size must convert to a nonnegative Java int")
          (transducer-output t/never 0))

      (or (= t/never amount) (= t/never element)) (transducer-output t/never 0)

      :else
      (let [positive-capacity (when (and capacity (pos? capacity)) capacity)
            group-upper (if (and positive-capacity upper)
                          (min positive-capacity upper)
                          (or positive-capacity upper))
            groups-upper (cond
                           (nil? capacity) upper
                           (zero? capacity) 1
                           :else (partition-count upper capacity))]
        (transducer-output (length/with-bounds (coll-of :vector element) 1 group-upper)
                           groups-upper)))))

(defn- apply-transducer
  "Infer the emitted element type and its upper count bound. Nil denotes an opaque stage."
  [ctx xf {:keys [element upper] :as input}]
  (let [[f & more :as args] (args-of xf)
        head (when (empty? (:directives (first (:children xf)))) (builtin-head ctx xf))]
    (cond
      (and (= 'clojure.core/cat (guard-symbol ctx xf))
           (= 'cat (t/core-name (:file ctx) (:val xf)))
           (empty? (:directives xf)) (not (env/tag-node xf)))
      (do (record! ctx xf (t/prim :fn))
          (transducer-output (flatten-transducer-element ctx xf "cat cannot reduce input " element) nil))

      (= 'comp head)
      (let [out (reduce (fn [current stage]
                          ;; An opaque stage can also change completion. Keep the
                          ;; chain incomplete while still checking following forms.
                          (let [output (apply-transducer ctx stage (or current {:element t/unknown}))]
                            (when current output)))
                        input args)]
        (record! ctx xf (t/prim :fn))
        out)

      (and (#{'distinct 'dedupe} head) (empty? args))
      (do (record! ctx xf (t/prim :fn))
          input)

      (and (= 'partition-all head) f (empty? more))
      (let [output (partition-all-transducer-output ctx f input)]
        (record! ctx xf (t/prim :fn))
        output)

      (and (= 'interpose head) f (empty? more))
      (let [separator (eff ctx (infer ctx f))]
        (record! ctx xf (t/prim :fn))
        (transducer-output (interposed-element element separator upper)
                           (interposed-length upper)))

      (and (= 'take-nth head) f (empty? more))
      (let [amount (eff ctx (infer ctx f))
            raw (:val (t/resolve-t (:env ctx) amount))
            step (when (and (integer? raw) (not (zero? raw))) (abs (bigint raw)))]
        (record! ctx xf (t/prim :fn))
        (when-not (= t/never element)
          (cond
            (and (number? raw) (zero? raw))
            (report! ctx f "TSP2345" "take-nth transducer step cannot be zero")
            (= :mismatch (:status (t/sub-result (:env ctx) amount (t/prim :number))))
            (assignable! ctx f amount (t/prim :number))))
        (transducer-output element (or (partition-count upper step) upper)))

      (and (#{'take 'drop} head) f (empty? more))
      (let [amount (infer ctx f)
            limit (literal-transducer-limit f)
            maximum (cond
                      (nil? limit) upper
                      (= 'take head) (if upper (min upper limit) limit)
                      upper (max 0 (- upper limit)))]
        (record! ctx xf (t/prim :fn))
        (when-not (= t/never element)
          (assignable! ctx f amount (t/prim :number)))
        (transducer-output element maximum))

      (and (#{'map 'map-indexed 'filter 'remove 'take-while 'drop-while
              'keep 'keep-indexed 'mapcat 'partition-by} head) f (empty? more))
      (let [indexed? (#{'map-indexed 'keep-indexed} head)]
        (record! ctx xf (t/prim :fn))
        (if (= t/never element)
          (let [params (if indexed? [(t/prim :int) element] [element])]
            (if (callback-node? ctx f)
              (infer ctx f {:t :fn :arities [{:params params :ret t/unknown}]})
              (infer ctx f))
            input)
          (let [out (case head
                      (map keep mapcat partition-by) (apply-fn-node ctx f [element])
                      (map-indexed keep-indexed) (apply-indexed-callback ctx f element)
                      (filter remove take-while drop-while)
                      (predicate-transducer-element ctx head f element))
                output (case head
                         (keep keep-indexed) (t/remove-nil (:env ctx) (eff ctx out))
                         mapcat (flatten-transducer-element ctx f "mapcat cannot reduce callback result " out)
                         partition-by (length/with-bounds (coll-of :vector element) 1 nil)
                         out)]
            (transducer-output output (when-not (= 'mapcat head) upper)))))

      :else
      (do (infer ctx xf)
          (unchecked-hof! ctx xf "into: transducer input and output types not checked" :unsupported-function)
          nil))))

(defn- into-transformed [ctx target out & [{:keys [upper] :as output}]]
  (if (= t/never out)
    (dissoc target :fresh)
    (let [out (widen (dissoc out :fresh))]
      (apply t/union
             (for [m (t/members (:env ctx) target)]
               (case (:t m)
                 :coll (let [result (update (dissoc m :fresh :items :min-count :max-count :entry-map) :of t/union out)]
                         (if output
                           (let [[target-min target-max] (length/bounds m)]
                             (length/with-bounds result target-min
                                                 (when (and target-max upper) (+' target-max upper))))
                           result))
                 (:map :map-of) (t/prim :map)
                 :nil (t/union t/nil-t
                               (cond-> (coll-of :seq out)
                                 output (length/with-bounds 1 upper)))
                 :prim (if (#{:vector :set :seq :map :coll} (:name m)) m t/unknown)
                 :never t/never
                 :any t/any
                 t/unknown))))))

(defmethod special 'into [ctx node _]
  (let [[to second-arg from :as args] (args-of node)]
    (case (count args)
      0 (coll-of :vector t/never)
      1 (infer ctx to)
      2 (let [target (infer ctx to)
              source (eff ctx (infer ctx second-arg))
              members (t/members (:env ctx) target)]
          (cond (some #{t/never} [target source]) t/never
                (every? vector-member? members) (checked-vector-into ctx second-arg target members source)
                (and (= :vector (:op to)) (empty? (:children to))) (coll-of :vector (elem-type ctx source))
                (and (= :set (:op to)) (empty? (:children to))) (coll-of :set (elem-type ctx source))
                (some #(or (:items %) (length/constrained? %) (:map-index %)) members)
                (into-transformed ctx target (elem-type ctx source))
                :else (dissoc target :fresh)))
      3 (let [target (infer ctx to)
              source (infer ctx from)
              output (apply-transducer ctx second-arg (transducer-source ctx from (eff ctx source)))
              out (get output :element t/unknown)]
          (when-let [bad (unsupported-sequence-member ctx source)]
            (report! ctx from "TSP2345" (str "into cannot reduce " (t/show bad))))
          (if (= t/never source) t/never (into-transformed ctx target out output)))
      (do (run! #(infer ctx %) args)
          (report! ctx node "TSP2554" "into expects zero to three arguments")
          t/unknown))))

(defn- initial-atom-content [ty]
  (case (:t ty)
    :map (t/prim :map)
    :coll (if (and (= :never (:t (:of ty))) (:kind ty))
            (t/prim (:kind ty))
            (dissoc ty :items :min-count :max-count :fresh :entry-map))
    :union (apply t/union (map initial-atom-content (:types ty)))
    (widen (dissoc ty :fresh))))

(defn- atom-contents [ctx node ty]
  (let [members (t/members (:env ctx) (eff ctx ty))
        atoms (filter #(= :atom (:t %)) members)]
    (when (and (seq atoms) (every? #(#{:atom :nil} (:t %)) members))
      (when (some #(= :nil (:t %)) members)
        (assignable! ctx node ty (t/class-type "clojure.lang.Atom")))
      (vec (distinct (map :of atoms))))))

(defn- atom-write! [ctx node actual content]
  (binding [t/*strict-shapes* true]
    (let [resolved (t/resolve-t (:env ctx) content)]
      (if (= :unknown (:t resolved))
        (unchecked-hof! ctx node "atom content type is not checked" :atom-content)
        (assignable! ctx node actual content)))))

(defmethod special 'atom [ctx node _]
  (let [[init & options] (args-of node)
        content (:atom-content ctx)
        ctx (dissoc ctx :atom-content)]
    (if init
      (let [ty (infer-with-expected ctx init content)]
        (run! #(infer ctx %) options)
        (when content (atom-write! ctx init ty content))
        (when (odd? (count options))
          (report! ctx node "TSP2554" "atom expects key/value options after its initializer"))
        (if (= t/never ty) t/never {:t :atom :of (or content (initial-atom-content ty))}))
      (do (report! ctx node "TSP2554" "atom expects an initializer") t/unknown))))

(defmethod special 'deref [ctx node _]
  (let [[target & options :as args] (args-of node)
        ty (when target (infer ctx target))
        contents (when ty (atom-contents ctx target ty))
        members (when (and ty (not (seq contents))) (t/members (:env ctx) (eff ctx ty)))
        reduced? (and (seq members) (every? reduced-type? members))]
    (run! #(infer ctx %) options)
    (cond
      (= t/never ty) t/never
      (and (seq contents) (empty? options)) (apply t/union contents)
      (and reduced? (empty? options))
      (t/with-assumption (apply t/union (map reduced-payload members)) (t/assumption ty))
      (or (not (#{1 3} (count args))) (seq contents) reduced?)
      (do (report! ctx node "TSP2554" (cond (seq contents) "deref of an atom expects one argument"
                                           reduced? "deref of Reduced expects one argument"
                                           :else "deref expects 1 or 3 arguments"))
          t/unknown)
      :else t/unknown)))

(defn- atom-write-contents [ctx target ty values?]
  (or (atom-contents ctx target ty)
      (let [expected (t/class-type (if values? "clojure.lang.IAtom2" "clojure.lang.IAtom"))]
        (when (= :mismatch (:status (t/sub-result (:env ctx) (eff ctx ty) expected)))
          (assignable! ctx target ty expected))
        nil)))

(defn- atom-value-pairs [contents after]
  (apply t/union (for [before (or (seq contents) [t/unknown])]
                   (t/tuple [before (or after before)]))))

(doseq [op '[reset! reset-vals! compare-and-set!]]
  (defmethod special op [ctx node _]
    (let [args (args-of node)
          compare? (= 'compare-and-set! op)
          values? (= 'reset-vals! op)
          arity (if compare? 3 2)
          types (mapv #(infer ctx %) args)]
      (cond
        (not= arity (count args))
        (do (report! ctx node "TSP2554" (str op " expects " arity " arguments")) t/unknown)

        (some #{t/never} types) t/never

        :else
        (let [contents (atom-write-contents ctx (first args) (first types) values?)
              actual (peek types)]
          ;; The old value in compare-and-set! is an identity test. Only the new value is written.
          (doseq [content contents] (atom-write! ctx (last args) actual content))
          (cond compare? (t/prim :boolean)
                values? (atom-value-pairs contents (dissoc actual :fresh))
                (seq contents) (apply t/union contents)
                :else t/unknown))))))

(doseq [op '[swap! swap-vals!]]
  (defmethod special op [ctx node _]
    (let [[target f & args :as all] (args-of node)
          values? (= 'swap-vals! op)]
      (if-not f
        (do (run! #(infer ctx %) all)
            (report! ctx node "TSP2554" (str op " expects an atom and an update function"))
            t/unknown)
        (let [ty (infer ctx target)
              contents (atom-write-contents ctx target ty values?)
              arg-types (mapv #(infer ctx %) args)]
          (if (seq contents)
            (let [current (apply t/union contents)
                  before (count @*diags*)
                  assoc? (= 'assoc (builtin-head ctx {:op :list :children [f]}))
                  result (if assoc?
                           (if (and (seq args) (even? (count args)))
                             (let [accepted (t/union (t/prim :map) (t/prim :vector) t/nil-t)]
                               (assignable! ctx f current accepted)
                               (assoc-entries ctx current (map vector (partition 2 args) (partition 2 arg-types))))
                             (do (report! ctx f "TSP2554" (str "assoc in " op " expects key/value arguments")) t/never))
                           (apply-fn-node ctx f (into [current] arg-types) true))]
              (if (or (= t/never result) (some #{t/never} arg-types))
                t/never
                (do (when (or (not= :unknown (:t result)) (= before (count @*diags*)))
                      (doseq [content contents] (atom-write! ctx f result content)))
                    (if values? (atom-value-pairs contents nil) current))))
            (do (infer ctx f)
                (cond (or (= t/never ty) (some #{t/never} arg-types)) t/never
                      values? (atom-value-pairs nil nil)
                      :else t/unknown))))))))

(defmethod special 'new [ctx node _]
  (let [[cls & args] (args-of node)]
    (constructor-call ctx node
                      (when (= :sym (:op cls)) (t/resolve-class (:imports (:file ctx)) (:val cls)))
                      args)))

;; ---- defn ----

(defn- check-return! [ctx node actual expected & [implementation]]
  (when-not (or (:generic-failure actual) (#{:unknown :any} (:t expected)))
    (let [env (:env ctx) actual (eff ctx actual)
          result (t/sub-result env actual expected)
          nested-error (when (= :mismatch (:status result))
                         (or (nested-platform-problem ctx "TSP2322" actual expected result)
                             (nested-type-problem ctx "TSP2322" result)))
          collection-error (when (= :mismatch (:status result)) (collection-problem ctx actual expected))
          emit (fn [[c m extra]]
                 (apply report! ctx node c (if implementation (str m " (" implementation ")") m)
                        (mapcat identity extra)))]
      (cond
        (= :proved (:status result)) (run! emit (problems ctx node actual expected))
        nested-error (emit nested-error)
        (seq (fresh-problems ctx actual expected)) (run! emit (fresh-problems ctx actual expected))
        (= :inconclusive (:status result)) (emit (incomplete-problem ctx actual expected result))
        collection-error (let [[code message] collection-error]
                      (emit [(if (= "TSP330-01" code) code "TSP2322") message]))
        (and (t/nilable? env actual) (not (t/nilable? env expected)) (t/sub? env (t/remove-nil env actual) expected))
        (emit ["TSP2322" (str "may return nil, but the signature says " (show-ref ctx expected)
                             (when-let [w (and (= :sym (:op node)) (get-in ctx [:why (:val node)]))]
                               (str ": " w)))])
        :else (if-let [[k opt?] (missing-req ctx actual expected)]
                (emit ["TSP2741" (str "the returned map has no required key " k " of " (t/show expected)
                                     (when opt? " (it is optional in the returned map)"))])
                (emit ["TSP2322" (str "returns " (show-ref ctx actual) ", but the signature says "
                                     (show-ref ctx expected))]))))))

(defn- spec-implementation-plans [bodies contracts coverage]
  (let [runtime (mapv (fn [{:keys [params]}]
                        (let [{:keys [fixed rest]} (env/split-params params)]
                          {:node params :params (vec (repeat (count fixed) t/unknown))
                           :rest (when rest t/unknown)}))
                      bodies)]
    (mapv #(t/dispatch-plan runtime % coverage) contracts)))

(defn- merge-implementation-contracts [n contracts]
  (when (seq contracts)
    {:params (mapv (fn [i] (apply t/union (map #(nth (:params %) i) contracts))) (range n))
     :tail (apply t/union
                  (for [{:keys [params rest] :as contract} contracts]
                    (if rest
                      (let [tail {:t :coll :kind :seq :of (apply t/union rest (drop n params))
                                  :min-count (max 1 (- (t/arity-minimum contract) n))}]
                        (if (= n (t/arity-minimum contract)) (t/union tail t/nil-t) tail))
                      (if (= n (count params))
                        t/nil-t
                        (assoc (t/tuple (subvec params n)) :kind :seq)))))
     :ret (apply t/union (map :ret contracts))}))

(defn- check-defn [ctx node]
  (let [file (:file ctx) env (:env ctx)
        parts (env/defn-parts node)
        nm (:name parts)
        sym (symbol (str (:ns file)) (str (:val nm)))
        v (get-in env [:vars sym])
        v (or (:prior-defn v) v)
        ftype (if (= :all (:t (:type v))) (solver/open-rigid (:type v)) (:type v))
        bodies (:arities parts)
        plans (when (and (= :fdef (:source v))
                         (not-any? #(= :map (:op (:rest (env/split-params (:params %))))) bodies))
                (spec-implementation-plans bodies (:arities ftype) :total))
        body-contracts (group-by (comp :node first) (mapcat identity plans))]
    (when-let [tn (env/tag-node nm)]
      (when (t/primitives (str (:val tn)))
        (report! ctx tn "TSP7021"
                 (str "a hint on the name is not a return type: write (defn " (:val nm) " ^" (:val tn) " [...] ...)"))))
    (when (and (:strict (:opts ctx)) (not (:private? parts)) (= :hints (:source v))
               (some (fn [a] (or (= :unknown (:t (:ret a))) (some #(= :unknown (:t %)) (:params a)))) (:arities ftype)))
      (report! ctx nm "TSP7010" (str "public function " (:val nm) " has no signature: add s/fdef, {:t/sig '[...]} or type hints")))
    (when (some nil? plans)
      (report! ctx nm "TSP2790" (str "implementation of " (:val nm) " does not accept every arity in " (t/show ftype))))
    (doseq [{:keys [params body]} bodies
            :let [{:keys [fixed rest]} (env/split-params params)
                  declared (if plans
                             (merge-implementation-contracts (count fixed) (map second (get body-contracts params)))
                             (or (first (filter #(and (= (count fixed) (count (:params %)))
                                                    (= (boolean rest) (boolean (or (:rest %) (:kwargs %)))))
                                              (:arities ftype)))
                                 (first (filter #(= (count fixed) (count (:params %))) (:arities ftype)))))]]
      (if (nil? declared)
        (report! ctx params "TSP2790" (str "the signature of " (:val nm) " has no arity with " (count fixed) " parameter(s)"))
        (let [prims (filter #(some-> (env/tag-node %) :val str t/primitives) fixed)
              _ (when (and (seq prims) (> (count fixed) 4))
                  (report! ctx params "TSP7025" "functions taking primitives support only 4 or fewer parameters"))
              ctx' (reduce (fn [c [p ty]]
                             (let [c' (bind-pattern c p ty)]
                               (record! ctx p ty)
                               (if (= :sym (:op p)) (update c' :params (fnil conj #{}) (:val p)) c')))
                           (dissoc ctx :recur-types :load-time?) (map vector fixed (:params declared)))
              ctx' (cond (nil? rest) ctx'
                         (:kwargs declared) (bind-pattern ctx' rest (:kwargs declared))
                         (:tail declared) (bind-pattern ctx' rest (:tail declared))
                         (:rest declared) (bind-pattern ctx' rest (t/union {:t :coll :kind :seq :of (:rest declared)} t/nil-t))
                         :else (bind-pattern ctx' rest t/unknown))
              ret-hint (env/hint-type file params)]
          (when (and ret-hint (not (#{:unknown :any} (:t (:ret declared))))
                     (not (t/sub? env ret-hint (:ret declared))) (not (t/sub? env (:ret declared) ret-hint)))
            (report! ctx (env/tag-node params) "TSP2322"
                     (str "hint " (t/show ret-hint) " contradicts the signature: " (t/show (:ret declared)))))
          (let [rty (infer-body ctx' body (:ret declared))]
            (when (seq body) (check-return! ctx' (last body) rty (:ret declared)))))))
    t/unknown))

(defmethod special 'defn [ctx node _] (check-defn ctx node))
(defmethod special 'defn- [ctx node _] (check-defn ctx node))

(defn- qualified-var [file sym]
  (let [resolved (env/resolve-sym file sym)]
    (if (qualified-symbol? resolved) resolved (symbol (str (:ns file)) (name resolved)))))

(defn- dispatch-leaf-class [leaf]
  (let [primitive (if (= :lit (:t leaf)) (:name (t/lit-prim (:val leaf))) (:name leaf))]
    (cond
      (= :class (:t leaf)) (t/load-class (:name leaf))
      (and (#{:prim :lit} (:t leaf)) (#{:string :boolean :keyword :symbol :uuid} primitive))
      (t/load-class (t/prim->class primitive)))))

(defn- disjoint-dispatch-classes? [^Class a ^Class b]
  (and (not (.isAssignableFrom a b)) (not (.isAssignableFrom b a))
       ;; Arrays of different interface types can have a common subtype.
       (not (or (.isArray a) (.isArray b)))
       (or (and (not (.isInterface a)) (not (.isInterface b)))
           (java.lang.reflect.Modifier/isFinal (.getModifiers a))
           (java.lang.reflect.Modifier/isFinal (.getModifiers b)))))

(defn- class-dispatch-filter [ctx dispatch]
  (if (and (= :nil (:op dispatch)) (literal-node? dispatch))
    (fn [leaf]
      (let [nil-type (nil-part (:env ctx) leaf)]
        (when-not (= t/never nil-type) nil-type)))
    (when-let [class-name (class-symbol-name ctx dispatch)]
      (when-let [^Class target (t/load-class class-name)]
        (when-not (.isPrimitive target)
          (fn [leaf]
            (let [receiver (dispatch-leaf-class leaf)]
              (when-not (and receiver (disjoint-dispatch-classes? target receiver))
                (first (instance-parts ctx class-name leaf))))))))))

(defn- narrow-dispatch-params [ctx types {:keys [arity index key default kind]} dispatch]
  (let [class? (= :class kind)
        narrow (when (and (= arity (count types)) (not= default (:val dispatch)))
                 (if class?
                   (class-dispatch-filter ctx dispatch)
                   (when (and (= :str (:op dispatch)) (literal-node? dispatch))
                     #(when (= (:val dispatch) (:val %)) %))))]
    (if narrow
      (let [original (nth types index)
            alternatives (path-alternatives ctx original (if key [key] []) class?)
            string-leaf? (fn [[leaf]] (and (= :lit (:t leaf)) (string? (:val leaf))))
            matches (when (or class? (every? string-leaf? alternatives))
                      (keep (fn [[leaf rebuild]] (some-> (narrow leaf) rebuild)) alternatives))]
        (if (seq matches)
          (assoc types index (t/with-assumption (apply t/union matches) (t/assumption original)))
          types))
      types)))

(defmethod special 'defmethod [ctx node _]
  (let [{:keys [target dispatch local-name arities]} (env/defmethod-parts node)
        {:keys [env file]} ctx
        sym (when (and (= :sym (:op target)) (not (contains? (:locals ctx) (:val target))))
              (qualified-var file (:val target)))
        shared (get-in env [:vars sym])
        contracts (when (= :fdef (:source shared)) (get-in shared [:type :arities]))
        selector (get-in env [:multimethod-dispatches sym])
        plans (spec-implementation-plans arities contracts :partial)
        by-params (group-by (comp :node first) (mapcat identity plans))
        description (when contracts (str "defmethod " sym " " (parse/unparse dispatch) ", s/fdef"))
        ctx (cond-> (dissoc ctx :recur-types :load-time?)
              local-name (bind (:val local-name) t/unknown))]
    (when sym (record-var-use! ctx sym target :implementation))
    (doseq [{:keys [params body]} arities
            :let [{:keys [fixed rest]} (env/split-params params)
                  expected (merge-implementation-contracts (count fixed) (map second (get by-params params)))
                  types (or (:params expected) (mapv #(env/pattern-type env file %) fixed))
                  types (if (and expected selector (nil? rest))
                          (narrow-dispatch-params ctx types selector dispatch)
                          types)
                  method-ctx (reduce (fn [c [param type]]
                                       (when expected (record! c param type))
                                       (bind-pattern c param type))
                                     ctx (map vector fixed types))
                  method-ctx (if rest
                               (bind-pattern method-ctx rest
                                             (if (= :map (:op rest))
                                               (env/pattern-type env file rest)
                                               (or (:tail expected) t/unknown)))
                               method-ctx)
                  actual (infer-body method-ctx body)]
            :when expected]
      (check-return! method-ctx (or (last body) params) actual (:ret expected) description))
    t/unknown))

(defn- method-groups [forms target-ops]
  (reduce (fn [groups form]
            (cond
              (target-ops (:op form)) (conj groups [form []])
              (and (= :list (:op form)) (seq groups)) (update-in groups [(dec (count groups)) 1] conj form)
              :else groups))
          [] forms))

(defn- protocol-diagnostic! [ctx at code message]
  (report! ctx at code message :severity (if (:strict (:opts ctx)) :error :warning)))

(defn- check-protocol-body [ctx method fixed body params expected receiver]
  (let [types (or (:params expected) (mapv #(env/pattern-type (:env ctx) (:file ctx) %) fixed))
        types (if receiver (assoc types 0 receiver) types)
        method-ctx (reduce (fn [c [p ty]] (record! c p ty) (bind-pattern c p ty))
                           (dissoc ctx :recur-types :load-time?) (map vector fixed types))
        actual (infer-body method-ctx body)]
    (when expected
      (check-return! method-ctx (or (last body) params method) actual (:ret expected)))))

(defn- check-protocol-group [ctx protocol methods receiver form-kind]
  (let [full (qualified-var (:file ctx) (:val protocol))
        declared (get-in ctx [:env :protocols full])]
    (when declared
      (let [covered (volatile! #{})]
        (doseq [form methods
                :let [{method :name arities :arities}
                      (env/defn-parts (update form :children #(into [{:op :sym :val 'defn}] %)))
                      method-name (when (= :sym (:op method))
                                    ;; extend-type and extend-protocol key the method by its name.
                                    (if (= :reify form-kind) (:val method) (symbol (name (:val method)))))
                      {:keys [var] allowed :arities} (get declared method-name)]]
          (if-not (and var (seq arities))
            (report! ctx (or method form) "TSP430-01"
                     (str "invalid method " (parse/unparse method) " for protocol " full))
            (do
              (record-var-use! ctx var method :implementation)
              (doseq [{:keys [params body]} arities
                      :let [{:keys [fixed rest]} (env/split-params params)
                            n (count fixed)
                            method-arity [method-name n]
                            contracts (get-in ctx [:env :vars var :contract-arities])
                            contract (merge-implementation-contracts n (filter #(= n (count (:params %))) contracts))]]
                (cond
                  (or rest (not (contains? allowed n)))
                  (report! ctx params "TSP430-01" (str var " has no fixed arity " n " in " full))

                  (contains? @covered method-arity)
                  (report! ctx params "TSP430-01" (str "duplicate implementation of " var " with arity " n))

                  :else
                  (do (vswap! covered conj method-arity)
                      (when-not contract
                        (protocol-diagnostic! ctx method "TSP430-03" (str var " has no s/fdef for arity " n)))
                      (check-protocol-body ctx method fixed body params contract receiver)))))))
        (when-not (#{:extend-protocol :extend} form-kind)
          (doseq [[method {:keys [var arities]}] (sort-by key declared)
                  n arities :when (not (contains? @covered [method n]))]
            (protocol-diagnostic! ctx protocol "TSP430-02" (str "missing implementation of " var " with arity " n))))))))

(defmethod special 'reify [ctx node _]
  (doseq [[protocol methods] (method-groups (args-of node) #{:sym})]
    (check-protocol-group ctx protocol methods nil :reify))
  t/unknown)

(defn- extension-receiver [ctx target]
  (case (:op target)
    :nil t/nil-t
    :sym (some-> (t/resolve-class (get-in ctx [:file :imports]) (:val target)) t/class-type)
    nil))

(defmethod special 'extend-type [ctx node _]
  (let [[target & groups] (args-of node)
        receiver (extension-receiver ctx target)]
    (doseq [[protocol methods] (method-groups groups #{:sym})]
      (check-protocol-group ctx protocol methods receiver :extend-type)))
  t/nil-t)

(defmethod special 'extend-protocol [ctx node _]
  (let [[protocol & groups] (args-of node)]
    (when (= :sym (:op protocol))
      (doseq [[target methods] (method-groups groups #{:sym :nil})]
        (check-protocol-group ctx protocol methods (extension-receiver ctx target) :extend-protocol))))
  t/nil-t)

(defn- direct-extension-receiver [ctx target]
  (when (and (empty? (:directives target)) (not (env/tag-node target)))
    (case (:op target)
      :nil t/nil-t
      :sym (some-> (class-symbol-name ctx target) t/class-type)
      :list (when-let [^Class cls (literal-for-name-class ctx target)]
              (t/class-type (.getName cls)))
      nil)))

(defn- literal-extension-methods [ctx method-map]
  (when (and (= :map (:op method-map)) (empty? (:directives method-map)) (not (env/tag-node method-map)))
    (let [methods (mapv (fn [[k f]]
                          (let [[_ & parts] (:children f)]
                            (when (and (= :kw (:op k)) (literal-node? k) (simple-keyword? (:val k))
                                       (#{'fn 'fn*} (builtin-head ctx f))
                                       (#{:vector :list} (:op (first parts))))
                              (assoc f :children (into [(assoc k :op :sym :val (symbol (name (:val k))))] parts)))))
                        (partition 2 (:children method-map)))]
      (when (every? some? methods) methods))))

(defmethod special 'extend [ctx node _]
  (let [[target & groups] (args-of node)
        receiver (direct-extension-receiver ctx target)]
    (if-not target
      (do (report! ctx node "TSP2554" "extend expects a class or nil") t/unknown)
      (let [target-type (infer ctx target)]
        (if (and receiver (even? (count groups)))
          (doseq [[protocol method-map] (partition 2 groups)]
            (if-let [methods (when (and (= :sym (:op protocol)) (empty? (:directives protocol))
                                        (not (env/tag-node protocol)) (not (contains? (:locals ctx) (:val protocol)))
                                        (get-in ctx [:env :protocols (qualified-var (:file ctx) (:val protocol))]))
                               (literal-extension-methods ctx method-map))]
              (check-protocol-group ctx protocol methods receiver :extend)
              (do (infer ctx protocol) (infer ctx method-map))))
          (run! #(infer ctx %) groups))
        (if (= t/never target-type) t/never t/nil-t)))))

(defn- guard-claim [env entry]
  (when-let [node (env/attr-get (get-in entry [:parts :attr]) :t/guard)]
    (let [raw (parse/node->data node)
          data (if (and (seq? raw) (= 'quote (first raw))) (second raw) raw)
          {:keys [type mode two-sided] :as contract}
          (if (map? data) data {:type data :mode :trusted})
          invalid (volatile! false)
          fail! (fn [& _] (vreset! invalid true))
          type-ctx (assoc (env/type-ctx (:pfile entry) fail!) :operator-error fail!)
          target (operators/normalize env (t/parse-type type-ctx type) fail!)
          valid? (and (#{:checked :trusted} mode) (contains? contract :type) (not @invalid)
                      (every? #{:type :mode :two-sided} (keys contract))
                      (or (nil? two-sided) (boolean? two-sided))
                      (not (and two-sided (= :trusted mode))))]
      {:node node :target target :mode mode :two-sided two-sided :valid? valid?})))

(defn- dynamic-definition? [entry]
  (or (some-> (env/attr-get (get-in entry [:parts :attr]) :dynamic) parse/node->data)
      (some (fn [node]
              (let [form (parse/node->data node)]
                (or (= :dynamic form) (and (map? form) (:dynamic form)))))
            (get-in entry [:parts :name :meta]))))

(defn- guard-candidate [env [sym entry]]
  (when-let [definition (:definition entry)]
    (let [claim (guard-claim env entry)
          parts (get-in entry [:parts :arities])
          body (:body (first parts))
          {:keys [fixed rest]} (env/split-params (:params (first parts)))
          arities (get-in entry [:type :arities])
          param (first fixed)
          unary? (and (= 1 (count parts) (count fixed) (count arities))
                      (= 1 (count (:params (first arities))))
                      (nil? rest) (= :sym (:op param))
                      (not (:rest (first arities))) (not (:kwargs (first arities))))
          stable? (and (= 1 (get-in env [:root-definitions sym]))
                       (not (dynamic-definition? entry)))
          domain (guards/input-type (get-in arities [0 :params 0]))
          domain (if (#{:any :unknown} (:t (t/resolve-t env domain))) t/unknown domain)
          ctx {:env env :file (:pfile entry) :locals {}}
          plan (when (and unary? stable?) (guards/compile-body (guard-compiler ctx) [(:val param)] body))]
      (when (or claim (guards/useful? plan))
        [sym {:claim claim :entry entry :definition definition :ctx ctx
              :unary? unary? :stable? stable? :domain domain :plan plan}]))))

(defn- compile-guards [env opts]
  (let [candidates (into {} (keep #(guard-candidate env %)) (:vars env))
        solved (atom {})
        summaries (atom {})
        diagnostics (atom [])]
    (letfn [(finish [sym result]
              (swap! solved assoc sym result)
              result)
            (reject! [candidate code message reason]
              (when-let [node (get-in candidate [:claim :node])]
                (swap! diagnostics conj {:code code :severity :error :file (get-in candidate [:entry :file])
                                         :pos (:pos node) :message message :reasons #{reason}})))
            (body-valid? [{:keys [ctx definition]}]
              (binding [*diags* (atom []) *types* nil *ignores* (atom []) *rets* (atom {})
                        *trust* (atom {:cast 0 :any 0 :ignore 0})]
                (let [checked (into {} (filter #(= :checked (:mode (val %)))) @summaries)]
                  (check-defn (-> ctx (assoc :opts (assoc opts :strict false)) (assoc-in [:env :guards] checked)) definition)
                  (not-any? #(= :error (:severity %)) @*diags*))))
            (visit [sym path]
              (cond
                (path sym) :recursive
                (>= (count path) 64) :limit
                (contains? @solved sym) (get @solved sym)
                :else
                (when-let [{:keys [claim unary? stable? domain plan] :as candidate} (get candidates sym)]
                  (let [{:keys [valid? mode target two-sided]} claim
                        deps (when (and plan (not= :trusted mode)) (guards/dependencies plan))
                        dependency-error (some #{:recursive :limit} (map #(visit % (conj path sym)) (sort deps)))
                        proof-env (assoc env :guards @summaries)]
                    (cond
                      (or (and claim (not valid?)) (not unary?))
                      (do (reject! candidate "TSP250-03" "invalid guard declaration or unsupported parameter list" :invalid-guard)
                          (finish sym :invalid))

                      (not stable?)
                      (do (reject! candidate "TSP250-03" "guard Var can have a different definition" :unstable-var)
                          (finish sym :invalid))

                      (= :trusted mode)
                      (do (swap! summaries assoc sym {:mode :trusted :domain domain :target target})
                          (finish sym :trusted))

                      (or dependency-error (not (guards/useful? plan)))
                      (do (reject! candidate "TSP250-03" "guard body cannot provide checked facts"
                                   (or dependency-error :unsupported-body))
                          (finish sym (or dependency-error :unsupported)))

                      (not (body-valid? candidate))
                      (do (reject! candidate "TSP250-03" "guard body has type errors" :body-errors)
                          (finish sym :invalid))

                      :else
                      (let [[yes no] (guards/split proof-env plan domain)]
                        (cond
                          (and claim (or (= t/never yes) (not (guards/proves? proof-env yes target))))
                          (do (reject! candidate "TSP250-01" (str "truthy result does not prove " (t/show target)) :truthy)
                              (finish sym :invalid))

                          (and two-sided (not= t/never no)
                               (not= t/never (second (guards/split proof-env plan target))))
                          (do (reject! candidate "TSP250-02" (str "falsy result does not exclude " (t/show target)) :falsy)
                              (finish sym :invalid))

                          :else
                          (do (swap! summaries assoc sym (cond-> {:mode :checked :domain domain :plan plan
                                                                 :truthy yes :falsy no}
                                                          claim (assoc :target target)))
                              (finish sym :checked)))))))))]
      (run! #(visit % #{}) (sort (keys candidates)))
      {:guards @summaries :diagnostics @diagnostics})))

;; ---- infer ----

(defn- parse-directive-type [ctx d]
  (let [form (parse/node->data (second (:children d)))]
    (t/parse-type (env/type-ctx (:file ctx) (fn [f] (report! ctx d "TSP9001" (str "not a type: " (pr-str f))))) form)))

(defn- directive-head [directive]
  (:val (first (:children directive))))

(def ^:private literal-directive? (comp '#{t/const t/satisfies} directive-head))

(defn- literal-directives [ctx node]
  (when-let [directives (seq (filter literal-directive? (:directives node)))]
    (let [groups (group-by directive-head directives)
          conflict (or (some #(#{'t/cast 't/any} (directive-head %)) (:directives node))
                       (env/tag-node node))]
      (into {}
            (keep (fn [d]
                    (let [h (directive-head d)]
                      (if-let [message (cond
                                         (:missing-directive-target node) "missing expression"
                                         (next (groups h)) "duplicate directive on one expression"
                                         conflict "cannot combine with t/cast, t/any or a type hint on the same expression"
                                         (not= (count (:children d)) (if (= h 't/const) 1 2))
                                         (if (= h 't/const) "expects no arguments" "expects exactly one type"))]
                        (do (report! ctx d "TSP280-02" (str h ": " message)) nil)
                        [h d]))))
            directives))))

(defn- record-escape! [ctx directive kind]
  ;; t/satisfies tracks visits within the expression. The trust summary counts source directives.
  (let [site [(:file (:file ctx)) (:pos directive)]]
    (swap! *trust* (fn [trust]
                    (cond-> (update trust kind inc)
                      (not (:quiet ctx)) (update-in [::escape-sites kind] (fnil conj #{}) site))))))

(defn- check-satisfies! [ctx node directive actual before-trust]
  (let [annotation (second (:children directive))
        env (:env ctx)
        {:keys [type reason]} (literals/parse-contract env (:file ctx) (parse/node->data annotation true))
        escape (some #(when (> (get @*trust* % 0) (get before-trust % 0)) %) [:cast :any])]
    (if reason
      (report! ctx annotation "TSP280-01" (str "t/satisfies cannot check " (parse/unparse annotation)
                                             " (" (name reason) ")")
               :reasons #{reason})
      (if-let [issue (or escape (literals/evidence-issue env actual))]
        (report! ctx node "TSP280-01" (str "t/satisfies cannot establish " (show-ref ctx type)
                                         " (" (name issue) ")")
                 :reasons #{issue})
        (let [fields (when (= :map (:op node))
                       (into {} (map (fn [[k v]] [(:val k) {:key k :value v}]))
                             (partition 2 (:children node))))]
          (binding [t/*strict-shapes* true]
            (doseq [[code message details [role field]] (problems ctx node actual type)]
              (apply report! ctx (or (get-in fields [field role]) node) code message
                     (mapcat identity (cond-> details (= "TSP210-01" code) (assoc :severity :error)))))))))))

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

(defn- check-spec-macro [ctx node sym {:keys [spec index arity]} args]
  (if (= arity (count args))
    (doseq [[i arg] (map-indexed vector args)]
      (let [actual (infer ctx arg)]
        (when (= i index)
          (assignable! ctx arg actual (t/ref-t spec)))))
    (report! ctx node "TSP2554" (str sym " expects " arity " argument(s), got " (count args))))
  t/unknown)

(defn- check-unknown-call [ctx args]
  (let [binding-vector (first args)
        bindings (:children binding-vector)
        possible? (and (= :vector (:op binding-vector)) (seq bindings)
                       (every? #(and (= :sym (:op %)) (simple-symbol? (:val %))) bindings))
        ;; The call may be a foreign binding macro. Without a contract these
        ;; names could be locals, so their uses cannot prove a read of a Var.
        ctx (cond-> ctx possible? (update :possible-bindings (fnil into #{}) (map :val bindings)))]
    (run! #(infer ctx %) args)
    t/unknown))

(defn- infer-list [ctx node]
  (let [h (first (:children node))
        args (args-of node)
        file (:file ctx)]
    (when (and (#{:sym :kw} (:op h))
               (some literal-directive? (:directives h)))
      (infer ctx h))
    (cond
      (nil? h) {:t :coll :kind :seq :of t/never :jvm-class "clojure.lang.IPersistentList"}
      (= :kw (:op h)) (let [[m default] args
                            ty (infer ctx m)]
                        (if default
                          (read-key ctx ty (:val h) h (infer ctx default) t/unknown)
                          (read-key ctx ty (:val h) h)))
      (not= :sym (:op h))
      (let [types (when (inline-callback? ctx h) (mapv #(infer ctx %) args))
            ft (t/resolve-t (:env ctx) (eff ctx (infer ctx h (some-> types direct-callback-context))))]
        (if (#{:fn :all} (:t ft))
          (call-fn ctx node (parse/unparse h) ft args types)
          (do (when-not types (run! #(infer ctx %) args)) t/unknown)))
      (contains? (:locals ctx) (:val h))
      (if-let [callback (deferred-callback ctx h)]
        (let [types (mapv #(infer ctx %) args)
              ft (force-callback ctx callback (direct-callback-context types))]
          (apply-type ctx ft types node (str (:val h))))
        (let [local (get-in ctx [:locals (:val h)])
              ft (if (= :ref (:t local)) (t/resolve-t (:env ctx) local) local)]
          (if (or (= :all (:t ft)) (seq (:arities ft)))
            (call-fn ctx node (str (:val h)) ft args)
            (do (run! #(infer ctx %) args) t/unknown))))
      :else
      (let [sym (:val h) n (name sym) head (env/head file node)
            own? (and (nil? (namespace (env/resolve-sym file sym)))
                      (contains? (get-in (:env ctx) [:ns-defs (:ns file)]) head))
            core-sym (if (namespace head) head (symbol "clojure.core" (name head)))
            extern? (= :extern (get-in ctx [:env :vars core-sym :source]))
            has-rule? (and (not own?) (not extern?) (not= (get-method special head) (get-method special ::none)))
            ;; An unqualified, unreferred head names a contract only when this
            ;; namespace itself defines that macro: never a function or core form.
            local-sym (symbol (str (:ns file)) (name head))
            contract-sym (cond (namespace head) head
                               (contains? (:macros (:env ctx)) local-sym) local-sym)
            contract (when contract-sym (get-in ctx [:env :macro-contracts contract-sym]))
            spec-contract (when-not (:guards-disabled? ctx)
                            (get-in ctx [:env :spec-macro-contracts contract-sym]))
            r (cond contract (check-macro-body ctx node contract)
                    has-rule? (special ctx node head)
                    spec-contract (check-spec-macro ctx node contract-sym spec-contract args)
                    ;; a macro of Clojure itself without a rule yet: its body is not checked
                    (and (not own?) (not extern?) (core/stdlib-macro? core-sym)) t/unknown
                    :else ::none)
            cname (when (= ::none r) (java-static? ctx sym))]
        (cond
          (not= ::none r) r
          (and (nil? (namespace sym)) (str/starts-with? n ".-")) (instance-field-call ctx node (subs n 2))
          (and (nil? (namespace sym)) (str/starts-with? n ".") (> (count n) 1) (not= ".." n))
          (let [[target & arg-nodes] args]
            (method-call ctx node target h (subs n 1) arg-nodes :no-zero-arg-method))
          (and (nil? (namespace sym)) (str/ends-with? n ".") (> (count n) 1))
          (constructor-call ctx node (t/resolve-class (:imports file) (symbol (subs n 0 (dec (count n))))) args)
          cname (if (and (> (count n) 1) (str/starts-with? n "."))
                  (qualified-instance-call ctx node h cname args)
                  (java-call ctx node h cname n true args :no-methods))
          (contains? (:macros (:env ctx)) (:sym (resolve-var (assoc ctx :quiet true) sym h)))
          (do (report! ctx h "TSP7031" (str "macro " sym " is not described: its body is not checked") :severity :warning)
              t/unknown)
          :else
          (let [{:keys [type]} (read-var ctx sym h :call)]
            (if type
              (call-fn ctx node (str sym) type args)
              (check-unknown-call ctx args))))))))

(defn- literal-element-type [ctx types]
  (apply t/union (if (:preserve-literals? ctx) types (map widen types))))

(defn- infer* [ctx node]
  (case (:op node)
    :str (t/lit (:val node))
    :char (t/class-type "java.lang.Character")
    :num (t/lit (:val node))
    :kw (t/lit (:val node))
    :bool (t/lit (:val node))
    :nil t/nil-t
    :var (var-reference ctx (:children node))
    :regex (t/class-type "java.util.regex.Pattern")
    :sym (let [s (:val node)
               cname (java-static? ctx s)]
           (cond (contains? (:locals ctx) s) (get-in ctx [:locals s])
                 cname (if-let [class-name (java/field-class cname (name s) true)]
                         (java-field-type class-name)
                         t/unknown)
                 (and (nil? (namespace s)) (not (t/primitives (name s)))
                      (t/resolve-class (:imports (:file ctx)) s)) t/unknown
                 :else (or (:type (read-var ctx s node :value)) t/unknown)))
    :vector (let [items (mapv #(dissoc (infer ctx %) :fresh) (:children node))]
              (t/tuple items (literal-element-type ctx items)))
    :set (let [items (:children node)
               size (count items)
               minimum (if (every? literal-node? items)
                         (count (set (map :val items)))
                         (if (zero? size) 0 1))]
           (length/with-bounds {:t :coll :kind :set :of (literal-element-type ctx (map #(infer ctx %) items))}
                               minimum size))
    :map (let [children (:children node)
               pairs (partition 2 children)
               static-keys (map static-map-key (map first pairs))]
           (when (:missing-directive-target (peek children))
             (literal-directives ctx (peek children)))
           (if (every? some? static-keys)
             {:t :map :fresh true :inferred true :complete-keys? true :opt {}
              :req (into (array-map)
                         (map (fn [[k v] kk]
                           (let [_ (when (some literal-directive? (:directives k)) (infer ctx k))
                                 vty (infer ctx v)]
                             (check-key-value! ctx kk v vty)
                             (when (and (qualified-keyword? kk) (not (get-in (:env ctx) [:specs kk]))
                                        (contains? (:key-nss (:env ctx)) (namespace kk))
                                        (or (:strict (:opts ctx)) (similar kk (keys (:specs (:env ctx))))))
                               (report! ctx k "TSP2304" (str "no spec for " kk (hint-suffix kk (keys (:specs (:env ctx)))))))
                             ;; a wrong value is reported once; after that the key has the type of its spec
                             [kk (if (and (qualified-keyword? kk) (get-in (:env ctx) [:specs kk])
                                          (not (t/sub? (:env ctx) (eff ctx vty) (t/ref-t kk))))
                                   (t/ref-t kk)
                                   (dissoc vty :fresh))]))
                              pairs static-keys))}
             (do (run! #(infer ctx %) (:children node)) (t/prim :map))))
    :quote (quoted-type ctx (first (:children node)))
    :fn-lit (infer-callback ctx node nil)
    :list (infer-list ctx node)
    t/unknown))

(defn infer
  ([ctx node] (infer ctx node nil))
  ([ctx node callback-arity]
   (let [dirs (:directives node)
         find-d (fn [h] (first (filter #(= h (directive-head %)) dirs)))
         literal-dirs (literal-directives ctx node)
         satisfies (get literal-dirs 't/satisfies)
         before-trust (when satisfies @*trust*)
         ctx (if (and (#{:map :vector :set} (:op node))
                      (or (:preserve-literals? ctx) (get literal-dirs 't/const)))
               (assoc ctx :preserve-literals? true)
               (dissoc ctx :preserve-literals?))
         ty (if-let [callback (deferred-callback ctx node)]
              (force-callback ctx callback callback-arity)
              (if callback-arity (infer-callback ctx node callback-arity) (infer* ctx node)))
         ty (if-let [h (and (not= :sym (:op node)) (env/hint-type (:file ctx) node))]
              (do (check-hint! ctx node ty "type of the expression")
                  (if-let [why (:null-return (meta ty))]
                    (with-meta (t/union (dissoc h :platform) t/nil-t) {:null-return why})
                    h))
              ty)
         ty (if-let [cast (find-d 't/cast)]
              (do (record-escape! ctx cast :cast)
                  (t/with-assumption (parse-directive-type ctx cast) :cast))
              (if-let [any (find-d 't/any)]
                (do (record-escape! ctx any :any) (t/with-assumption t/any :any))
                ty))]
     (when satisfies (check-satisfies! ctx node satisfies ty before-trust))
     (record! ctx node ty))))

;; ---- entry points ----

(def ^:private known-diagnostic-codes
  #{"TSP250-01" "TSP250-02" "TSP250-03" "TSP270-01" "TSP270-02" "TSP270-03" "TSP280-01" "TSP280-02"
    "TSP295-01" "TSP295-02" "TSP330-01" "TSP330-02" "TSP410-01" "TSP430-01" "TSP430-02" "TSP430-03"
    "TSP2304" "TSP2305" "TSP2322" "TSP2339" "TSP2345" "TSP2353"
    "TSP2531" "TSP2554" "TSP2578" "TSP2678" "TSP2741" "TSP2790"
    "TSP7010" "TSP7020" "TSP7021" "TSP7023" "TSP7025" "TSP7031" "TSP7032" "TSP721-01" "TSP722-01"
    "TSP8001" "TSP9001" "TSP9002" "TSP9003" "TSP9004" "TSP830-01" "TSP210-01"})

(defn- directive-children
  "Directive scope follows source code, not quoted data or opaque declarations.
  Keep directives on the boundary form itself, but do not inspect its contents.
  Ordinary collection expressions and function binding forms remain traversable."
  [file node]
  (when-not (or (#{:quote :var} (:op node))
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
    (.formatHex (java.util.HexFormat/of) digest 0 16)))

(defn- filter-warnings [diagnostics rules]
  (if (and (map? rules) (seq rules))
    (reduce (fn [[visible suppressed] {:keys [code severity reasons] :as diagnostic}]
              (let [rule (when (= :warning severity) (get rules (symbol code)))]
                (if (or (= :all rule)
                        (and (set? rule) (seq reasons) (every? rule reasons)))
                  [visible (inc suppressed)]
                  [(conj visible diagnostic) suppressed])))
            [[] 0] diagnostics)
    [diagnostics 0]))

(defn check-files
  "Checks parsed files. With ::diagnostics-only, omit expression types and the signature index."
  [files opts]
  (let [[env0 env-diags] (env/build files (:extern-contracts opts))
        compiled (macros/compile-contracts
                  (get opts :macro-contracts {}) env0
                  #(not= (get-method special %) (get-method special ::none)))
        env-diags (concat env-diags (:diagnostics compiled))
        guard-env (assoc env0 :macro-contracts (:contracts compiled)
                             :spec-macro-contracts (macros/infer-spec-contracts env0 files)
                             :strict-nil (boolean (:strict-nil opts))
                             :nss (set (map :ns files)))
        guard-result (compile-guards guard-env opts)
        env (assoc guard-env :guards (:guards guard-result))
        guard-trust (count (filter #(= :trusted (:mode %)) (vals (:guards env))))]
    (binding [*diags* (atom (vec (map #(merge {:severity :error} %) (concat env-diags (:diagnostics guard-result)))))
              *types* (when-not (::diagnostics-only opts) (atom []))
              *var-uses* (when (:track-uses opts) (atom #{}))
              *spec-uses* (when (:track-uses opts) (atom #{}))
              *ignores* (atom [])
              *trust* (atom (cond-> {:cast 0 :any 0 :ignore 0} (pos? guard-trust) (assoc :guard guard-trust)))
              *rets* (atom {})]
      (doseq [f files]
        (binding [t/*cur-ns* (:ns f)]
          (let [ctx {:env env :file f :opts opts :locals {} :load-time? true}]
            (doseq [form (:forms f)] (infer ctx form)))))
      (let [{:keys [valid invalid] ignore-count :count} (collect-ignores files)
            ;; Inner ranges win; equal ranges retain source directive order.
            ignores (sort-by (fn [i] [(- (or (:row (:pos i)) 0)) (- (or (:col (:pos i)) 0))
                                     (:end-row (:pos i)) (:end-col (:pos i))]) valid)
            _ (swap! *trust* assoc :ignore ignore-count)
            hit (fn [d] (first (filter #(and (= (:file %) (:file d))
                                             (or (nil? (:code %)) (= (:code %) (:code d)))
                                             (inside? (:pos d) (:pos %))) ignores)))
            all (distinct @*diags*)
            used (set (keep hit all))
            unused (for [i ignores :when (not (used i))]
                     {:code "TSP2578" :severity :warning :file (:file i) :pos (:at i) :message "unused t/ignore: nothing to ignore here"})
            levels (:levels opts)
            report (->> (concat (remove hit all) invalid unused)
                        (map #(if-let [l (get levels (symbol (:code %)))] (assoc % :severity l) %))
                        (remove #(= :off (:severity %))))
            [visible suppressed] (filter-warnings report (:suppress-warnings opts))
            {::keys [escape-sites] :as trust} @*trust*
            trust (assoc (dissoc trust ::escape-sites)
                         :cast (count (:cast escape-sites))
                         :any (count (:any escape-sites)))
            ds (->> visible
                    (map #(assoc % :id (diag-id %)))
                    (sort-by (juxt :file #(:row (:pos %)) #(:col (:pos %)) :code)))]
        (cond-> {:diagnostics (vec ds) :types (if *types* @*types* []) :trust trust :env env
                 :uses (vec (sort-by (juxt :file (comp :row :pos) (comp :col :pos) :var :kind) (when *var-uses* @*var-uses*)))
                 :spec-uses (vec (sort-by (juxt :file (comp :row :pos) (comp :col :pos) :spec) (when *spec-uses* @*spec-uses*)))
                 :sigs (if (::diagnostics-only opts)
                         {}
                         (into {} (for [sym (keys (:vars env))]
                                    [sym (var-type {:env env :opts opts} sym)])))}
          (pos? suppressed) (assoc :suppressed-warnings suppressed))))))
