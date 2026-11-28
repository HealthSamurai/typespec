(ns typespec.env
  "Collects declarations from source text: s/def, s/fdef, {:t/sig ...}, type hints. No code is loaded."
  (:require [typespec.parse :as parse]
            [typespec.types :as t]
            [typespec.recursive-types :as recursive]
            [typespec.type-operators :as operators]
            [typespec.externs :as externs]))

(defn resolve-sym
  "Fully qualified symbol for a symbol used in `file`. Unqualified symbols stay unqualified."
  [file sym]
  (if-let [n (namespace sym)]
    (if-let [lib (get (:aliases file) (symbol n))] (symbol (str lib) (name sym)) sym)
    (get (:refers file) sym sym)))

(defn head [file node]
  (when (= :list (:op node))
    (let [h (first (:children node))]
      (when (= :sym (:op h))
        (let [s (resolve-sym file (:val h))]
          (if (= "clojure.core" (namespace s)) (symbol (name s)) s))))))

(defn- array-descriptor-node? [node]
  (and (= :str (:op node)) (.startsWith ^String (:val node) "[")))

(defn attr-get [attr k]
  (some (fn [[kn vn]] (when (= k (:val kn)) vn)) (partition 2 (:children attr))))

(defn tag-node
  "The supported type hint from reader metadata, with outer metadata taking precedence."
  [node]
  (let [tag (some (fn [m]
                    (case (:op m)
                      (:sym :str) m
                      :map (attr-get m :tag)
                      :kw (when (= :tag (:val m)) m)
                      nil))
                  (:meta node))]
    (when (or (= :sym (:op tag)) (array-descriptor-node? tag))
      tag)))

(def ^:private array-hint-classes
  {'booleans "[Z" 'bytes "[B" 'chars "[C" 'shorts "[S" 'ints "[I"
   'longs "[J" 'floats "[F" 'doubles "[D" 'objects "[Ljava.lang.Object;"})

(defn hint-class [file tag]
  (or (array-hint-classes (:val tag))
      (when (and (array-descriptor-node? tag) (t/load-class (:val tag))) (:val tag))
      (when (= :sym (:op tag)) (t/resolve-class (:imports file) (:val tag)))))

(defn hint-type [file node]
  (when-let [tag (tag-node node)]
    (when-let [c (hint-class file tag)]
      (t/platform (t/class-type c) (str (:val tag))))))

(defn return-hint [file name params]
  (if-let [type (hint-type file params)]
    {:tag (tag-node params) :type type}
    (let [tag (tag-node name)]
      ;; A primitive tag on the name does not define the arity's return.
      (when-not (some-> tag :val str t/primitives)
        (when-let [type (hint-type file name)]
          {:tag tag :type type})))))

(defn- function-arities [forms]
  (let [arity (fn [[params & body]] {:params params :body (vec body)})]
    (if (= :vector (:op (first forms)))
      [(arity forms)]
      (vec (for [form forms
                 :when (and (= :list (:op form)) (= :vector (:op (first (:children form)))))]
             (arity (:children form)))))))

(defn defn-parts [node]
  (let [[h nm & more] (:children node)
        [doc more] (if (= :str (:op (first more))) [(first more) (rest more)] [nil more])
        [attr more] (if (= :map (:op (first more))) [(first more) (rest more)] [nil more])]
    {:name nm
     :private? (or (= 'defn- (:val h)) (boolean (some #(= :private (:val %)) (:meta nm))))
     :attr attr
     :arities (function-arities more)}))

(defn defmethod-parts [node]
  (let [[_ target dispatch & forms] (:children node)
        local-name (when (= :sym (:op (first forms))) (first forms))]
    {:target target :dispatch dispatch :local-name local-name
     :arities (function-arities (if local-name (rest forms) forms))}))

(defn split-params
  "{:fixed [nodes] :rest node-or-nil} of a parameter vector."
  [params]
  (let [[fixed more] (split-with #(not (and (= :sym (:op %)) (= '& (:val %)))) (:children params))]
    {:fixed (vec fixed) :rest (second more)}))

(defn- var-name [file node]
  (when (and (= :sym (:op node)) (simple-symbol? (:val node)))
    (symbol (str (:ns file)) (name (:val node)))))

(defn- literal-var-name [file node]
  (let [symbol-node (case (:op node)
                      :var (first (:children node))
                      :list (when (= 'var (head file node)) (second (:children node)))
                      nil)]
    (when (= :sym (:op symbol-node))
      (let [sym (resolve-sym file (:val symbol-node))]
        (if (namespace sym) sym (var-name file symbol-node))))))

(defn- dynamic-var? [node]
  (some #(or (= :dynamic (:val %)) (:dynamic (parse/node->data %))) (:meta node)))

(defn- record-factories [file form]
  (let [[_ target fields] (:children form)]
    (when (and (var-name file target) (= :vector (:op fields))
               (every? #(and (= :sym (:op %)) (simple-symbol? (:val %))) (:children fields)))
      (for [[prefix arity] [["->" (count (:children fields))] ["map->" 1]]]
        [(symbol (str (:ns file)) (str prefix (:val target)))
         {:file file :pos (:pos target) :factory-arity arity}]))))

(defn definition-initializer [form]
  (last (drop 2 (:children form))))

(defn initialized-var
  "The name initialized by a direct def/defonce, including an explicit nil initializer."
  [file form]
  (let [[_ target] (:children form)]
    (when (and (#{'def 'defonce} (head file form)) (definition-initializer form))
      (var-name file target))))

(defn- function-bodies [parts]
  (let [parts (if (= :sym (:op (first parts))) (rest parts) parts)]
    (if (= :vector (:op (first parts)))
      (rest parts)
      (mapcat (fn [arity]
                (when (= :vector (:op (first (:children arity))))
                  (rest (:children arity))))
              parts))))

(defn- binding-values [node]
  (when (= :vector (:op node))
    (take-nth 2 (rest (:children node)))))

(defn- declaration-forms [file]
  (map (fn [form]
         (loop [form form]
           (if-let [definition (when (= 'clojure.test/with-test (head file form))
                                 (second (:children form)))]
             (recur definition)
             form)))
       (:forms file)))

(defn- expression-children [form operator]
  (let [[_ a & more] (:children form)]
    (case (:op form)
      (:vector :map :set) (:children form)
      :fn-lit [(assoc form :op :list)]
      :list
      (case operator
        (defn defn-) (mapcat :body (:arities (defn-parts form)))
        defmethod (mapcat :body (:arities (defmethod-parts form)))
        (fn fn*) (function-bodies (rest (:children form)))
        (def defonce) (when-let [init (definition-initializer form)] [init])
        (let let* loop loop* binding with-open if-let if-some when-let when-some)
        (concat (binding-values a) more)
        time (when (and a (empty? more)) [a])
        case (concat [a] (map second (partition 2 more))
                     (when (odd? (count more)) [(last more)]))
        (do if if-not when when-not and or cond condp try finally throw future lazy-seq clojure.test/with-test)
        (rest (:children form))
        catch (drop 3 (:children form))
        nil)
      nil)))

(defn- literal-constant? [node]
  (and (empty? (:directives node)) (not (tag-node node))
       (case (:op node)
         (:kw :str :num :bool :char :nil) true
         (:map :vector :set) (every? literal-constant? (:children node))
         false)))

(defn- constant-initializer [file target initializer]
  (let [flag (some (fn [metadata]
                     (if (and (= :kw (:op metadata)) (= :const (:val metadata)))
                       {:val true}
                       (attr-get metadata :const)))
                   (:meta target))]
    (when (and (true? (:val flag)) (literal-constant? initializer))
      (let [value (:val initializer)]
        (case (:op initializer)
          (:kw :str :num :bool) {:type (t/lit value) :class (.getName ^Class (class value))}
          :char {:type (t/class-type "java.lang.Character") :class "java.lang.Character"}
          :nil {:type t/nil-t}
          {:node initializer :file file
           :class (case (:op initializer)
                    :map "java.util.Map"
                    :vector "java.util.List"
                    :set "java.util.Set")})))))

(defn- var-declarations [files macros]
  (letfn [(collect [by-name file form at-load?]
            (let [[head-node target] (:children form)
                  name (var-name file target)
                  operator (when-not (contains? macros (var-name file head-node)) (head file form))
                  factories (when (and at-load? (= 'defrecord operator)) (record-factories file form))
                  data? (or (= 'def operator)
                            (and (#{'defonce 'declare} operator)
                                 (or at-load? (namespace (:val head-node)))))
                  names (cond
                          (seq factories) (map first factories)
                          (and data? (= 'declare operator)) (keep #(var-name file %) (rest (:children form)))
                          (and name (or data? (and at-load? (#{'defn 'defn-} operator)))) [name])
                  data-initializer (when (and at-load? (#{'def 'defonce} operator))
                                     (initialized-var file form))
                  value (when (#{'def 'defonce} operator) (definition-initializer form))
                  later-write (cond
                                (= 'alter-var-root operator) (literal-var-name file target)
                                (and (= 'def operator) value (not at-load?)) name)
                  init? (and at-load?
                             (or (seq factories) (#{'defn 'defn-} operator) data-initializer))
                  hint (when data-initializer (hint-type file target))
                  constant (when data-initializer (constant-initializer file target value))
                  regex-initializer? (and data-initializer
                                          (or (= :regex (:op value)) (= 're-pattern (head file value)))
                                          (not (dynamic-var? target)) (empty? (:directives value))
                                          (not (tag-node value)))
                  initializer (cond-> {:file (:file file) :pos (:pos form)}
                                hint (assoc :hint {:type hint :class (hint-class file (tag-node target))})
                                constant (assoc :constant constant)
                                regex-initializer? (assoc :regex-initializer {:node value :file file}))
                  by-name (cond-> by-name later-write (assoc-in [later-write :root-may-change?] true))
                  by-name (reduce (fn [acc sym]
                                    (cond-> acc
                                      data? (assoc-in [sym :tracks-root?] true)
                                      init? (update-in [sym :initializers] (fnil conj [])
                                                       initializer)))
                                  by-name names)]
              (reduce (fn [acc child]
                        (collect acc file child
                                 (and at-load?
                                      (or (#{'do 'time} operator)
                                          (and (= 'clojure.test/with-test operator)
                                               (identical? child target))))))
                      by-name (expression-children form operator))))]
    (into {}
          (filter (fn [[_ entry]] (or (:tracks-root? entry) (seq (:initializers entry)))))
          (reduce (fn [acc file] (reduce #(collect %1 file %2 true) acc (:forms file))) {} files))))

(defn unique-var-initializer [env sym]
  (let [[initializer & others] (get-in env [:var-declarations sym :initializers])]
    (when-not (seq others) initializer)))

(declare ^:private protocol-methods)

(defn namespace-vars
  "Known names and Var initialization sites. Defs inside opaque macro bodies are excluded."
  [files macros]
  (let [declarations (var-declarations files macros)
        top-level (reduce (fn [acc file]
                            (update acc (:ns file) (fnil into #{})
                                    (mapcat (fn [form]
                                              (let [[h nm & forms] (:children form)]
                                                (when (and (= :list (:op form)) (= :sym (:op h)) (= :sym (:op nm))
                                                           (.startsWith (name (:val h)) "def"))
                                                  (cons (:val nm)
                                                        (when (= 'defprotocol (head file form))
                                                          (keys (protocol-methods file forms)))))))
                                            (declaration-forms file))))
                          {} files)]
    {:var-declarations declarations
     :ns-defs (reduce (fn [acc sym]
                       (update acc (symbol (namespace sym)) (fnil conj #{}) (symbol (name sym))))
                     top-level (keys declarations))}))

(defn type-ctx [file fail]
  (assoc (select-keys file [:ns :aliases :refers :imports :refer-clojure :defined-symbols]) :fail fail))

(defn- map-pattern-keys
  "[[key required?] ...] of a map destructuring pattern, without positions."
  [pattern]
  (let [pairs (partition 2 (:children pattern))
        ;; with :missing the function handles absent keys itself, so callers may omit them
        missing? (some (fn [[k _]] (= :missing (:val k))) pairs)]
    (for [[k v] pairs
          :when (and (= :kw (:op k)) (#{"keys" "keys!"} (name (:val k))) (= :vector (:op v)))
          s (:children v) :when (#{:sym :kw} (:op s))
          :let [nsp (or (namespace (:val k)) (namespace (:val s)))]]
      [(keyword nsp (name (:val s))) (and (= "keys!" (name (:val k))) (not missing?))])))

(defn pre-conditions
  "Test nodes of {:pre [...]} at the start of a function body."
  [body]
  (let [m (first body)]
    (when (and (= :map (:op m)) (next body))
      (some (fn [[k v]] (when (and (= :pre (:val k)) (= :vector (:op v))) (:children v)))
            (partition 2 (:children m))))))

(def ^:private pre-preds
  '{string? :string int? :int integer? :int number? :number keyword? :keyword boolean? :boolean
    map? :map vector? :vector uuid? :uuid})

(defn- precondition-predicate-type [head]
  (case head
    some? t/some-t
    nil? t/nil-t
    (when-let [primitive (pre-preds head)] (t/prim primitive))))

(defn- precondition-head [file test]
  (let [operator (first (:children test))
        sym (:val operator)]
    (when (and (= :list (:op test)) (= :sym (:op operator))
               (empty? (:directives test)) (empty? (:directives operator))
               (not (contains? (:locals file) sym))
               (not (contains? (:defined-symbols file) sym)))
      (or (t/core-name file sym) (let [resolved (resolve-sym file sym)]
                                  (when (namespace resolved) resolved))))))

(defn- parameter-bindings [pattern]
  (case (:op pattern)
    :sym (when (not= '& (:val pattern)) [(:val pattern)])
    :vector (mapcat parameter-bindings (:children pattern))
    :map (mapcat (fn [[k v]]
                   (case (when (= :kw (:op k)) (name (:val k)))
                     "or" nil
                     "as" (parameter-bindings v)
                     ("keys" "strs" "syms")
                     (for [{:keys [op val]} (:children v) :when (#{:sym :kw} op)]
                       (symbol (name val)))
                     (parameter-bindings k)))
                 (partition 2 (:children pattern)))
    nil))

(defn- precondition-class [file node]
  (let [sym (:val node)]
    (when (and (= :sym (:op node))
               (not (contains? (:locals file) sym))
               (not (contains? (:defined-symbols file) sym))
               (not (contains? (:refers file) sym)))
      (when-let [name (t/resolve-class (:imports file) sym)]
        (when-let [^Class class (t/load-class name)]
          (when-not (.isPrimitive class)
            (let [ty (t/class-type name)]
              (cond
                (= :any (:t ty)) t/some-t
                (#{:int :double} (:name ty)) nil
                :else ty))))))))

(declare condition-facts)

(defn- disjunction-fact [file test args]
  (let [branches (mapv #(condition-facts file %) args)]
    (when (and (seq branches) (every? #(= 1 (bounded-count 2 %)) branches))
      (let [facts (mapv first branches)
            param (ffirst facts)]
        (when (every? #(= param (first %)) facts)
          [[param (apply t/union (map second facts)) test]])))))

(defn- negated-nil-fact [file test predicate]
  (let [[_ arg & more] (:children predicate)
        complement (case (precondition-head file predicate)
                     nil? t/some-t
                     some? t/nil-t
                     nil)]
    (when (and complement (= :sym (:op arg)) (empty? more))
      [[(:val arg) complement test]])))

(defn- condition-facts [file test]
  (let [head (precondition-head file test)
        args (rest (:children test))
        [a b form & more] args
        type (when (nil? b) (precondition-predicate-type head))
        class (when (and (= 'instance? head) b (nil? form)) (precondition-class file a))]
    (cond
      (= 'and head) (mapcat #(condition-facts file %) args)
      (= 'or head) (disjunction-fact file test args)
      (and (= 'not head) a (nil? b)) (negated-nil-fact file test a)
      (and type (= :sym (:op a))) [[(:val a) type test]]
      (and class (= :sym (:op b))) [[(:val b) class test]]
      (and (= 'clojure.spec.alpha/valid? head) (= :kw (:op a)) (= :sym (:op b)) (empty? more))
      [[(:val b) (t/ref-t (:val a)) test]])))

(defn pre-facts
  "Facts about fixed parameters with their source conditions from supported :pre forms."
  [file params body]
  (when-let [tests (seq (pre-conditions body))]
    (let [file (assoc file :locals (set (parameter-bindings params)))
          fixed (into #{} (keep #(when (= :sym (:op %)) (:val %))) (:fixed (split-params params)))]
      (filter #(fixed (first %)) (mapcat #(condition-facts file %) tests)))))

(defn- precondition-domain [ty]
  (case (:t ty)
    :nil :nil
    :lit (recur (t/lit-prim (:val ty)))
    :prim (case (:name ty)
            (:int :double :number) :number
            (:string :keyword :symbol :boolean :uuid :inst) (:name ty)
            nil)
    nil))

(defn- incompatible-preconditions? [env left right]
  (every? true?
          (for [left (t/members env left)
                right (t/members env right)]
            (cond
              (= :nil (:t left)) (not (t/may-be-nil-leaf? right))
              (= :nil (:t right)) (not (t/may-be-nil-leaf? left))
              :else (let [a (precondition-domain left) b (precondition-domain right)]
                      (boolean (and a b (not= a b))))))))

(defn- intersect-precondition [env previous type]
  (cond
    ;; any? permits every input, even though ordinary subtyping treats it as a bypass.
    (some #(= :any (:t %)) (t/members env type)) previous
    (t/sub? env type previous) type

    (or (= :union (:t previous)) (= :union (:t type)))
    (let [[alternatives other] (if (= :union (:t previous))
                                 [(:types previous) type]
                                 [(:types type) previous])
          possible (apply t/union (remove #(incompatible-preconditions? env % other) alternatives))]
      (if (t/sub? env possible previous) possible previous))

    (incompatible-preconditions? env previous type) t/never
    :else previous))

(defn- pre-types [env file params body conflict!]
  (reduce (fn [facts [param type test]]
            (let [previous (get facts param)
                  next-type (cond
                              (nil? previous) type
                              (= t/never previous) previous
                              :else (intersect-precondition env previous type))]
              (when (and previous (= t/never next-type) (not= t/never previous))
                (conflict! test param previous type))
              (assoc facts param next-type)))
          {} (pre-facts file params body)))

(defn pattern-type
  "Type a parameter must have, as far as its hint or destructuring pattern says."
  [env file node]
  (or (hint-type file node)
      (when (= :map (:op node))
        (let [ks (map-pattern-keys node)
              ty (fn [k] (if (get-in env [:specs k]) (t/ref-t k) t/unknown))]
          (when (seq ks)
            {:t :map :inferred true ; a lower bound read from the pattern, not a declaration
             :req (into (array-map) (for [[k req?] ks :when req?] [k (ty k)]))
             :opt (into (array-map) (for [[k req?] ks :when (not req?)] [k (ty k)]))})))
      t/unknown))

(defn- hints-sig [env file parts conflict!]
  {:t :fn
   :arities (vec (for [{:keys [params body]} (:arities parts)
                       :let [{:keys [fixed rest]} (split-params params)
                             pre (pre-types env file params body conflict!)]]
                   (cond-> {:params (mapv #(or (when (= :sym (:op %)) (get pre (:val %)))
                                               (pattern-type env file %))
                                          fixed)
                            :ret (or (:type (return-hint file (:name parts) params)) t/unknown)}
                     (= :map (:op rest)) (assoc :kwargs (pattern-type env file rest))
                     (and rest (not= :map (:op rest))) (assoc :rest t/unknown))))})

(defn- note [diags file node code msg]
  (swap! diags conj {:code code :file (:file file) :pos (:pos node) :message msg}))

(defn- operator-errors [diags file pos]
  (fn [code form message]
    (note diags file {:pos (or (:typespec/pos (meta form)) pos)} code message)))

(defn- protocol-methods [file forms]
  (into {}
        (for [form forms
              :when (= :list (:op form))
              :let [[method & parts] (:children form)]
              :when (and (= :sym (:op method)) (simple-symbol? (:val method)))
              :let [arities (into (sorted-set)
                                  (for [params parts :when (= :vector (:op params))
                                        :let [{:keys [fixed rest]} (split-params params)]
                                        :when (and (seq fixed) (nil? rest))]
                                    (count fixed)))]]
          [(:val method) {:var (symbol (str (:ns file)) (name (:val method)))
                          :file (:file file) :pos (:pos method) :arities arities}])))

(defn- dispatch-projection [file dispatch]
  (case (:op dispatch)
    :kw {:index 0 :key (:val dispatch) :arity 1}
    :list (let [[head params body & extra] (:children dispatch)
                [key target & args] (:children body)
                names (mapv :val (:children params))]
            (when (and (#{'fn 'fn*} (t/core-name file (:val head)))
                       (= :vector (:op params)) (every? simple-symbol? names)
                       (not-any? #{'&} names) (= (count names) (count (distinct names)))
                       (empty? extra) (= :list (:op body)) (= :kw (:op key))
                       (= :sym (:op target)) (empty? args))
              (when-let [index (first (keep-indexed #(when (= %2 (:val target)) %1) names))]
                {:index index :key (:val key) :arity (count names)})))
    nil))

(defn- class-dispatch-projection [file dispatch]
  (when (and (empty? (:directives dispatch)) (not (tag-node dispatch)))
    (case (:op dispatch)
      :sym (when (= 'class (t/core-name file (:val dispatch)))
             {:kind :class :index 0 :arity 1})
      :list (let [[head f key & more] (:children dispatch)]
              (when (and (= 'comp (t/core-name file (:val head)))
                         (= :sym (:op f)) (= 'class (t/core-name file (:val f)))
                         (= :kw (:op key)) (empty? more)
                         (every? #(and (empty? (:directives %)) (not (tag-node %))) [head f key]))
                {:kind :class :index 0 :key (:val key) :arity 1}))
      nil)))

(defn- multimethod-dispatch [file forms]
  (let [forms (if (= :str (:op (first forms))) (rest forms) forms)
        forms (if (= :map (:op (first forms))) (rest forms) forms)
        [dispatch & options] forms
        [option fallback & extra] options]
    ;; Strings do not participate in Clojure's built-in derive relations.
    ;; Only use that fact with the default hierarchy and a known fallback.
    (when (or (empty? options)
              (and (= :kw (:op option)) (= :default (:val option))
                   (#{:kw :str :nil :num :bool :char} (:op fallback)) (empty? extra)))
      (when-let [projection (or (class-dispatch-projection file dispatch)
                               (dispatch-projection file dispatch))]
        (assoc projection :default (if (empty? options) :default (:val fallback)))))))

(defn- register-spec [acc bucket sym entry diags]
  (when-let [previous (get-in acc [bucket sym])]
    (when (and (not= (:file entry) (:file previous))
               (or (not (t/alpha-equivalent? (:type entry) (:type previous)))
                   (not= (some-> previous :type meta ::t/spec-tags)
                         (some-> entry :type meta ::t/spec-tags))))
      (let [[earlier later] (sort-by :file [previous entry])]
        (note diags later later "TSP722-01"
              (str "conflicting spec declarations for " sym ", also declared in " (:file earlier))))))
  (assoc-in acc [bucket sym] entry))

(defn- spec-definition [file form]
  (let [[_ k body] (:children form)]
    (when (and (= :kw (:op k)) body)
      [(:val k) {:file (:file file) :pos (:pos k)
                 :spec-form (parse/node->data body)
                 :spec-context (assoc (dissoc (type-ctx file nil) :fail) :spec? true)}])))

(defn- collect-file [acc file spec-definitions diags]
  (reduce
   (fn [acc form]
     (let [h (head file form)
           fail (fn [n] (fn [f] (note diags file n "TSP9001" (str "not a type: " (pr-str f)))))
           opaque (fn [n] (fn [f] (swap! diags conj {:code "TSP9002" :severity :warning :file (:file file) :pos (:pos n)
                                                     :message (str "this spec is opaque to the checker and is treated as t/unknown: "
                                                                   (pr-str f))})))
           qualify (fn [s] (let [r (resolve-sym file s)] (if (namespace r) r (symbol (str (:ns file)) (name r)))))]
       (cond
         (= 'clojure.spec.alpha/def h)
         (if-let [[k {:keys [spec-form spec-context] :as entry}] (spec-definition file form)]
           (let [body (nth (:children form) 2)
                 ctx (assoc spec-context :spec-definitions spec-definitions :fail (opaque body))]
             (register-spec acc :specs k (assoc entry :type (t/parse-type ctx spec-form)) diags))
           acc)

         (= 'clojure.spec.alpha/fdef h)
         (let [[_ nm & opts] (:children form)
               options (t/spec-option-map (map parse/node->data opts))
               {:keys [args ret]} options
               ctx (assoc (type-ctx file (opaque form)) :spec-definitions spec-definitions :spec? true)
               result (if ret (t/parse-type ctx ret) t/unknown)
               ars (if options
                     (t/spec-args->arities ctx args result)
                     (do ((:fail ctx) (parse/node->data form)) nil))
               type (cond
                      ars {:t :fn :arities ars}
                      (not= t/unknown result) {:t :fn :ret result})
               opaque-args? (and (some? args) (nil? ars))]
           (if (= :sym (:op nm))
             (let [sym (qualify (:val nm))
                   location {:file (:file file) :pos (:pos form)}
                   opaque? (or (nil? type) opaque-args?)]
               (cond-> acc
                 type (register-spec :fdefs sym (assoc location :type type :opaque-args? opaque-args?) diags)
                 opaque? (assoc-in [:opaque-fdefs sym] location)
                 (not opaque?) (update :opaque-fdefs dissoc sym)))
             acc))

         (= 'defprotocol h)
         (let [[_ nm & forms] (:children form)]
           (if (and (= :sym (:op nm)) (simple-symbol? (:val nm)))
             (assoc-in acc [:protocols (symbol (str (:ns file)) (name (:val nm)))] (protocol-methods file forms))
             acc))

         (= 'defrecord (t/core-name file (:val (first (:children form)))))
         (reduce (fn [acc [sym factory]]
                   (let [prior (get-in acc [:defns sym])
                         prior-defn (if (:parts prior) prior (:prior-defn prior))]
                     (-> acc
                         (update-in [:root-definitions sym] (fnil inc 0))
                         (assoc-in [:defns sym] (cond-> factory prior-defn (assoc :prior-defn prior-defn))))))
                 acc (record-factories file form))

         (= 'defmulti (t/core-name file (:val (first (:children form)))))
         (let [[_ target & forms] (:children form)]
           (if-let [sym (var-name file target)]
             (update-in acc [:multimethod-dispatches sym] (fnil conj []) (multimethod-dispatch file forms))
             acc))

         (#{'defn 'defn-} h)
         (let [parts (defn-parts form)
               sig-node (some-> (:attr parts) (attr-get :t/sig))
               sym (qualify (:val (:name parts)))]
           (-> acc
               (update-in [:root-definitions sym] (fnil inc 0))
               (assoc-in [:defns sym]
                         {:file file :parts parts :pos (:pos (:name parts))
                          :definition form
                          :sig-pos (:pos sig-node)
                          :sig (when sig-node
                                 (t/parse-type (assoc (type-ctx file (fail sig-node))
                                                      :operator-error (operator-errors diags file (:pos sig-node)))
                                               (parse/node->data sig-node true)))})))

         (and (#{'def 'defonce} h) (= :sym (:op (second (:children form)))))
         (let [target (second (:children form))
               sym (qualify (:val target))
               init (definition-initializer form)]
           (cond-> (update-in acc [:root-definitions sym] (fnil inc 0))
             (and (= 'def h) init (contains? (get-in acc [:defns sym]) :factory-arity))
             (update :defns dissoc sym)
             (and (= :list (:op init)) (= 'atom (t/core-name file (:val (first (:children init)))))
                  (not (dynamic-var? target)))
             (assoc-in [:atom-initializers sym] {:file file :node init})))

         :else acc)))
   acc (declaration-forms file)))

(defn- protocol-vars [protocols fdefs diags]
  (into {}
        (for [[protocol methods] (sort-by key protocols)
              [_ {:keys [var file pos arities]}] methods
              :let [fd (get fdefs var)
                    contracts (if-let [ret (get-in fd [:type :ret])]
                                (for [n arities] {:params (vec (repeat n t/unknown)) :ret ret})
                                (get-in fd [:type :arities]))
                    valid? (fn [arity]
                             (and (contains? arities (count (:params arity)))
                                  (not (:rest arity)) (not (:kwargs arity))))
                    valid (filter valid? contracts)
                    covered (set (map #(count (:params %)) valid))]]
          (do
            (when (some (complement valid?) contracts)
              (note diags fd fd "TSP430-01"
                    (str "s/fdef of " var " has an arity outside " protocol " " arities)))
            [var {:file file :pos pos :protocol protocol :contract (select-keys fd [:file :pos])
                  :source (if fd :fdef :protocol)
                  :contract-arities (vec valid)
                  :type {:t :fn :arities (into (vec valid)
                                              (for [n arities :when (not (contains? covered n))]
                                                {:params (vec (repeat n t/unknown)) :ret t/unknown}))}}]))))

(defn- factory-var [sym {:keys [file pos factory-arity]} fdef diags]
  (let [contracts (get-in fdef [:type :arities])
        return-only (get-in fdef [:type :ret])
        matches? (fn [arity] (and (= factory-arity (count (:params arity)))
                                  (not (:rest arity)) (not (:kwargs arity))))
        valid (vec (filter matches? contracts))
        use-spec? (or (seq valid) return-only)]
    (when (and fdef (some (complement matches?) contracts))
      (note diags fdef fdef "TSP2790"
            (str "s/fdef of " sym " must have the generated factory arity " factory-arity)))
    {:file (:file file) :pos pos :factory-arity factory-arity
     :source (if use-spec? :fdef :record)
     :contract (when use-spec? (select-keys fdef [:file :pos]))
     :type {:t :fn :arities (if (seq valid) valid
                              [{:params (vec (repeat factory-arity t/unknown)) :ret (or return-only t/unknown)}])}}))

(defn- function-var [env operator-env sym d fd diags]
  (let [sig (when (:sig d)
              (operators/normalize operator-env (:sig d)
                                   (operator-errors diags (:file d) (:sig-pos d))))
        return-only (get-in fd [:type :ret])
        declared (or sig (when-not return-only (:type fd)))
        invalid? (operators/invalid? sig)
        hints (when-not declared
                (hints-sig env (:file d) (:parts d)
                           (fn [at param previous type]
                             (note diags (:file d) at "TSP114-01"
                                   (str "incompatible preconditions on " param ": "
                                        (t/show previous) " and " (t/show type))))))
        inferred (cond-> hints (and hints return-only)
                   (update :arities #(mapv (fn [arity] (assoc arity :ret return-only)) %)))]
    (when (and sig fd (not invalid?)
               (if return-only
                 (some #(not (t/alpha-equivalent? (:ret %) return-only))
                       (:arities (if (= :all (:t sig)) (:body sig) sig)))
                 (not (t/alpha-equivalent? sig (:type fd)))))
      (note diags (:file d) (:name (:parts d)) "TSP2790"
            (str "t/sig and s/fdef of " (name sym) " disagree: "
                 (t/show sig) " vs " (if return-only (str ":ret " (t/show return-only)) (t/show (:type fd))))))
    {:parts (:parts d) :pfile (:file d) :definition (:definition d)
     :type (or declared inferred)
     :contract (if sig {:file (:file (:file d)) :pos (:sig-pos d)}
                   (select-keys fd [:file :pos]))
     :source (cond sig :sig fd :fdef :else :hints)
     :return-only? (boolean (and return-only (not sig)))
     :tagged? (boolean (or (some #(tag-node (:params %)) (:arities (:parts d)))
                           (tag-node (:name (:parts d)))))
     :complete? (if declared
                  (not invalid?)
                  (and (not (:opaque-args? fd))
                       (every? (fn [a] (and (not= :unknown (:t (:ret a)))
                                            (not-any? #(= :unknown (:t %)) (:params a))))
                               (:arities inferred))))
     :private? (:private? (:parts d))
     :file (:file (:file d)) :pos (:pos d)}))

(defn build
  "Environment from parsed files: {:specs :vars :macros :key-nss}. Returns [env diagnostics]."
  ([files] (build files {}))
  ([files contracts]
  (let [diags (atom [])
        macros (into #{} (for [file files form (declaration-forms file)
                               :when (= 'defmacro (head file form))
                               :let [sym (var-name file (second (:children form)))]
                               :when sym]
                           sym))
        names (namespace-vars files macros)
        files (mapv #(assoc % :defined-symbols (get-in names [:ns-defs (:ns %)])) files)
        spec-definitions (into {} (for [file files form (declaration-forms file)
                                        :when (= 'clojure.spec.alpha/def (head file form))
                                        :let [entry (spec-definition file form)]
                                        :when entry]
                                    entry))
        acc (reduce (fn [acc file] (collect-file acc file spec-definitions diags))
                    {:specs {} :fdefs {} :defns {} :protocols {}} files)
        contracts (externs/normalize-contracts (:specs acc) contracts)
        operator-env {:specs (merge (:specs contracts) (:specs acc))}
        env (assoc names :specs (:specs acc) :opaque-fdefs (:opaque-fdefs acc)
                   :macros macros :root-definitions (:root-definitions acc)
                   :protocols (:protocols acc)
                   :atom-initializers (into {} (filter (fn [[sym]] (= 1 (get-in acc [:root-definitions sym]))))
                                            (:atom-initializers acc))
                   :multimethod-dispatches (into {}
                                                 (for [[sym [dispatch :as declarations]] (:multimethod-dispatches acc)
                                                       :when (and dispatch (= 1 (count declarations))
                                                                  (not (contains? (:root-definitions acc) sym)))]
                                                   [sym dispatch]))
                   :key-nss (set (keep namespace (keys (:specs acc)))))
        vars (into {}
                   (for [[sym d] (:defns acc)
                         :let [fd (get-in acc [:fdefs sym])]]
                     [sym (if (contains? d :factory-arity)
                            (cond-> (factory-var sym d fd diags)
                              (:prior-defn d) (assoc :prior-defn
                                                     (function-var env operator-env sym (:prior-defn d) nil diags)))
                            (function-var env operator-env sym d fd diags))]))
        only-fdef (for [[sym fd] (:fdefs acc) :when (not (contains? vars sym))]
                    [sym (cond-> {:type (:type fd) :source :fdef :file (:file fd) :pos (:pos fd)}
                           (:opaque-args? fd) (assoc :complete? false))])]
    (let [vars (merge (into vars only-fdef) (protocol-vars (:protocols acc) (:fdefs acc) diags))
          [env ext-diags] (externs/merge-contracts (assoc env :vars vars) contracts)
          {:keys [diagnostics] :as recursion} (recursive/analyze (:specs env))]
      [(assoc env :recursive-types (dissoc recursion :diagnostics))
       (into (into @diags ext-diags) diagnostics)]))))
