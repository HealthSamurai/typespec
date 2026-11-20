(ns typespec.env
  "Collects declarations from source text: s/def, s/fdef, {:t/sig ...}, type hints. No code is loaded."
  (:require [typespec.parse :as parse]
            [typespec.types :as t]
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

(defn tag-node
  "The type hint of a node: ^String x, ^long x."
  [node]
  (first (filter #(= :sym (:op %)) (:meta node))))

(defn hint-type [file node]
  (when-let [tn (tag-node node)]
    (when-let [c (t/resolve-class (:imports file) (:val tn))]
      (t/platform (t/class-type c) (str (:val tn))))))

(defn defn-parts [node]
  (let [[h nm & more] (:children node)
        [doc more] (if (= :str (:op (first more))) [(first more) (rest more)] [nil more])
        [attr more] (if (= :map (:op (first more))) [(first more) (rest more)] [nil more])
        arity (fn [[params & body]] {:params params :body (vec body)})]
    {:name nm
     :private? (or (= 'defn- (:val h)) (boolean (some #(= :private (:val %)) (:meta nm))))
     :attr attr
     :arities (cond (= :vector (:op (first more))) [(arity more)]
                    :else (vec (for [l more :when (and (= :list (:op l)) (= :vector (:op (first (:children l)))))]
                                 (arity (:children l)))))}))

(defn split-params
  "{:fixed [nodes] :rest node-or-nil} of a parameter vector."
  [params]
  (let [[fixed more] (split-with #(not (and (= :sym (:op %)) (= '& (:val %)))) (:children params))]
    {:fixed (vec fixed) :rest (second more)}))

(defn- var-name [file node]
  (when (and (= :sym (:op node)) (simple-symbol? (:val node)))
    (symbol (str (:ns file)) (name (:val node)))))

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

(defn- expression-children [form operator]
  (let [[_ a & more] (:children form)]
    (case (:op form)
      (:vector :map :set) (:children form)
      :fn-lit [(assoc form :op :list)]
      :list
      (case operator
        (defn defn-) (mapcat :body (:arities (defn-parts form)))
        defmethod (drop 2 more)
        (fn fn*) (function-bodies (rest (:children form)))
        (def defonce) (when-let [init (definition-initializer form)] [init])
        (let let* loop loop* binding with-open if-let if-some when-let when-some)
        (concat (binding-values a) more)
        time (when (and a (empty? more)) [a])
        case (concat [a] (map second (partition 2 more))
                     (when (odd? (count more)) [(last more)]))
        (do if if-not when when-not and or cond condp try finally throw future lazy-seq)
        (rest (:children form))
        catch (drop 3 (:children form))
        nil)
      nil)))

(defn- var-declarations [files macros]
  (letfn [(collect [by-name file form at-load?]
            (let [[head-node target] (:children form)
                  name (var-name file target)
                  operator (when-not (contains? macros (var-name file head-node)) (head file form))
                  data? (or (= 'def operator)
                            (and (#{'defonce 'declare} operator)
                                 (or at-load? (namespace (:val head-node)))))
                  names (cond
                          (and data? (= 'declare operator)) (keep #(var-name file %) (rest (:children form)))
                          (and name (or data? (and at-load? (#{'defn 'defn-} operator)))) [name])
                  init? (and at-load?
                             (or (#{'defn 'defn-} operator) (initialized-var file form)))
                  by-name (reduce (fn [acc sym]
                                    (cond-> acc
                                      data? (assoc-in [sym :tracks-root?] true)
                                      init? (update-in [sym :initializers] (fnil conj [])
                                                       {:file (:file file) :pos (:pos form)})))
                                  by-name names)]
              (reduce #(collect %1 file %2 (and at-load? (#{'do 'time} operator)))
                      by-name (expression-children form operator))))]
    (reduce (fn [acc file] (reduce #(collect %1 file %2 true) acc (:forms file))) {} files)))

(defn namespace-vars
  "Known names and Var initialization sites. Defs inside opaque macro bodies are excluded."
  [files macros]
  (let [declarations (var-declarations files macros)
        top-level (reduce (fn [acc file]
                            (update acc (:ns file) (fnil into #{})
                                    (for [form (:forms file)
                                          :when (= :list (:op form))
                                          :let [[h nm] (:children form)]
                                          :when (and (= :sym (:op h)) (= :sym (:op nm))
                                                     (.startsWith (name (:val h)) "def"))]
                                      (:val nm))))
                          {} files)]
    {:var-declarations declarations
     :ns-defs (reduce (fn [acc sym]
                       (update acc (symbol (namespace sym)) (fnil conj #{}) (symbol (name sym))))
                     top-level (keys declarations))}))

(defn attr-get [attr k]
  (some (fn [[kn vn]] (when (= k (:val kn)) vn)) (partition 2 (:children attr))))

(defn type-ctx [file fail]
  (assoc (select-keys file [:ns :aliases :refers :imports]) :fail fail))

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

(defn- pre-types
  "{param-symbol type} from :pre conditions like (string? x) and (s/valid? ::spec x)."
  [file body]
  (into {}
        (for [test (pre-conditions body)
              :let [h (head file test) [a b] (rest (:children test))]]
          (cond (and (pre-preds h) (= :sym (:op a))) [(:val a) (t/prim (pre-preds h))]
                (and (= 'clojure.spec.alpha/valid? h) (= :kw (:op a)) (= :sym (:op b))) [(:val b) (t/ref-t (:val a))]
                :else nil))))

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

(defn- hints-sig [env file parts]
  {:t :fn
   :arities (vec (for [{:keys [params body]} (:arities parts)
                       :let [{:keys [fixed rest]} (split-params params)
                             pre (pre-types file body)]]
                   (cond-> {:params (mapv #(or (when (= :sym (:op %)) (get pre (:val %)))
                                               (pattern-type env file %))
                                          fixed)
                            :ret (or (hint-type file params)
                                     ;; a class hint on the name is a return hint too; a primitive one is a mistake
                                     (when-not (some-> (tag-node (:name parts)) :val str t/primitives)
                                       (hint-type file (:name parts)))
                                     t/unknown)}
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

(defn- collect-file [acc file diags]
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
         (let [[_ k body] (:children form)]
           (if (and (= :kw (:op k)) body)
             (let [spec-form (parse/node->data body)]
               (register-spec acc :specs (:val k)
                              {:file (:file file) :pos (:pos k)
                               :type (t/parse-type (type-ctx file (opaque body)) spec-form)
                               :spec-form spec-form :spec-context (dissoc (type-ctx file nil) :fail)}
                              diags))
             acc))

         (= 'clojure.spec.alpha/fdef h)
         (let [[_ nm & opts] (:children form)
               {:keys [args ret]} (apply hash-map (parse/node->data {:op :list :children opts}))
               ctx (type-ctx file (opaque form))
               r (if ret (t/parse-type ctx ret) t/unknown)
               ars (t/spec-args->arities ctx args r)]
           (if (and (= :sym (:op nm)) ars)
             (register-spec acc :fdefs (qualify (:val nm))
                            {:type {:t :fn :arities ars} :file (:file file) :pos (:pos form)} diags)
             acc))

         (= 'defprotocol h)
         (let [[_ nm & forms] (:children form)]
           (if (and (= :sym (:op nm)) (simple-symbol? (:val nm)))
             (assoc-in acc [:protocols (symbol (str (:ns file)) (name (:val nm)))] (protocol-methods file forms))
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

         (= 'defmacro h)
         (update acc :macros conj (qualify (:val (second (:children form)))))

         (and (#{'def 'defonce} h) (= :sym (:op (second (:children form)))))
         (update-in acc [:root-definitions (qualify (:val (second (:children form))))] (fnil inc 0))

         :else acc)))
   acc (:forms file)))

(defn- protocol-vars [protocols fdefs diags]
  (into {}
        (for [[protocol methods] (sort-by key protocols)
              [_ {:keys [var file pos arities]}] methods
              :let [fd (get fdefs var)
                    contracts (get-in fd [:type :arities])
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

(defn build
  "Environment from parsed files: {:specs :vars :macros :key-nss}. Returns [env diagnostics]."
  ([files] (build files {}))
  ([files contracts]
  (let [diags (atom [])
        acc (reduce #(collect-file %1 %2 diags) {:specs {} :fdefs {} :defns {} :macros #{} :protocols {}} files)
        contracts (externs/normalize-contracts (:specs acc) contracts)
        operator-env {:specs (merge (:specs contracts) (:specs acc))}
        env {:specs (:specs acc) :macros (:macros acc) :root-definitions (:root-definitions acc)
             :protocols (:protocols acc)
             :key-nss (set (keep namespace (keys (:specs acc))))}
        vars (into {}
                   (for [[sym d] (:defns acc)
                         :let [fd (get-in acc [:fdefs sym])
                               sig (when (:sig d)
                                     (operators/normalize operator-env (:sig d)
                                                          (operator-errors diags (:file d) (:sig-pos d))))
                               declared (or sig (:type fd))
                               invalid? (operators/invalid? sig)]]
                     (do
                       (when (and sig fd (not invalid?) (not (t/alpha-equivalent? sig (:type fd))))
                         (note diags (:file d) (:name (:parts d)) "TSP2790"
                               (str "t/sig and s/fdef of " (name sym) " disagree: "
                                    (t/show sig) " vs " (t/show (:type fd)))))
                       [sym {:parts (:parts d) :pfile (:file d) :definition (:definition d)
                             :type (or declared (hints-sig env (:file d) (:parts d)))
                             :contract (if sig {:file (:file (:file d)) :pos (:sig-pos d)}
                                           (select-keys fd [:file :pos]))
                             :source (cond sig :sig fd :fdef :else :hints)
                             :tagged? (boolean (or (some #(tag-node (:params %)) (:arities (:parts d)))
                                                   (tag-node (:name (:parts d)))))
                             :complete? (if declared
                                          (not invalid?)
                                            (every? (fn [a] (and (not= :unknown (:t (:ret a)))
                                                                 (not-any? #(= :unknown (:t %)) (:params a))))
                                                    (:arities (hints-sig env (:file d) (:parts d)))))
                             :private? (:private? (:parts d))
                             :file (:file (:file d)) :pos (:pos d)}])))
        only-fdef (for [[sym fd] (:fdefs acc) :when (not (contains? vars sym))]
                    [sym {:type (:type fd) :source :fdef :file (:file fd) :pos (:pos fd)}])]
    (let [vars (merge (into vars only-fdef) (protocol-vars (:protocols acc) (:fdefs acc) diags))
          [env ext-diags] (externs/merge-contracts (assoc env :vars vars) contracts)]
      [env (into @diags ext-diags)]))))
