(ns typespec.env
  "Collects declarations from source text: s/def, s/fdef, {:t/sig ...}, type hints. No code is loaded."
  (:require [typespec.parse :as parse]
            [typespec.types :as t]))

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

(defn- attr-get [attr k]
  (some (fn [[kn vn]] (when (= k (:val kn)) vn)) (partition 2 (:children attr))))

(defn type-ctx [file fail]
  {:aliases (:aliases file) :imports (:imports file) :fail fail})

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
             (assoc-in acc [:specs (:val k)]
                       {:file (:file file) :pos (:pos k)
                        :type (t/parse-type (type-ctx file (opaque body)) (parse/node->data body))})
             acc))

         (= 'clojure.spec.alpha/fdef h)
         (let [[_ nm & opts] (:children form)
               {:keys [args ret]} (apply hash-map (parse/node->data {:op :list :children opts}))
               ctx (type-ctx file (opaque form))
               r (if ret (t/parse-type ctx ret) t/unknown)
               ars (some-> (t/cat-items ctx args) (t/cat->arities r))]
           (if (and (= :sym (:op nm)) ars)
             (assoc-in acc [:fdefs (qualify (:val nm))] {:type {:t :fn :arities ars} :file (:file file) :pos (:pos form)})
             acc))

         (#{'defn 'defn-} h)
         (let [parts (defn-parts form)
               sig-node (some-> (:attr parts) (attr-get :t/sig))]
           (assoc-in acc [:defns (qualify (:val (:name parts)))]
                     {:file file :parts parts :pos (:pos (:name parts))
                      :sig (when sig-node
                             (t/parse-type (type-ctx file (fail sig-node)) (parse/node->data sig-node)))}))

         (= 'defmacro h)
         (update acc :macros conj (qualify (:val (second (:children form)))))

         :else acc)))
   acc (:forms file)))

(defn build
  "Environment from parsed files: {:specs :vars :macros :key-nss}. Returns [env diagnostics]."
  [files]
  (let [diags (atom [])
        acc (reduce #(collect-file %1 %2 diags) {:specs {} :fdefs {} :defns {} :macros #{}} files)
        env {:specs (:specs acc) :macros (:macros acc)
             :key-nss (set (keep namespace (keys (:specs acc))))}
        vars (into {}
                   (for [[sym d] (:defns acc)
                         :let [fd (get-in acc [:fdefs sym])
                               declared (or (:sig d) (:type fd))]]
                     (do
                       (when (and (:sig d) fd (not= (:sig d) (:type fd)))
                         (note diags (:file d) (:name (:parts d)) "TSP2790"
                               (str "t/sig and s/fdef of " (name sym) " disagree: "
                                    (t/show (:sig d)) " vs " (t/show (:type fd)))))
                       [sym {:parts (:parts d) :pfile (:file d)
                             :type (or declared (hints-sig env (:file d) (:parts d)))
                             :source (cond (:sig d) :sig fd :fdef :else :hints)
                             :tagged? (boolean (or (some #(tag-node (:params %)) (:arities (:parts d)))
                                                   (tag-node (:name (:parts d)))))
                             :complete? (or (some? declared)
                                            (every? (fn [a] (and (not= :unknown (:t (:ret a)))
                                                                 (not-any? #(= :unknown (:t %)) (:params a))))
                                                    (:arities (hints-sig env (:file d) (:parts d)))))
                             :private? (:private? (:parts d))
                             :file (:file (:file d)) :pos (:pos d)}])))
        only-fdef (for [[sym fd] (:fdefs acc) :when (not (contains? vars sym))]
                    [sym {:type (:type fd) :source :fdef :file (:file fd) :pos (:pos fd)}])]
    [(assoc env :vars (into vars only-fdef)) @diags]))
