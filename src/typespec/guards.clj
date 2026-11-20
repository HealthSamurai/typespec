(ns typespec.guards
  "Compile unary guard bodies to bounded recipes over the type of a stable argument."
  (:require [typespec.types :as t]))

(def ^:private predicates
  {'clojure.core/string? [:string string?] 'clojure.core/keyword? [:keyword keyword?]
   'clojure.core/number? [:number number?] 'clojure.core/int? [:int int?]
   'clojure.core/map? [:map map?] 'clojure.core/vector? [:vector vector?]
   'clojure.core/boolean? [:boolean boolean?] 'clojure.core/symbol? [:symbol symbol?]})

(def ^:private flow-heads
  '#{clojure.core/and clojure.core/or clojure.core/not clojure.core/if clojure.core/if-not
     clojure.core/when clojure.core/when-not clojure.core/do clojure.core/let clojure.core/throw})

(def ^:private dynamic-heads
  '#{clojure.core/with-redefs clojure.core/with-redefs-fn clojure.core/alter-var-root
     clojure.core/binding clojure.core/set! clojure.core/eval clojure.core/load-string
     clojure.spec.alpha/def})

(defn- literal? [node]
  (#{:kw :str :num :bool :nil} (:op node)))

(defn compile-body
  "The resolver returns the actual var symbol for a head, without loading it.
  Scope entries either identify the original parameter or hold a saved condition recipe."
  [{:keys [resolve-head class-name macro? callable?]} params body]
  (let [nodes (vec (take 257 (mapcat #(tree-seq (comp seq :children) :children %) body)))
        unsupported? (fn [node]
                       (let [head (resolve-head node)]
                         (or (some #(#{'t/cast 't/any} (:val (first (:children %)))) (:directives node))
                             (dynamic-heads head)
                             (and (macro? head) (not (flow-heads head))))))]
    (when (and (<= (count nodes) 256) (not-any? unsupported? nodes))
      (letfn [(argument? [scope node]
                (and (= :sym (:op node)) (= :argument (get scope (:val node)))))
              (recipe [scope node]
                (cond
                  (literal? node) [:constant (boolean (:val node))]
                  (= :sym (:op node)) (let [value (get scope (:val node))]
                                        (cond (= :argument value) [:truth]
                                              (vector? value) value
                                              :else [:opaque]))
                  (not= :list (:op node)) [:opaque]
                  :else
                  (let [[operator a b c & tail] (:children node)
                        head (when-not (contains? scope (:val operator)) (resolve-head node))
                        args (rest (:children node))]
                    (case head
                      (clojure.core/and clojure.core/or)
                      [(if (= head 'clojure.core/and) :and :or) (mapv #(recipe scope %) args)]
                      clojure.core/not (if (= 1 (count args)) [:not (recipe scope a)] [:opaque])
                      (clojure.core/if clojure.core/if-not)
                      (if (and b (empty? tail))
                        [:if (cond->> (recipe scope a) (= head 'clojure.core/if-not) (vector :not))
                         (recipe scope b) (if c (recipe scope c) [:constant false])]
                        [:opaque])
                      (clojure.core/when clojure.core/when-not)
                      [:if (cond->> (recipe scope a) (= head 'clojure.core/when-not) (vector :not))
                       (result scope (rest args)) [:constant false]]
                      clojure.core/do (result scope args)
                      clojure.core/throw [:exit]
                      clojure.core/let
                      (if (and (= :vector (:op a)) (even? (count (:children a))))
                        (let [locals (reduce (fn [env [p init]]
                                               (if (= :sym (:op p))
                                                 (assoc env (:val p) (if (argument? env init) :argument (recipe env init)))
                                                 (reduce #(assoc %1 (:val %2) :opaque) env
                                                         (filter #(= :sym (:op %)) (tree-seq :children :children p)))))
                                             scope (partition 2 (:children a)))]
                          (result locals (rest args)))
                        [:opaque])
                      (clojure.core/= clojure.core/not=)
                      (if (and (= 2 (count args))
                               (or (and (argument? scope a) (literal? b))
                                   (and (argument? scope b) (literal? a))))
                        (cond->> [:equal (:val (if (literal? a) a b))]
                          (= head 'clojure.core/not=) (vector :not))
                        [:opaque])
                      clojure.spec.alpha/valid?
                      (if (and (= 2 (count args)) (= :kw (:op a)) (argument? scope b))
                        [:spec (:val a)] [:opaque])
                      clojure.core/instance?
                      (if-let [class (and (= 2 (count args)) (= :sym (:op a)) (argument? scope b)
                                          (not (contains? scope (:val a))) (class-name (:val a)))]
                        [:class class] [:opaque])
                      (if (and (= 1 (count args)) (argument? scope a))
                        (cond
                          (#{'clojure.core/some? 'clojure.core/nil?} head) [:nil (= head 'clojure.core/nil?)]
                          (predicates head) [:predicate head]
                          (callable? head) [:call head]
                          :else [:opaque])
                        [:opaque])))))
              (result [scope forms]
                (if-let [form (last forms)] (recipe scope form) [:constant false]))]
        (let [plan (result (zipmap params (repeat :argument)) body)]
          (when (<= (count (take 257 (tree-seq vector? seq plan))) 256)
            plan))))))

(defn dependencies [plan]
  (into #{} (keep #(when (and (vector? %) (= :call (first %))) (second %)))
        (tree-seq vector? seq plan)))

(defn useful? [plan]
  (boolean (some #(and (vector? %) (#{:truth :nil :predicate :equal :class :spec :call} (first %)))
                 (tree-seq vector? seq plan))))

(defn evidence-type?
  "A source type can support an independent proof when expansion has no unchecked leaves."
  [env ty]
  (let [budget (volatile! 256)]
    (letfn [(known? [ty seen]
              (and (not (neg? (vswap! budget dec)))
                   (case (:t ty)
                     (:prim :lit :nil :never) true
                     :class (some? (t/load-class (:name ty)))
                     :ref (let [k (:name ty) entry (get-in env [:specs k])]
                            (and entry (not= :extern (:source entry)) (not (seen k))
                                 (known? (:type entry) (conj seen k))))
                     (:union :merge) (every? #(known? % seen) (:types ty))
                     :map (every? #(known? % seen) (concat (vals (:req ty)) (vals (:opt ty))))
                     :map-of (every? #(known? % seen) [(:k ty) (:v ty)])
                     :coll (known? (:of ty) seen)
                     false)))]
      (boolean (known? ty #{})))))

(defn proves? [env actual expected]
  (and (evidence-type? env actual) (evidence-type? env expected)
       (binding [t/*strict-shapes* true]
         (letfn [(proof [actual expected depth]
                   (and (< depth 16)
                        (every? (fn [a]
                                  (some (fn [e]
                                          (and (t/sub? env a e)
                                               (case (:t e)
                                                 :map (every? (fn [[k field]]
                                                                (when-let [value (or (get (:req a) k) (get (:opt a) k))]
                                                                  (proof value field (inc depth))))
                                                              (concat (:req e) (:opt e)))
                                                 :map-of (and (= :map-of (:t a))
                                                              (proof (:k a) (:k e) (inc depth))
                                                              (proof (:v a) (:v e) (inc depth)))
                                                 :coll (and (= :coll (:t a))
                                                            (if (:items e)
                                                              (every? true? (map #(proof %1 %2 (inc depth)) (:items a) (:items e)))
                                                              (proof (:of a) (:of e) (inc depth))))
                                                 true)))
                                        (t/members env expected)))
                                (t/members env actual))))]
           (or (= t/never actual) (proof actual expected 0))))))

(defn input-type [ty]
  (if (:platform ty) (t/union (dissoc ty :platform) t/nil-t) ty))

(defn- literal-type [v]
  (if (nil? v) t/nil-t (t/lit v)))

(defn- definitely-disjoint? [a b]
  (and (= :prim (:t a) (:t b))
       (#{:string :keyword :symbol :boolean :number :int :double} (:name a))
       (#{:string :keyword :symbol :boolean :number :int :double} (:name b))
       (not= (:name a) (:name b))
       (not (and (#{:number :int :double} (:name a)) (#{:number :int :double} (:name b))))))

(def ^:private vague-tags #{:any :unknown :rigid :var :ivar :all})

(defn- truth-split [leaf]
  (case (:t leaf)
    (:any :unknown :rigid :var :ivar :all :class) [leaf leaf]
    :nil [t/never leaf]
    :lit (if (:val leaf) [leaf t/never] [t/never leaf])
    :prim (if (= :boolean (:name leaf)) [(t/lit true) (t/lit false)] [leaf t/never])
    [leaf t/never]))

(defn- predicate-split [env leaf [name predicate] head]
  (let [target (t/prim name)]
    (cond
      (and (= head 'clojure.core/int?) (or (integer? (:val leaf)) (= :int (:name leaf)))) [leaf leaf]
      (#{:lit :nil} (:t leaf)) (if (predicate (:val leaf)) [leaf t/never] [t/never leaf])
      (proves? env leaf target) [leaf t/never]
      (definitely-disjoint? leaf target) [t/never leaf]
      :else [target leaf])))

(defn- class-split [env leaf class]
  (let [target (t/class-type class)]
    (cond
      (= :nil (:t leaf)) [t/never leaf]
      (= "java.lang.Object" class) [leaf (if (vague-tags (:t leaf)) t/nil-t t/never)]
      (and (or (= :class (:t leaf)) (#{:string :keyword :boolean :symbol} (:name target)))
           (proves? env leaf target)) [leaf t/never]
      :else [target leaf])))

(defn- literal-split [env leaf value]
  (let [target (literal-type value)]
    (cond
      (#{:lit :nil} (:t leaf)) (if (= value (:val leaf)) [leaf t/never] [t/never leaf])
      (and (= :boolean (:name leaf)) (boolean? value)) [target (t/lit (not value))]
      (and (= :prim (:t leaf)) (not (t/sub? env target leaf))) [t/never leaf]
      :else [target leaf])))

(defn- atoms [env [op value] leaf]
  (case op
    :truth (truth-split leaf)
    :nil (let [parts (case (:t leaf)
                       (:unknown :any :rigid :var :ivar :all) [leaf t/nil-t]
                       :nil [t/never leaf]
                       [leaf t/never])]
           (if value (vec (reverse parts)) parts))
    :predicate (predicate-split env leaf (predicates value) value)
    :class (class-split env leaf value)
    :equal (literal-split env leaf value)
    :spec (let [ref (t/ref-t value)]
            (if (evidence-type? env ref) [(if (proves? env leaf ref) leaf ref) leaf] [leaf leaf]))))

(defn split
  "Return [truthy-type falsy-type]. Exhausting the budget retains the input on both paths."
  [env plan input]
  (let [budget (volatile! 256)]
    (letfn [(run [[op a b c] ty active]
              (cond
                (= t/never ty) [ty ty]
                (neg? (vswap! budget dec)) [ty ty]
                :else
                (case op
                  :opaque [ty ty]
                  :exit [t/never t/never]
                  :constant (if a [ty t/never] [t/never ty])
                  :not (vec (reverse (run a ty active)))
                  :if (let [[yes no] (run a ty active)
                            [yt yf] (run b yes active)
                            [nt nf] (run c no active)]
                        [(t/union yt nt) (t/union yf nf)])
                  (:and :or)
                  (let [and? (= op :and)
                        [live stopped] (reduce (fn [[current stopped] sub]
                                                 (let [[yes no] (run sub current active)]
                                                   (if and? [yes (t/union stopped no)] [no (t/union stopped yes)])))
                                               [ty t/never] a)]
                    (if and? [live stopped] [stopped live]))
                  :call (if-let [{:keys [mode plan domain]} (get-in env [:guards a])]
                          (if (and (= :checked mode) (not (active a))
                                   (or (= t/unknown domain) (proves? env ty domain)))
                            (run plan ty (conj active a)) [ty ty])
                          [ty ty])
                  (let [members (vec (take (inc @budget) (t/members env (input-type ty))))]
                    (if (neg? (vswap! budget - (count members)))
                      [ty ty]
                      (let [parts (mapv #(if (= t/never %) [t/never t/never] (atoms env [op a] %)) members)]
                        [(apply t/union (map first parts)) (apply t/union (map second parts))]))))))]
      (run plan input #{}))))

(defn apply-summary [env {:keys [mode plan domain target]} input]
  (if (or (= t/unknown domain) (proves? env input domain))
    (if (= :trusted mode)
      [(if (proves? env input target) input (t/with-assumption target :trusted-guard)) input]
      (split env plan input))
    [input input]))
