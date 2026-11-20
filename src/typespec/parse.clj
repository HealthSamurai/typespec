(ns typespec.parse
  "Reads Clojure source into a small AST with positions. Nothing is loaded or evaluated."
  (:require [clojure.string :as str]
            [rewrite-clj.parser :as p]
            [rewrite-clj.node :as n]))

(def ^:private skip-tags #{:whitespace :newline :comma :comment})

(defn- npos [x]
  (let [m (meta x)]
    {:row (:row m) :col (:col m) :end-row (:end-row m) :end-col (:end-col m)}))

(def directive-heads '#{t/ignore t/cast t/any t/const t/satisfies})

(defn- directive? [d]
  (and (= :list (:op d))
       (= :sym (:op (first (:children d))))
       (contains? directive-heads (:val (first (:children d))))))

(declare convert)

(defn- convert-seq
  "Converts sibling nodes. A `#_(t/...)` form is not dropped: it becomes a directive on the next form."
  [nodes opts]
  (loop [[x & more :as xs] (seq nodes), pending [], out []]
    (if (empty? xs)
      (cond-> out
        (seq pending) (conj {:op :opaque :pos (:pos (first pending))
                            :directives pending :missing-directive-target true}))
      (let [tag (n/tag x)]
        (cond
          (skip-tags tag) (recur more pending out)
          (= :uneval tag) (let [c (first (remove #(skip-tags (n/tag %)) (n/children x)))
                                d (when c (convert c opts))]
                            (recur more (cond-> pending (directive? d) (conj d)) out))
          :else (let [c (convert x opts)]
                  (if (nil? c)
                    (recur more pending out)
                    (recur more [] (conj out (cond-> c (seq pending) (assoc :directives pending)))))))))))

(defn- reader-cond [x opts]
  (let [[tok form] (remove #(skip-tags (n/tag %)) (n/children x))
        t (try (n/sexpr tok) (catch Exception _ nil))]
    (cond
      (and (= '? t) (= :list (n/tag form)))
      (let [kvs (partition 2 (convert-seq (n/children form) opts))
            m (into {} (map (fn [[k v]] [(:val k) v])) kvs)]
        (or (get m :clj) (get m :default)))
      (= '?@ t) nil
      :else {:op :opaque :pos (npos x)})))

(defn convert [x opts]
  (let [tag (n/tag x) pos (npos x)]
    (case tag
      (:token :multi-line)
      (let [v (n/sexpr x opts)]
        (cond (symbol? v)  {:op :sym :val v :pos pos}
              (keyword? v) {:op :kw :val v :pos pos}
              (string? v)  {:op :str :val v :pos pos}
              (number? v)  {:op :num :val v :pos pos}
              (nil? v)     {:op :nil :pos pos}
              (char? v)    {:op :char :val v :pos pos}
              (boolean? v) {:op :bool :val v :pos pos}
              :else        {:op :opaque :pos pos}))
      (:list :vector :map :set) {:op tag :children (convert-seq (n/children x) opts) :pos pos}
      (:meta :meta*) (let [[m t] (convert-seq (n/children x) opts)
                           m (update m :pos merge (select-keys pos [:row :col]))] ; include the ^
                       (when t (update t :meta #(into [m] %))))
      :quote {:op :quote :children (convert-seq (n/children x) opts) :pos pos}
      :fn {:op :fn-lit :children (convert-seq (n/children x) opts) :pos pos}
      :deref {:op :list :pos pos
              :children (into [{:op :sym :val 'clojure.core/deref :pos pos}] (convert-seq (n/children x) opts))}
      :regex {:op :regex :pos pos}
      :namespaced-map (some-> (last (convert-seq (n/children x) opts)) (assoc :pos pos))
      :reader-macro (reader-cond x opts)
      {:op :opaque :pos pos})))

(defn- parse-ns-form [form]
  (let [[_ nm & more] form
        clauses (filter seq? more)
        libspecs (for [c clauses :when (= :require (first c)) s (rest c)] s)
        imports (for [c clauses :when (= :import (first c)) s (rest c)] s)]
    {:ns nm
     :aliases (into {} (for [s libspecs :when (vector? s)
                             :let [[lib & opts] s o (apply hash-map opts)]
                             a [(:as o) (:as-alias o)] :when a]
                         [a lib]))
     :refers (into {} (for [s libspecs :when (vector? s)
                            :let [[lib & opts] s o (apply hash-map opts)]
                            r (when (sequential? (:refer o)) (:refer o))]
                        [r (symbol (str lib) (str r))]))
     :imports (into {} (for [s imports
                             [short full] (if (symbol? s)
                                            [[(symbol (last (.split (str s) "\\."))) (str s)]]
                                            (for [c (rest s)] [c (str (first s) "." c)]))]
                         [short full]))}))

(defn parse-string
  "Returns {:file :ns :aliases :refers :imports :forms}."
  [file src]
  (let [root (p/parse-string-all src)
        dummy {:auto-resolve (fn [a] (if (= :current a) 'user a))}
        ns-node (first (filter #(and (= :list (n/tag %))
                                     (= 'ns (try (n/sexpr (first (n/children %))) (catch Exception _ nil))))
                               (n/children root)))
        info (if ns-node (parse-ns-form (n/sexpr ns-node dummy)) {:ns 'user :aliases {} :refers {} :imports {}})
        ;; Like ns imports, literal top-level imports are collected file-wide;
        ;; source-order rebinding is not modeled. Do not evaluate forms
        ;; (in particular, never require the namespace being checked).
        import-specs (for [node (n/children root)
                           :when (= :list (n/tag node))
                           :let [form (try (n/sexpr node dummy) (catch Exception _ nil))]
                           :when (#{'import 'clojure.core/import} (first form))
                           spec (rest form)]
                       (cond
                         (and (seq? spec) (= 'quote (first spec))) (second spec)
                         (or (symbol? spec) (vector? spec)) spec
                         :else nil))
        literal-imports (filter #(or (symbol? %)
                                     (and (sequential? %) (seq %) (every? symbol? %))) import-specs)
        info (update info :imports merge
                     (:imports (parse-ns-form (list 'ns (:ns info) (cons :import literal-imports)))))
        opts {:auto-resolve (fn [a] (if (= :current a) (:ns info) (get (:aliases info) a a)))}]
    (assoc info :file file :forms (convert-seq (n/children root) opts))))

(defn parse-file [file] (parse-string (str file) (slurp file)))

(defn unparse
  "Short source text of a node, for messages."
  [node]
  (let [kids #(str/join " " (map unparse (:children node)))]
    (case (:op node)
      (:sym :kw :num :bool) (str (:val node))
      (:str :char) (pr-str (:val node))
      :nil "nil"
      :list (str "(" (kids) ")")
      :vector (str "[" (kids) "]")
      :map (str "{" (kids) "}")
      :set (str "#{" (kids) "}")
      :quote (str "'" (kids))
      :fn-lit (str "#(" (kids) ")")
      "…")))

(defn node->data
  "Plain Clojure data for a node. Type signatures can retain list positions as metadata."
  ([n] (node->data n false))
  ([n positions?]
   (let [read-child #(node->data % positions?)]
     (case (:op n)
       (:sym :kw :str :num :bool :char) (:val n)
       :nil nil
       :list (let [form (apply list (map read-child (:children n)))]
               (if positions? (with-meta form {:typespec/pos (:pos n)}) form))
       :vector (mapv read-child (:children n))
       :map (let [kids (map read-child (:children n))]
              (if (even? (count kids)) (apply array-map kids) ::opaque))
       :set (set (map read-child (:children n)))
       :quote (list 'quote (read-child (first (:children n))))
       :fn-lit (list 'fn* '[%] (apply list (map read-child (:children n))))
       ::opaque))))
