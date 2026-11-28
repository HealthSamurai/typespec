(ns typespec.java
  "Java members, read by reflection from classes that are loaded but never initialized."
  (:require [typespec.types :as t])
  (:import [clojure.lang Reflector]
           [java.lang.reflect Constructor Field MalformedParameterizedTypeException Method Modifier
            ParameterizedType TypeVariable WildcardType]))

(defn- nominal-class [bindings ty]
  (cond
    (instance? Class ty) ty
    (instance? TypeVariable ty) (get bindings ty)
    (instance? ParameterizedType ty) (.getRawType ^ParameterizedType ty)))

(defn- parent-binding [bindings ty]
  (if (instance? ParameterizedType ty)
    (let [^ParameterizedType ty ty
          ^Class raw (.getRawType ty)]
      [raw (zipmap (.getTypeParameters raw)
                   (map #(nominal-class bindings %) (.getActualTypeArguments ty)))])
    [ty {}]))

(defn- inherited-return [^Class receiver ^Class owner variable]
  ;; A class-level variable is identified by its declaration, not its name.
  ;; Treat an unresolved path as evidence against choosing a concrete return.
  (loop [remaining (list [receiver {}]), seen #{}, results #{}, budget 128]
    (cond
      (empty? remaining) (when (= 1 (count results)) (first results))
      (zero? budget) nil
      :else
      (let [[[^Class c bindings :as entry] & more] remaining]
        (cond
          (seen entry) (recur more seen results (dec budget))
          (or (= c owner) (and (empty? bindings) (seq (.getTypeParameters c))))
          (recur more (conj seen entry) (conj results (get bindings variable)) (dec budget))
          :else
          (let [parents (keep (fn [ty]
                                (when-let [^Class raw (nominal-class bindings ty)]
                                  (when (.isAssignableFrom owner raw)
                                    (parent-binding bindings ty))))
                              (cons (.getGenericSuperclass c) (.getGenericInterfaces c)))]
            (recur (concat parents more) (conj seen entry) results (dec budget))))))))

(defn- method-return [^Class receiver ^Method method]
  (let [erased (.getReturnType method)
        owner (.getDeclaringClass method)
        resolved (when-not (or (= receiver owner) (Modifier/isStatic (.getModifiers method)))
                   (try
                     (when (seq (.getTypeParameters owner))
                       (let [generic (.getGenericReturnType method)]
                         (when (and (instance? TypeVariable generic)
                                    (= owner (.getGenericDeclaration ^TypeVariable generic)))
                           (inherited-return receiver owner generic))))
                     (catch TypeNotPresentException _ nil)
                     (catch MalformedParameterizedTypeException _ nil)
                     (catch LinkageError _ nil)))]
    (.getName ^Class (if (and resolved (.isAssignableFrom ^Class erased resolved)) resolved erased))))

(def ^:private element-parameter-classes
  #{Iterable java.util.Iterator java.util.ListIterator java.util.Enumeration java.util.Collection java.util.List
    java.util.Set java.util.SortedSet java.util.NavigableSet java.util.Queue java.util.Deque
    java.util.stream.Stream})

(defn- method-element-class [^Class receiver ^Method method]
  (when (element-parameter-classes (.getReturnType method))
    (try
      (let [generic (.getGenericReturnType method)]
        (when (instance? ParameterizedType generic)
          (let [[argument] (.getActualTypeArguments ^ParameterizedType generic)
                argument (if (instance? WildcardType argument)
                           (let [[upper & more] (.getUpperBounds ^WildcardType argument)]
                             (when (empty? more) upper))
                           argument)
                owner (.getDeclaringClass method)
                ^Class element (if (and (instance? TypeVariable argument)
                                        (= owner (.getGenericDeclaration ^TypeVariable argument)))
                                 (inherited-return receiver owner argument)
                                 (nominal-class {} argument))]
            (when (and element (not= Object element))
              (.getName element)))))
      (catch TypeNotPresentException _ nil)
      (catch MalformedParameterizedTypeException _ nil)
      (catch LinkageError _ nil))))

(defn- most-specific [class-of methods]
  (let [classes (distinct (map class-of methods))]
    (if (next classes)
      (remove (fn [method]
                (let [^Class cls (class-of method)]
                  (some #(and (not= cls %) (.isAssignableFrom cls ^Class %)) classes)))
              methods)
      methods)))

(defn- select-method [methods]
  (if (next methods)
    (let [rank (fn [^Method m] [(if (.isBridge m) 1 0) (if (.isSynthetic m) 1 0)])
          best-rank (rank (first (sort-by rank methods)))]
      (->> methods
           (filter #(= best-rank (rank %)))
           (most-specific #(.getReturnType ^Method %))
           (most-specific #(.getDeclaringClass ^Method %))
           (sort-by (fn [^Method m] [(.getName (.getDeclaringClass m)) (.getName (.getReturnType m))]))
           first))
    (first methods)))

(defn methods-named
  "Public overloads with erased parameters and the most specific known nominal return."
  [cname mname static?]
  (when-let [^Class c (t/load-class cname)]
    (let [params (fn [^Method m] (mapv #(.getName ^Class %) (.getParameterTypes m)))
          public-methods (cond-> (seq (.getMethods c))
                           (and (.isInterface c) (not static?)) (concat (.getMethods Object)))
          methods (->> public-methods
                       (filter (fn [^Method m] (and (= mname (.getName m))
                                                    (= (boolean static?) (Modifier/isStatic (.getModifiers m))))))
                       (sort-by (fn [^Method m] [(.getParameterCount m) (if (= Object (.getReturnType m)) 1 0)])))
          by-params (group-by params methods)]
      (mapv (fn [ps]
              (let [^Method m (select-method (get by-params ps))
                    element (method-element-class c m)]
                (cond-> {:params ps
                         :varargs (.isVarArgs m)
                         :ret (method-return c m)
                         :declaring (.getName (.getDeclaringClass m))}
                  element (assoc :element-class element))))
            (distinct (map params methods))))))

(defn method-names [cname]
  (when-let [^Class c (t/load-class cname)]
    (distinct (map #(.getName ^Method %) (.getMethods c)))))

(def ^:private native-function-methods
  {"java.lang.Runnable" {:name "run" :params [] :ret "void"}
   "java.util.concurrent.Callable" {:name "call" :params [] :ret "java.lang.Object"}
   "java.util.Comparator" {:name "compare" :params ["java.lang.Object" "java.lang.Object"]
                           :ret "int" :clojure-comparator? true}})

(def functional-method
  "The erased Java callback contract implemented or adapted by a Clojure function."
  (memoize
   (fn [cname]
     (or (get native-function-methods cname)
         (when-let [^Class c (t/load-class cname)]
           (try
             (when (and (.isInterface c)
                        (not (.isAssignableFrom c clojure.lang.IFn))
                        (Reflector/paramArgTypeMatch c clojure.lang.IFn))
               (some (fn [^Method m]
                       (when (and (Modifier/isAbstract (.getModifiers m))
                                  (<= (.getParameterCount m) 10)
                                  (not (#{"equals" "hashCode" "toString"} (.getName m))))
                         {:name (.getName m)
                          :params (mapv #(.getName ^Class %) (.getParameterTypes m))
                          :ret (.getName (.getReturnType m))}))
                     (.getMethods c)))
             (catch LinkageError _ nil)
             (catch SecurityException _ nil)))))))

(defn ambiguous-functional-targets [parameter-classes]
  (let [targets (->> parameter-classes
                     distinct
                     (keep t/load-class)
                     (filter #(Reflector/paramArgTypeMatch % clojure.lang.AFunction)))]
    (when (and (next targets)
               (every? (fn [^Class target]
                         (and (not (.isAssignableFrom target clojure.lang.AFunction))
                              (functional-method (.getName target))))
                       targets))
      (let [specific (most-specific identity targets)]
        (when (next specific)
          (sort (map #(.getName ^Class %) specific)))))))

(defn constructors
  "Public constructor signatures. Returns nil if the signatures cannot be read."
  [^Class cls]
  (try
    (->> (.getConstructors cls)
         (map (fn [^Constructor ctor]
                {:params (mapv #(.getName ^Class %) (.getParameterTypes ctor))
                 :varargs (.isVarArgs ctor)}))
         (sort-by (juxt (comp count :params) :params))
         vec)
    (catch LinkageError _ nil)
    (catch SecurityException _ nil)))

(defn field-info
  "The declared JVM type and final modifier of a public field."
  [cname fname static?]
  (when-let [^Class cls (t/load-class cname)]
    (try
      (when-let [^Field field (Reflector/getField cls fname static?)]
        {:type (.getType field)
         :final? (Modifier/isFinal (.getModifiers field))})
      (catch LinkageError _ nil)
      (catch SecurityException _ nil))))

(defn field-class [cname fname static?]
  (some-> ^Class (:type (field-info cname fname static?)) .getName))

(defn has-field? [cname fname]
  (boolean (or (field-class cname fname true) (field-class cname fname false))))

(defn has-class? [cname] (some? (t/load-class cname)))

;; An omitted parameter vector applies to every overload.
(def ^:private nullable-returns
  #{["java.util.Map" "get"] ["java.util.Map" "remove"] ["java.util.Queue" "poll"] ["java.util.Queue" "peek"]
    ["java.lang.System" "getenv" ["java.lang.String"]] ["java.lang.System" "getProperty"]
    ["java.lang.Class" "getResource"]
    ["java.util.regex.Matcher" "group"]})

(defn nullable-return? [cname mname params]
  (boolean (some (fn [[c m ps]]
                   (and (= m mname)
                        (or (nil? ps) (= ps params))
                        (when-let [^Class owner (t/load-class c)]
                          (when-let [^Class k (t/load-class cname)] (.isAssignableFrom owner k)))))
                 nullable-returns)))

(defn short-name [cname]
  (if (re-matches #"java\.lang\.[A-Z]\w*" cname) (subs cname 10) cname))
