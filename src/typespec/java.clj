(ns typespec.java
  "Java members, read by reflection from classes that are loaded but never initialized."
  (:require [typespec.types :as t])
  (:import [java.lang.reflect MalformedParameterizedTypeException Method Modifier ParameterizedType TypeVariable]))

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
          methods (->> (.getMethods c)
                       (filter (fn [^Method m] (and (= mname (.getName m))
                                                    (= (boolean static?) (Modifier/isStatic (.getModifiers m))))))
                       (sort-by (fn [^Method m] [(.getParameterCount m) (if (= Object (.getReturnType m)) 1 0)])))
          by-params (group-by params methods)]
      (mapv (fn [ps]
              (let [^Method m (select-method (get by-params ps))]
                {:params ps
                 :varargs (.isVarArgs m)
                 :ret (method-return c m)
                 :declaring (.getName (.getDeclaringClass m))}))
            (distinct (map params methods))))))

(defn method-names [cname]
  (when-let [^Class c (t/load-class cname)]
    (distinct (map #(.getName ^Method %) (.getMethods c)))))

(defn has-field? [cname fname]
  (when-let [^Class c (t/load-class cname)]
    (boolean (some #(= fname (.getName ^java.lang.reflect.Field %)) (.getFields c)))))

(defn has-class? [cname] (some? (t/load-class cname)))

;; The JDK carries no nullability annotations, so the well-known cases are listed here.
(def ^:private nullable-returns
  #{["java.util.Map" "get"] ["java.util.Map" "remove"] ["java.util.Queue" "poll"] ["java.util.Queue" "peek"]
    ["java.lang.System" "getenv"] ["java.lang.System" "getProperty"] ["java.lang.Class" "getResource"]
    ["java.util.regex.Matcher" "group"]})

(defn nullable-return? [cname mname]
  (boolean (some (fn [[c m]] (and (= m mname)
                                  (when-let [^Class owner (t/load-class c)]
                                    (when-let [^Class k (t/load-class cname)] (.isAssignableFrom owner k)))))
                 nullable-returns)))

(defn short-name [cname]
  (if (re-matches #"java\.lang\.[A-Z]\w*" cname) (subs cname 10) cname))
