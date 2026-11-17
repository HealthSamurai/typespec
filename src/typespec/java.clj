(ns typespec.java
  "Java members, read by reflection from classes that are loaded but never initialized."
  (:require [typespec.types :as t])
  (:import [java.lang.reflect Method Modifier]))

(defn methods-named
  "Public methods of class `cname` called `mname`: [{:params [class names] :ret class-name :declaring class-name}]."
  [cname mname static?]
  (when-let [^Class c (t/load-class cname)]
    (->> (.getMethods c)
         (filter (fn [^Method m] (and (= mname (.getName m)) (= (boolean static?) (Modifier/isStatic (.getModifiers m))))))
         (map (fn [^Method m] {:params (mapv #(.getName ^Class %) (.getParameterTypes m))
                               :varargs (.isVarArgs m)
                               :ret (.getName (.getReturnType m))
                               :declaring (.getName (.getDeclaringClass m))}))
         (sort-by (fn [m] [(count (:params m)) (if (= "java.lang.Object" (:ret m)) 1 0)]))
         (reduce (fn [acc m] (if (some #(= (:params %) (:params m)) acc) acc (conj acc m))) []))))

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
