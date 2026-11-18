(ns typespec.externs
  "Inert EDN contracts. :externs is a vector of paths relative to the project root.
  Each file is a map: qualified var symbols -> signatures, keywords -> types.
  Built-in replacements require :externs-core :override; default is :error."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [typespec.types :as t]
            [typespec.core-types :as core]))

(def position {:row 1 :col 1 :end-row 1 :end-col 1})

(defn diagnostic [path code message]
  {:code code :severity :error :file (str path) :pos position :message message})

(defn- read-map [path]
  (with-open [r (java.io.PushbackReader. (io/reader path))]
    (let [eof (Object.)
          opts {:eof eof :readers {} :default (fn [tag _] (throw (ex-info (str "unsupported EDN tag " tag) {})))}
          data (edn/read opts r)]
      (when-not (map? data) (throw (ex-info "expected an EDN map" {})))
      (when-not (identical? eof (edn/read opts r))
        (throw (ex-info "expected exactly one EDN map" {})))
      data)))

(defn load-contracts
  "Reads extern files without requiring namespaces or evaluating forms.
  Returns {:vars :specs :diagnostics}; malformed entries are not installed."
  [root cfg]
  (let [diags (atom [])
        error! #(swap! diags conj (diagnostic %1 "TSP9003" %2))
        paths (:externs cfg [])
        policy (:externs-core cfg :error)
        valid? (and (vector? paths) (every? string? paths))
        result
        (if (and valid? (#{:error :override} policy))
          (reduce
           (fn [acc path]
             (let [f (if (.isAbsolute (io/file path)) (io/file path) (io/file root path))]
               (try
                 (reduce
                  (fn [acc [k form]]
                    (try
                      (when-not (or (qualified-symbol? k) (keyword? k))
                        (throw (ex-info "extern key must be a qualified symbol or keyword" {})))
                      (let [ctx {:aliases {} :imports {}
                                 :fail (fn [bad] (throw (ex-info (str "not a type: " (pr-str bad)) {})))}
                            ty ((if (symbol? k) t/parse-sig t/parse-type) ctx form)
                            bucket (if (symbol? k) :vars :specs)
                            old (get-in acc [bucket k])
                            builtin? (and (symbol? k) (or (= "clojure.core" (namespace k)) (contains? core/table k)))]
                        (cond
                          (and builtin? (not= :override policy))
                          (do (error! f (str "extern " k " requires :externs-core :override")) acc)
                          (and old (not= ty (:type old)))
                          (do (error! f (str "conflicting extern " k "; first declared in " (:extern/path old))) acc)
                          :else (assoc-in acc [bucket k]
                                          {:type ty :source :extern :extern/path path
                                           :file (str f) :pos position :complete? true})))
                      (catch Exception e
                        (error! f (str "invalid extern " (pr-str k) ": " (.getMessage e))) acc)))
                  acc (sort-by (comp pr-str key) (read-map f)))
                 (catch Exception e (error! f (str "cannot read extern: " (.getMessage e))) acc))))
           {:vars {} :specs {}} paths)
          (do (error! (io/file root "typespec.edn")
                      (if valid? "invalid :externs-core policy; use :error or :override"
                          ":externs must be a vector of path strings"))
              {:vars {} :specs {}}))]
    (assoc result :diagnostics @diags)))

(defn merge-contracts
  "Merges externs with source declarations. Source contracts win conflicts.
  An unannotated source function keeps its body and receives the extern signature."
  [env externs]
  (let [diags (atom (vec (:diagnostics externs)))
        merged (reduce
                (fn [env bucket]
                  (reduce-kv
                   (fn [env k ext]
                     (let [old (get-in env [bucket k])
                           source-contract? (and old (or (= bucket :specs) (not= :hints (:source old)) (:tagged? old)))]
                       (if source-contract?
                         (do (when (not= (:type old) (:type ext))
                               (swap! diags conj (diagnostic (:file ext) "TSP2790"
                                                            (str "extern " k " conflicts with source contract in " (:file old)))))
                             env)
                         (assoc-in env [bucket k]
                                   (if old (merge old (select-keys ext [:type :source :extern/path :complete?])) ext)))))
                   env (get externs bucket {})))
                env [:specs :vars])]
    (letfn [(missing [ty seen]
              (mapcat (fn [node]
                        (when (and (map? node) (= :ref (:t node)) (not (contains? seen (:name node))))
                          (if-let [target (get-in merged [:specs (:name node) :type])]
                            (missing target (conj seen (:name node)))
                            [(:name node)])))
                      (tree-seq coll? seq ty)))]
      (let [validated
            (reduce (fn [env bucket]
                      (reduce-kv
                       (fn [env k ext]
                         (let [refs (set (missing (:type ext) #{}))]
                           (if (seq refs)
                             (do (swap! diags conj (diagnostic (:file ext) "TSP9003"
                                                              (str "extern " k " refers to undefined types: "
                                                                   (pr-str (sort refs)))))
                                 (if (= :extern (get-in env [bucket k :source]))
                                   (assoc-in env [bucket k :complete?] false) env))
                             env)))
                       env (get externs bucket {})))
                    merged [:specs :vars])]
        [(assoc validated :key-nss (set (keep namespace (keys (:specs validated))))) @diags]))))
