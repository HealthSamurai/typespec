(ns typespec.main
  "Command line: check, sig, type-at."
  (:require [clojure.data.json :as json]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [typespec.parse :as parse]
            [typespec.types :as t]
            [typespec.check :as check]
            [typespec.externs :as externs]
            [typespec.contract-diff :as contract-diff]
            [typespec.project-contracts :as project-contracts])
  (:gen-class))

(defn source-files [paths]
  (->> paths
       (mapcat #(file-seq (io/file %)))
       (filter #(and (.isFile ^java.io.File %) (re-find #"\.cljc?$" (.getName ^java.io.File %))))
       (map str) sort))

(defn load-config [root]
  (let [f (io/file root "typespec.edn")]
    (if (.exists f) (edn/read-string (slurp f)) {})))

(defn run-check
  "Checks every source file under `paths` (relative to `root`). Options override typespec.edn."
  [root paths opts]
  (let [cfg (merge (load-config root) opts)
        paths (or (seq paths) (:paths cfg) ["src"])
        files (mapv parse/parse-file (source-files (map #(str (io/file root %)) paths)))
        prefix (str (.getPath (io/file root)) "/")
        rel #(if (str/starts-with? (str %) prefix) (subs (str %) (count prefix)) (str %))
        res (check/check-files files (assoc cfg :extern-contracts (externs/load-contracts root cfg)))]
    (cond-> (-> res
                (update :diagnostics (fn [ds] (mapv #(update % :file rel) ds)))
                (update :types (fn [ts] (mapv #(update % :file rel) ts)))
                (update :uses (fn [uses] (mapv #(update % :file rel) uses)))
                (update :spec-uses (fn [uses] (mapv #(update % :file rel) uses)))
                (assoc :files (count files)))
      (:track-uses cfg) (assoc :contracts (project-contracts/index res rel)))))

(defn format-diag [d]
  (format "%s:%d:%d  %s  %s%s" (:file d) (:row (:pos d)) (:col (:pos d)) (:code d) (:message d)
          (if (= :warning (:severity d)) "  [warning]" "")))

(defn diag->json [d]
  (cond-> {:id (:id d) :code (:code d) :severity (name (:severity d)) :file (:file d)
           :range {:start [(:row (:pos d)) (:col (:pos d))] :end [(:end-row (:pos d)) (:end-col (:pos d))]}
           :message (:message d)}
    (:reasons d) (assoc :reasons (vec (sort (map name (:reasons d)))))
    (:type-path d) (assoc :type-path (mapv #(if (keyword? %) (str %) %) (:type-path d)))
    (:fixes d) (assoc :fixes (:fixes d))))

(defn sig-lines [res ns-sym]
  (binding [t/*cur-ns* ns-sym]
    (let [vars (->> (:vars (:env res))
                    (filter #(= (str ns-sym) (namespace (key %))))
                    (sort-by #(vec ((juxt :row :col) (:pos (val %))))))
          width (apply max 0 (map #(count (name (key %))) vars))]
      (vec (for [[sym v] vars]
             (let [ty (get (:sigs res) sym (:type v))]
               (format (str "%-" width "s  %s   ; from %s") (name sym) (t/show ty)
                       (case (:source v)
                         :sig "t/sig" :fdef "s/fdef"
                         :protocol "defprotocol"
                         :extern (str "extern " (:extern/path v))
                         :hints (cond (some :inferred (:arities ty)) "the body"
                                      :else "hints")))))))))

(defn type-at [res file row col]
  (let [hits (filter (fn [{f :file p :pos}]
                       (and (= f file)
                            (<= (compare [(:row p) (:col p)] [row col]) 0)
                            (pos? (compare [(:end-row p) (:end-col p)] [row col]))))
                     (:types res))
        size (fn [{p :pos}] [(- (:end-row p) (:row p)) (- (:end-col p) (:col p))])]
    (when-let [h (first (sort-by size hits))]
      (binding [t/*cur-ns* (:ns h)] (t/show (:type h))))))

(defn run-contract-diff [base-root root paths opts]
  (let [snapshot (fn [path revision]
                   (let [root (.getCanonicalPath (io/file path))
                         result (run-check root paths (assoc opts :track-uses true))]
                     (when (zero? (:files result))
                       (throw (ex-info (str "no source files for " (name revision) " at " root) {})))
                     (when (some #(= "TSP722-01" (:code %)) (:diagnostics result))
                       (throw (ex-info (str "conflicting declarations in " (name revision) " at " root) {})))
                     (assoc result :root root)))
        before (snapshot base-root :baseline)
        after (snapshot root :current)]
    {:version 1
     :changes (contract-diff/compare-projects before after)
     :diagnostics {:before (:diagnostics before) :after (:diagnostics after)}}))

(defn- contracts-diff-cli [base-root root paths opts]
  (let [json? (:json opts)
        report (try
                 (when-not base-root
                   (throw (ex-info "contracts-diff requires a baseline project root" {})))
                 (run-contract-diff base-root root paths (select-keys opts [:strict :strict-nil]))
                 (catch Exception e {:version 1 :error (or (.getMessage e) (.getSimpleName (class e)))}))]
    (if json?
      (println (json/write-str (cond-> report
                                (:diagnostics report) (update :diagnostics #(update-vals % (fn [ds] (mapv diag->json ds)))))))
      (prn report))
    (cond (:error report) 2
          (some #(= :breaking (:status %)) (:changes report)) 1
          :else 0)))

(defn- parse-args [args]
  (loop [args args opts {} rest-args []]
    (if (empty? args)
      [opts rest-args]
      (let [a (first args)]
        (case a
          "--json" (recur (rest args) (assoc opts :json true) rest-args)
          "--strict" (recur (rest args) (assoc opts :strict true) rest-args)
          "--strict-nil" (recur (rest args) (assoc opts :strict-nil true) rest-args)
          "--root" (recur (drop 2 args) (assoc opts :root (second args)) rest-args)
          (recur (rest args) opts (conj rest-args a)))))))

(defn -main [& args]
  (let [[opts [cmd & more]] (parse-args args)
        root (:root opts ".")
        copts (select-keys opts [:strict :strict-nil])]
    (case cmd
      "check" (let [res (run-check root more copts)
                    ds (:diagnostics res)
                    errors (count (filter #(= :error (:severity %)) ds))]
                (if (:json opts)
                  (println (json/write-str {:version 1 :diagnostics (mapv diag->json ds) :trust (:trust res)}))
                  (do (run! #(println (format-diag %)) ds)
                      (println)
                      (let [{:keys [cast any ignore guard]} (:trust res)]
                        (println (str (format "%d error(s), %d warning(s) in %d file(s). Trust: %d cast, %d any, %d ignore."
                                              errors (- (count ds) errors) (:files res) cast any ignore)
                                      (when guard (format " %d trusted guard(s)." guard)))))))
                (shutdown-agents)
                (System/exit (if (pos? errors) 1 0)))
      "sig" (let [res (run-check root nil copts)]
              (run! println (sig-lines res (symbol (first more)))))
      "contracts" (let [res (run-check root more (assoc copts :track-uses true))
                        ds (:diagnostics res)
                        out {:version 1 :contracts (:contracts res) :diagnostics ds}]
                    (if (:json opts)
                      (println (json/write-str (assoc out :diagnostics (mapv diag->json ds))))
                      (prn out))
                    (shutdown-agents)
                    (System/exit (if (some #(= :error (:severity %)) ds) 1 0)))
      "contracts-diff" (let [[base-root & paths] more
                             status (contracts-diff-cli base-root root paths opts)]
                         (shutdown-agents)
                         (System/exit status))
      "type-at" (let [[file row col] (str/split (first more) #":")
                      res (run-check root nil copts)]
                  (println (or (type-at res file (parse-long row) (parse-long col)) "no expression here")))
      (do (println "usage: typespec check [paths] [--json] [--strict] [--strict-nil] [--root dir]")
          (println "       typespec contracts [paths] [--json] [--root dir]")
          (println "       typespec contracts-diff <baseline-root> [paths] [--json] [--root current-root]")
          (println "       typespec sig <ns> [--root dir]")
          (println "       typespec type-at <file>:<line>:<col> [--root dir]")))))
