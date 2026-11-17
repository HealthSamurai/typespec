(ns typespec.main
  "Command line: check, sig, type-at."
  (:require [clojure.data.json :as json]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [typespec.parse :as parse]
            [typespec.types :as t]
            [typespec.check :as check]))

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
        res (check/check-files files cfg)]
    (-> res
        (update :diagnostics (fn [ds] (mapv #(update % :file rel) ds)))
        (update :types (fn [ts] (mapv #(update % :file rel) ts)))
        (assoc :files (count files)))))

(defn format-diag [d]
  (format "%s:%d:%d  %s  %s%s" (:file d) (:row (:pos d)) (:col (:pos d)) (:code d) (:message d)
          (if (= :warning (:severity d)) "  [warning]" "")))

(defn diag->json [d]
  (cond-> {:id (:id d) :code (:code d) :severity (name (:severity d)) :file (:file d)
           :range {:start [(:row (:pos d)) (:col (:pos d))] :end [(:end-row (:pos d)) (:end-col (:pos d))]}
           :message (:message d)}
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
                      (println (format "%d error(s), %d warning(s) in %d file(s). Trust: %d cast, %d any, %d ignore."
                                       errors (- (count ds) errors) (:files res)
                                       (:cast (:trust res)) (:any (:trust res)) (:ignore (:trust res))))))
                (shutdown-agents)
                (System/exit (if (pos? errors) 1 0)))
      "sig" (let [res (run-check root nil copts)]
              (run! println (sig-lines res (symbol (first more)))))
      "type-at" (let [[file row col] (str/split (first more) #":")
                      res (run-check root nil copts)]
                  (println (or (type-at res file (parse-long row) (parse-long col)) "no expression here")))
      (do (println "usage: typespec check [paths] [--json] [--strict] [--strict-nil] [--root dir]")
          (println "       typespec sig <ns> [--root dir]")
          (println "       typespec type-at <file>:<line>:<col> [--root dir]")))))
