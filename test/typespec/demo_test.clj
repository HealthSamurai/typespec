;; Runs the checker on the demo projects and compares the output with the expected files.
;; Update the expected files with: clojure -M:test --update
(ns typespec.demo-test
  (:require [clojure.data.json :as json]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [typespec.main :as main]))

(def cases
  [{:root "demo"          :expected "demo/expected.txt"}
   {:root "test/readme"   :expected "test/readme/expected.txt"}
   {:root "test/examples" :expected "test/examples/expected.txt"}
   {:root "test/features" :expected "test/features/expected.txt" :marked "test/features/src/f/core.clj"}
   {:root "demo"          :expected "demo/expected-strict-nil.txt" :opts {:strict-nil true}}])

(defn marker-problems
  "In a marked file, every line with an ERR or WARN comment must have a diagnostic, and no other line may."
  [{:keys [root marked]}]
  (let [res (main/run-check root nil {})
        got (set (map #(:row (:pos %)) (:diagnostics res)))
        want (set (keep-indexed (fn [i l] (when (re-find #";.*\b(ERR|WARN)\b" l) (inc i)))
                                (str/split-lines (slurp marked))))]
    (concat (for [l (sort (remove got want))] (str "line " l ": expected a diagnostic, got none"))
            (for [l (sort (remove want got))] (str "line " l ": unexpected diagnostic")))))

(defn actual [{:keys [root opts]}]
  (let [res (main/run-check root nil (or opts {}))]
    (str (str/join "\n" (map main/format-diag (:diagnostics res))) "\n")))

;; ---- feature cards: features/F*.md ----
;; A card with a ```clojure example block and a ```text output block is checked:
;; the checker runs on the example and its diagnostics must equal the output block.

(defn card-files []
  (->> (file-seq (io/file "features"))
       (filter #(re-matches #"F\d+-.*\.md" (.getName ^java.io.File %)))
       (sort-by #(.getName ^java.io.File %))))

(defn card-meta [text]
  (let [[_ header] (re-find #"(?s)\A---\n(.*?)\n---\n" text)]
    (into {} (for [l (str/split-lines (or header ""))
                   :let [[_ k v] (re-matches #"(\w+):\s*(.*)" l)] :when k]
               [(keyword k) v]))))

(def example-re #"(?s)```clojure example\n(.*?)```")
(def output-re #"(?s)```text output\n(.*?)```")
(def sig-re #"(?s)```text sig\n(.*?)```")

(defn card-run
  "Runs the checker on the example of a card. Returns {:output text :sig text}."
  [text]
  (let [m (card-meta text)
        root (.toFile (java.nio.file.Files/createTempDirectory
                       "typespec-card" (make-array java.nio.file.attribute.FileAttribute 0)))
        f (io/file root "src" "example.clj")
        flags (set (str/split (or (:flags m) "") #"[\s,]+"))]
    (io/make-parents f)
    (spit f (second (re-find example-re text)))
    (let [res (main/run-check (str root) nil {:strict (contains? flags "strict")
                                              :strict-nil (contains? flags "strict-nil")})
          line (fn [d] (format "%d:%d  %s  %s%s" (:row (:pos d)) (:col (:pos d)) (:code d) (:message d)
                               (if (= :warning (:severity d)) "  [warning]" "")))]
      {:output (if (seq (:diagnostics res))
                 (str (str/join "\n" (map line (:diagnostics res))) "\n")
                 "(no diagnostics)\n")
       :sig (str (str/join "\n" (main/sig-lines res 'example)) "\n")})))

(defn cards-index []
  (let [cards (for [f (card-files)] (assoc (card-meta (slurp f)) :file (.getName ^java.io.File f)))
        by-status (frequencies (map :status cards))]
    (str "# Feature cards\n\n"
         "Каталог Typespec: существующий Clojure, вывод типов, Java, данные, доверие и инструменты.\n\nОсновной маршрут: F110 → F120 → F510 → F310 → F220 → F230 → F210. Карточки сгруппированы по смыслу, а не по источнику идеи.\n\n| Группа | Тема |\n|---|---|\n| F1xx | Источники типов |\n| F2xx | Система типов и вывод |\n| F3xx | Данные и коллекции |\n| F4xx | Функции и диспетчеризация |\n| F5xx | Java |\n| F6xx | Макросы |\n| F7xx | Модули и библиотеки |\n| F8xx | Диагностика и доверие |\n| F9xx | CLI, редактор и агенты |\n\nНомера сгруппированы по сотням; десятки обозначают темы, единицы — детализацию. Есть резерв для новых карточек. Будущий код диагностики: `TSP<номер карточки>-<номер правила>`, например `TSP210-01`. После выпуска кодов ID постоянны: удалённые номера не переиспользуем. Номер группы не означает порядок реализации. Текущие TSP-коды пока не мигрированы.\n\n`status` — зрелость дизайна; `implementation` — реализация. Примеры без меток example/output иллюстративны, тестовый раннер их не исполняет.\n"
         "To discuss a feature, open an issue named after its card, for example `F240: narrowing and exhaustiveness`.\n\n"
         "| Status | Cards | Meaning |\n|---|---|---|\n"
         "| `implemented` | " (by-status "implemented" 0) " | works in the prototype; the example in the card is checked by `clojure -M:test` |\n"
         "| `partial` | " (by-status "partial" 0) " | design partly settled; see implementation for code status |\n"
         "| `designed` | " (by-status "designed" 0) " | accepted design; implementation status is separate |\n"
         "| `proposed` | " (by-status "proposed" 0) " | an idea under discussion, not yet accepted into the design |\n\n"
         "This file is generated from the card headers by `clojure -M:test --update`.\n\n"
         "| Card | Feature | Design | Implementation | Area | Origin |\n|---|---|---|---|---|---|\n"
         (str/join "\n" (for [c cards]
                          (str "| [" (:id c) "](" (:file c) ") | " (:title c) " | `" (:status c) "` | `" (:implementation c) "` | " (:area c) " | " (:origin c) " |")))
         "\n")))

(defn check-cards
  "Returns the number of failures. With update?, rewrites the output blocks and the index."
  [update?]
  (let [failed (atom 0)]
    (doseq [^java.io.File f (card-files)
            :let [text (slurp f)]
            :when (and (re-find example-re text) (re-find output-re text))]
      (let [{:keys [output sig]} (card-run text)
            fix (fn [text re tag body]
                  (if (re-find re text)
                    (str/replace-first text re (java.util.regex.Matcher/quoteReplacement (str "```text " tag "\n" body "```")))
                    text))
            want (-> text (fix output-re "output" output) (fix sig-re "sig" sig))]
        (cond
          update? (when (not= want text) (spit f want) (println "updated" (.getName f)))
          (= want text) nil
          :else (do (swap! failed inc)
                    (println "FAILED " (.getName f) "- the checker says:")
                    (print output)))))
    (let [idx (io/file "features" "README.md")]
      (cond
        update? (spit idx (cards-index))
        (not= (cards-index) (when (.exists idx) (slurp idx)))
        (do (swap! failed inc) (println "FAILED  features/README.md is out of date: run clojure -M:test --update"))))
    (when-not update?
      (println (if (zero? @failed) "ok     " "FAILED ") "feature cards:"
               (count (filter #(re-find output-re (slurp %)) (card-files))) "checked examples in"
               (count (card-files)) "cards"))
    @failed))

(defn -main [& args]
  (let [update? (some #{"--update"} args)
        failed (atom 0)]
    (doseq [c cases
            :let [out (actual c)
                  f (io/file (:expected c))]]
      (cond
        update? (do (spit f out) (println "updated" (:expected c)))
        (not (.exists f)) (do (swap! failed inc) (println "MISSING" (:expected c)))
        (and (:marked c) (seq (marker-problems c)))
        (do (swap! failed inc)
            (println "FAILED " (:marked c))
            (run! #(println "  " %) (marker-problems c)))
        (= out (slurp f)) (println "ok     " (:expected c) (str "(" (count (str/split-lines out)) " diagnostics)"))
        :else (let [exp (set (str/split-lines (slurp f))) act (set (str/split-lines out))]
                (swap! failed inc)
                (println "FAILED " (:expected c))
                (doseq [l (sort (remove act exp))] (println "  - missing:   " l))
                (doseq [l (sort (remove exp act))] (println "  + unexpected:" l)))))
    (swap! failed + (check-cards update?))
    (when-not update?
      (let [res (main/run-check "demo" nil {})
            strict (main/run-check "demo" nil {:strict true})
            json (json/read-str (json/write-str {:diagnostics (mapv main/diag->json (:diagnostics res))}) :key-fn keyword)
            checks
            [["sig: from s/fdef" (some #{"total          [:clinic/invoice :-> number?]   ; from s/fdef"} (main/sig-lines res 'clinic.billing))]
             ["sig: from :pre and the body" (some #{"with-tax       [:clinic/invoice number? :-> number?]   ; from the body"} (main/sig-lines res 'clinic.billing))]
             ["sig: from hints" (some #{"slug               [String long :-> String]   ; from hints"} (main/sig-lines res 'clinic.export))]
             ["type-at: before narrowing" (= "(s/nilable :clinic/patient)" (main/type-at res "src/clinic/patients.clj" 57 9))]
             ["type-at: after narrowing" (= ":clinic/patient" (main/type-at res "src/clinic/patients.clj" 59 17))]
             ["json: a safe fix for the reflective call"
              (= [{:title "Add hint ^java.io.File" :risk "safe"
                   :edits [{:range {:start [39 12] :end [39 12]} :text "^java.io.File "}]}]
                 (:fixes (first (filter #(= "TSP7023" (:code %)) (:diagnostics json)))))]
             ["trust counters" (= {:cast 1 :any 0 :ignore 2} (:trust res))]
             ["strict: public functions without a signature"
              (= 11 (count (filter #(= "TSP7010" (:code %)) (:diagnostics strict))))]]]
        (doseq [[label ok?] checks]
          (if ok? (println "ok     " label) (do (swap! failed inc) (println "FAILED " label))))))
    (shutdown-agents)
    (System/exit (if (pos? @failed) 1 0))))
