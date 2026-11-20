# Registry: отдельный overlay контрактов

Не меняет baseline, checker или upstream. Подключается только явно через
`externs/load-contracts`; `:externs-core` остаётся `:error`, override не нужен.
Восемь функций, пять полей доменной карты; нет `t/any`, предположений об
инициализированном context, DB/GCS вызовов или фиктивных dependency stubs.

## Источники и границы

* `fhir-package-registry` pin `ea79b6a18d9ae759593c3824f502bc1200209e22`,
  `src/fhir/registry/semver.clj`, SHA256
  `3b44c5f13463146d8896a37d1375138fc671d0d26c04773f3028998a2c3c8f5b`.
* Фактический dependency JAR `org.clojure/clojure/1.12.4`,
  `clojure/java/io.clj`: Coercions (nil/String/File/URI/URL), IOFactory,
  default-streams-impl, extend Reader/Writer/InputStream/OutputStream,
  `file`, `reader`, `writer`, `output-stream`. Проверен исходник внутри JAR,
  не только docstrings. Контракты предполагают стандартные реализации
  протоколов, без пользовательского переопределения IOFactory/Coercions.

| Контракт | Основание и nullable/ошибки | Проверки |
|---|---|---|
| parse-semver | строки 4–24; nil → nil; иначе карта с обязательными major/minor/patch int, nullable pre-release vector<string>, nullable build string. Пустая строка и `.` дают major=0. Overflow/плохой формат может бросить исключение | nil, 1.2.3-rc.1+x, пустая строка, `.`, 1; число отвергается |
| compare-pre-release | 26–61; nullable vector<string> аргументы; int normal return, parseInt может бросить | nil/nil, rc/nil, число внутри vector отвергается |
| semver-comparator | 63–78; nullable strings, int normal return | nil/nil, 1/2, число отвергается |
| latest | 80–87; nullable collection nullable strings; nullable string result, пустая коллекция и [nil] могут дать nil | [], nil, [nil], [1-string 2-string], [42] отвергается |
| io/file | 423–435; nil единственным аргументом → nil; один поддержанный ненулевой аргумент → File; nullable parent + ненулевой child → File; неподходящие URI/URL/absolute child бросают | nil, string, nil+child; 42 отвергается |
| io/reader | 89–106 + IOFactory extensions; standard normal return BufferedReader, не nullable | StringReader in-memory; nil/42 отвергаются |
| io/writer | 108–125 + extensions; BufferedWriter, не nullable | StringWriter in-memory; nil/42 отвергаются |
| io/output-stream | 144–161 + extensions; BufferedOutputStream, не nullable | ByteArrayOutputStream in-memory; nil/42 отвергаются |

IO overlay намеренно ограничен вызовами без options; `file` — 1/2 аргумента.
Byte/char arrays, остальные arities/options не описаны. Это **не** утверждение,
что они недопустимы в Clojure. Extern заменяет сигнатуру целиком: применять этот
overlay глобально к другим проектам нельзя без расширения покрытия.
В проверенных 11 файлах новых arity/type errors не возникло.

NDJSON просмотрен на том же pin: `read-stream` имеет произвольный accumulator,
`write-ndjson-gz` возвращает результат callback, `write-stream-ndjson-gz` nil
после flush. Не выдаём им вымышленных map/vector контрактов: JSON включает
скаляры/null, callback требует полиморфизма. Сейчас точность NDJSON улучшает
контракт `io/writer`, без аннотации его API. Context не аннотирован.

Семантическая ошибка сортировки semver отдельно описана в
[fhir-registry-semver-bug.md](fhir-registry-semver-bug.md): int result не
доказывает правильность порядка; ошибка найдена runtime-пробой, не Typespec.

## Измерение

Все 11 исходных файлов читаются как текст и разбираются тем же parser;
зависимости на полном baseline classpath, JDK 27.0.1.1, Clojure 1.12.4.
Upstream namespaces после checker не загружены. Source bodies extern-функций
проверяются: это не trusted stub. Тесты с заведомо неверными телами дают errors.

* Без overlay: **2 errors, 121 warnings**.
* С overlay: **2 errors, 134 warnings**.
* По ключу file/pos/code/severity/message: **8 удалено, 21 добавлено**.
* Оба прежних errors сохранены; никаких новых errors.
* Это уточнение информации, **не снижение общего числа warnings**.

Полный список новых diagnostics (все warning):

| Файл:строка:колонка | Код | Содержание |
|---|---|---|
| registry.clj:144:29, 198:38 | TSP210-01 | unknown → nullable collection nullable strings для latest |
| registry.clj:720:41 | TSP210-01 | unknown → output-stream input union |
| registry.clj:721:9 | TSP7023 | receiver out теперь BufferedOutputStream |
| registry.clj:734:56 | TSP210-01 | unknown → reader input union |
| registry.clj:736:28 | TSP7023 | io/file receiver теперь File |
| registry/index.clj:105:52, 300:52 | TSP210-01 | unknown → nil? при выборе первого overload file |
| registry/index.clj:105:43, 300:43 | TSP210-01 | unknown → output-stream input union |
| registry/legacy.clj:39:55 | TSP210-01 | unknown → output-stream input union |
| registry/legacy.clj:46:21 | TSP210-01 | unknown → nil? при выборе первого overload file |
| registry/ndjson.clj:12:26 | TSP210-01 | unknown → output-stream input union |
| registry/ndjson.clj:13:33, 13:51, 18:33, 18:51, 20:5 | TSP7023 | writer receiver теперь BufferedWriter |
| registry/semver.clj:28:3, 67:3 | TSP210-01 | return unknown → int? |
| registry/semver.clj:81:3 | TSP210-01 | return unknown → nullable string |

`unknown → nil?` — ограничение выбора overload, не доказательство nil.
7 receiver diagnostics заменены более точными known-class diagnostics;
рефлексия не исчезает, так как upstream не изменён. Оставшиеся unknown честно
видны. Новые проверки возвращаемых значений semver пока не доказывают тела.

## Воспроизведение

Из корня typespec, после штатной подготовки baseline (`prepare.sh`):

```sh
CP=$(cat examples/fhir-registry-check/resolution-classpath.txt)
JAVA=/opt/homebrew/opt/openjdk/bin/java
"$JAVA" -cp "$CP:test" clojure.main -e "(require 'typespec.fhir-registry-contracts-test)(clojure.test/run-tests 'typespec.fhir-registry-contracts-test)(shutdown-agents)"
"$JAVA" -cp "$CP" clojure.main - <<'CLJ'
(require '[typespec.parse :as p] '[typespec.check :as c]
         '[typespec.externs :as e] '[clojure.edn :as edn] '[clojure.set :as s])
(let [manifest (edn/read-string (slurp "examples/fhir-registry-check/manifest.edn"))
      files (mapv (fn [f] (p/parse-string f (slurp (str ".hyper/tmp/ceya/upstream/" f))))
                  (sort (keys manifest)))
      contracts (e/load-contracts "." {:externs ["examples/fhir-registry-check/externs.edn"]})
      before (:diagnostics (c/check-files files {}))
      after (:diagnostics (c/check-files files {:extern-contracts contracts}))
      ds #(set (map (fn [d] (select-keys d [:file :pos :code :severity :message])) %))]
  (prn :extern-errors (:diagnostics contracts))
  (prn :before (frequencies (map :severity before)) :after (frequencies (map :severity after)))
  (prn :added (s/difference (ds after) (ds before)))
  (prn :removed (s/difference (ds before) (ds after)))
  (prn :upstream-loaded (keep #(when (find-ns (:ns %)) (:ns %)) files)))
(shutdown-agents)
CLJ
```

Тесты: 6 tests, 61 assertions. Runtime-проба читает только четыре проверенные
pure semver defn, SHA проверяется перед eval тех же bytes; временный namespace
удаляется. Upstream require отсутствует. Нет сетевых/DB/cloud операций;
IO runtime-пробы только in-memory. При отсутствии checkout тест явно падает
с prerequisite, не выдаёт ложный зелёный SKIP. Этот интеграционный namespace
не добавлен в default test runner.
