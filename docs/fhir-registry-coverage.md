# FHIR registry: предупреждения и пределы покрытия

## Рамки

Read-only аудит baseline `examples/fhir-registry-check/baseline-initial/{diagnostics,summary}.edn`: **121 warning = 72 TSP210-01 + 43 TSP7020 + 6 TSP7023**. Проверены все 11 файлов `src/`, 2405 строк, 11 parsed, 0 failed, 0 opaque nodes; это НЕ означает проверку всех тел. Источник — [fhir-clj/fhir-package-registry, pin ea79b6a18d9ae759593c3824f502bc1200209e22](https://github.com/fhir-clj/fhir-package-registry/tree/ea79b6a18d9ae759593c3824f502bc1200209e22/src), локально `.hyper/tmp/ceya/upstream`. Dependency source в эти 11 файлов не входит. Runtime app, GCS, DB не запускались; проверочные примеры используют только Typespec parser/checker.

**Ни одно предупреждение здесь не объявляется багом приложения.** TSP210-01 означает неизвестный тип на проверяемой границе; TSP7020 — неизвестный тип receiver Java; TSP7023 — checker знает класс, но компилятору может понадобиться hint. Это три разных вида ограничения/рефлексии, а не 121 подтверждённая ошибка. Два baseline error вынесены в `fhir-registry-context-triage.md` и `fhir-registry-java-triage.md`.

## Полный inventory

Позиции — `строка:колонка` начала diagnostic (полные end positions и ID сохранены в исходном EDN). Файлы относительно `src/fhir/`; ссылка на каждом файле ведёт к pinned исходнику. Несколько позиций одной строки означают отдельные диагностики, не дублирование подсчёта.

| Файл | Код | N | Все позиции |
|---|---|---:|---|
| [registry.clj](https://github.com/fhir-clj/fhir-package-registry/blob/ea79b6a18d9ae759593c3824f502bc1200209e22/src/fhir/registry.clj) | TSP210-01 | 32 | 37:18, 40:22, 89:36, 100:18, 100:24, 100:41, 128:71, 129:90, 219:70, 220:89, 342:93, 344:99, 469:31, 492:43, 495:45, 497:49, 499:48, 501:50, 503:53, 508:19, 508:68, 509:19, 510:41, 510:46, 510:52, 632:11, 667:66, 668:66, 672:108, 750:69, 753:65, 813:39 |
| [registry.clj](https://github.com/fhir-clj/fhir-package-registry/blob/ea79b6a18d9ae759593c3824f502bc1200209e22/src/fhir/registry.clj) | TSP7020 | 3 | 721:9, 736:28, 813:39 |
| [registry/database.clj](https://github.com/fhir-clj/fhir-package-registry/blob/ea79b6a18d9ae759593c3824f502bc1200209e22/src/fhir/registry/database.clj) | TSP210-01 | 4 | 28:29, 28:38, 36:106, 40:80 |
| [registry/database.clj](https://github.com/fhir-clj/fhir-package-registry/blob/ea79b6a18d9ae759593c3824f502bc1200209e22/src/fhir/registry/database.clj) | TSP7020 | 2 | 98:20, 99:24 |
| [registry/gcs.clj](https://github.com/fhir-clj/fhir-package-registry/blob/ea79b6a18d9ae759593c3824f502bc1200209e22/src/fhir/registry/gcs.clj) | TSP7020 | 23 | 49:7, 57:5, 63:16, 64:35, 65:26, 66:36, 67:19, 70:22, 72:5, 79:16, 98:13, 110:33, 113:5, 117:5, 117:22, 121:39, 162:57, 163:45, 168:40, 171:40, 179:15, 184:64, 186:61 |
| [registry/gcs.clj](https://github.com/fhir-clj/fhir-package-registry/blob/ea79b6a18d9ae759593c3824f502bc1200209e22/src/fhir/registry/gcs.clj) | TSP7023 | 2 | 108:23, 179:5 |
| [registry/gcs.clj](https://github.com/fhir-clj/fhir-package-registry/blob/ea79b6a18d9ae759593c3824f502bc1200209e22/src/fhir/registry/gcs.clj) | TSP210-01 | 8 | 160:25, 167:25, 168:40, 175:36, 178:55, 184:35, 185:26, 186:61 |
| [registry/healthsamurai.clj](https://github.com/fhir-clj/fhir-package-registry/blob/ea79b6a18d9ae759593c3824f502bc1200209e22/src/fhir/registry/healthsamurai.clj) | TSP210-01 | 1 | 13:37 |
| [registry/healthsamurai.clj](https://github.com/fhir-clj/fhir-package-registry/blob/ea79b6a18d9ae759593c3824f502bc1200209e22/src/fhir/registry/healthsamurai.clj) | TSP7020 | 1 | 27:49 |
| [registry/index.clj](https://github.com/fhir-clj/fhir-package-registry/blob/ea79b6a18d9ae759593c3824f502bc1200209e22/src/fhir/registry/index.clj) | TSP210-01 | 24 | 31:37, 31:41, 110:34, 111:40, 116:47, 162:30, 221:72, 250:42, 271:47, 345:51, 348:65, 348:85, 362:45, 363:46, 365:44, 366:45, 369:45, 386:34, 390:41, 391:44, 402:30, 404:36, 412:31, 414:41 |
| [registry/index.clj](https://github.com/fhir-clj/fhir-package-registry/blob/ea79b6a18d9ae759593c3824f502bc1200209e22/src/fhir/registry/index.clj) | TSP7020 | 8 | 111:40, 333:20, 334:24, 363:46, 366:45, 389:27, 404:36, 414:41 |
| [registry/legacy.clj](https://github.com/fhir-clj/fhir-package-registry/blob/ea79b6a18d9ae759593c3824f502bc1200209e22/src/fhir/registry/legacy.clj) | TSP210-01 | 1 | 18:34 |
| [registry/legacy.clj](https://github.com/fhir-clj/fhir-package-registry/blob/ea79b6a18d9ae759593c3824f502bc1200209e22/src/fhir/registry/legacy.clj) | TSP7020 | 1 | 46:3 |
| [registry/ndjson.clj](https://github.com/fhir-clj/fhir-package-registry/blob/ea79b6a18d9ae759593c3824f502bc1200209e22/src/fhir/registry/ndjson.clj) | TSP7020 | 5 | 13:33, 13:51, 18:33, 18:51, 20:5 |
| [registry/ndjson.clj](https://github.com/fhir-clj/fhir-package-registry/blob/ea79b6a18d9ae759593c3824f502bc1200209e22/src/fhir/registry/ndjson.clj) | TSP7023 | 4 | 27:17, 33:18, 41:17, 46:18 |
| [registry/packages2.clj](https://github.com/fhir-clj/fhir-package-registry/blob/ea79b6a18d9ae759593c3824f502bc1200209e22/src/fhir/registry/packages2.clj) | TSP210-01 | 1 | 9:19 |
| [registry/semver.clj](https://github.com/fhir-clj/fhir-package-registry/blob/ea79b6a18d9ae759593c3824f502bc1200209e22/src/fhir/registry/semver.clj) | TSP210-01 | 1 | 9:43 |

## Операции за предупреждениями

Ссылки/номера ниже относятся к тем же pinned файлам выше; inventory связывает операции с точными колонками.

- **registry.clj, 32 unknown boundaries:** headers → `str/includes?` (37,40); имя вкладки (89); `subs/min` для обрезки текста (100, три границы); query strings → `blank?/replace` (128–129,219–220,342–344,469,492–503); числовая пагинация (508–510); collection/reduce (632); форматирование путей и версий `name/join` (667–672); dependency/package names (750,753); timestamp → `format-relative-time` (813). **3 unknown receivers:** запись декодированных байтов (721), File.exists после io/file (736), SQL created_at.toInstant (813).
- **database.clj, 4 boundaries:** два аргумента сравнения LSN (28), два `name` для dependency name (36,40). **2 receivers:** Thread.getName/interrupt после обхода map keys (98–99). Это потеря типа элементов, не доказательство неправильного Thread.
- **gcs.clj, 8 boundaries:** DEFAULT_BUCKET в objects/output-stream (160,167,178,184), blob name → ends-with?/split (168,186), callback/acc reduce-tar и reduce (175,185). **23 unknown receivers:** builder chain/getService (49), service.get (57,98), Bucket.list и Page.getValues/getNextPage (63–72,79), copy-builder.setSource/service.copy/delete (110,113,117), Blob.reader (121), Blob name/content-type/content (162–171), content.getBytes (179), сортировка/разбор имени Blob (184,186). **2 known reflective:** BlobInfo.getBlobId (108), OutputStream.write (179). Ошибка builder-chain отдельно исследована; соседнее warning не считается дополнительным багом.
- **healthsamurai.clj:** split title (13), unknown HTTP body receiver getBytes (27).
- **index.clj, 24 boundaries:** idx/start-from арифметика (31); DEFAULT_BUCKET (110,362,365,386,402,412); blob/file/package string transformations (111,116,162,271,345,348,363,366,369,390,391,404,414); version name (221); LSN inc (250). **8 receivers:** Blob.getName (111,363,366,389,404,414), Thread.getName/interrupt (333–334).
- **legacy.clj:** split package string (18), File.exists после io/file (46).
- **ndjson.clj:** **5 unknown receivers** writer.write/flush (13,18,20); **4 known reflective** BufferedReader.readLine (27,33,41,46). Известный BufferedReader не означает неизвестный метод: это предупреждение о compiler-visible hint.
- **packages2.clj:** HTTP response body → str/split (9). **semver.clj:** input version string → str/split (9).
- **tar.clj:** нет baseline diagnostics; это не доказательство полной корректности. **diff.clj:** warning нет, отдельный error TSP2305.

## Конкретные пробелы обхода

Текущие ссылки на checker: `src/typespec/check.clj:1573–1595` (threading), `1611–1620` (defmethod), `1622–1634` (binding forms/doto), `1640–1650` (cond->), `2005–2021` (core/external macros). Номера checker относятся к рабочему snapshot аудита, не к upstream pin.

1. **`time` полностью скрывает body.** AST-инвентаризация всех 11 src, без обхода comment/quote/ns, нашла time в index.clj **90,147,157,179,193,285,305**, gcs.clj **194,200,238**. В частности не проверяются load-canonicals (index 179–188), current-packages-from-tgz (193–203), re-index с SQL truncate/upsert (305–321), GCS package re-index (194–215), index-ndjson (238–261). Вложенный time gcs:200 уже скрыт внешним :194: это не две независимые проверенные области. Это особенно существенно для интерпретации небольшого числа предупреждений.
2. **`lazy-seq` gcs:71** скрывает concat/getValues/recursive lazy-page body. `assert` gcs:99 также без special rule: condition/message не обходятся. Это coverage limits, не заявление о дефекте assert в приложении.
3. **`cond->` проверяет initial expression и predicates, но НЕ threaded steps** (`check.clj:1640–1650`). Активные upstream occurrences: registry.clj **482,485,491**, gcs.clj **62,78**, legacy.clj **23**. Значит conj extensions / assoc / SQL clauses / prefix options могут оставаться за границей проверки. `blank?` warnings на registry:492–503 подтверждают обход predicates, но не тел assoc.
4. **`future` в активном registry src не найден.** Его silent body skip подтверждён контрольным примером и прежним `docs/context-http-coverage.md`; нельзя приписывать registry несуществующий future. В этом repo существеннее time/lazy-seq.
5. **Lifecycle macros не следует автоматически называть skipped.** `system/defstart` registry:816, database:106, gcs:266, index:428; defstop database:113/index:437. В данном source-only scope definition system macros не включено в checker env; действует fallback обхода аргументов и body. Неправильный inc внутри обнаруживается. Но локальные bindings context/config не получают настоящей семантики macro, lifecycle/state contract не доказан. Для реально известных env macros без contract есть отдельная ветка TSP7031; в baseline такого warning нет.
6. **Обычный `->`/`->>` поддержан:** checker строит synthetic calls, и ошибки внутри обнаруживаются. Нельзя объяснять все Java unknown «пропущенным threading». `some->`, `as->`, `for`, `doseq`, `with-open`, `defmethod` тоже имеют rules; наличие rule не обещает точного типа результата. Отдельное ограничение doto: аргументы обходятся, сами методы не проверяются; registry active doto не найден.
7. **`comment`, reader discard `#_`, macro definitions и quoted code не равны исполняемому production body.** Они не служат coverage доказательством. В частности registry context в comment не следует считать обычной выполненной инициализацией; подробности в отдельном context triage. Нулевой opaque count считает AST форму, а не полноту infer-body.

## Минимальные checker-only проверки

Запущено из Typespec root: JDK 27, `clojure -M -e ...`; require только `typespec.parse`, `typespec.check`. Для каждой строки — отдельный `check/check-files [(parse/parse-string "probe.clj" (str "(ns probe (:require [system])) " source))] {}`. System/app namespace не require/eval: ns declaration является текстом parser input.

| Source | Полученные codes |
|---|---|
| `(inc "bad")` | TSP2345 |
| `(future (inc "bad"))` | [] |
| `(time (inc "bad"))` | [] |
| `(cond-> 1 true (+ "bad"))` | [] |
| `(cond-> 1 (inc "bad") inc)` | TSP2345 |
| `(-> 1 (+ "bad"))` | TSP2345 |
| `(->> 1 (+ "bad"))` | TSP2345 |
| `(doto (StringBuilder.) (.notAMethod))` | [] |
| `(system/defstart [context config] (inc "bad"))` | TSP2345 |
| `(system/defstop [context] (inc "bad"))` | TSP2345 |

Итого: source parsing полный в заявленном manifest; type coverage частичное, без честного процентного знаменателя. 121 warnings указывают на непроверенные границы/рефлексию, а дополнительные silent skips не входят в эти 121. Предыдущий HTTP audit подтверждает классы ограничений, но его строки checker устарели; выводы выше сверены с текущим checker и конкретными registry источниками. Код и baseline не менялись, commit не делался.
