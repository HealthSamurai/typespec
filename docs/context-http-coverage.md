# context-clj/http: границы покрытия Typespec

## Источник и рамки

Аудит исходников pin [`e158b6b244c2b8e053de2a097d1071d9419adef9`](https://github.com/context-clj/http/tree/e158b6b244c2b8e053de2a097d1071d9419adef9/src). GitHub API подтвердил SHA, сообщение `Exclude jetty cuz vulnerabilities`, дату 2026-03-19. Чтение — существующий gitlibs checkout с этим HEAD/origin; upstream namespaces не загружались, сервер не запускался. Проверены восемь src-файлов и ветки checker, влияющие на их конструкции. Baseline: `examples/context-http-check/{baseline.txt,diagnostics.edn,summary.edn}`; 8/8 parsed, 0 failed, 7 errors + 44 warnings. Семь ошибок отдельно исследует другой исполнитель; здесь не объявляются ошибками HTTP.

**8 прочитанных файлов не означают 8 полностью проверенных тел.** `check.clj:1578–1604` вызывает infer для top-level forms; дальнейший обход зависит от special rules. Счётчики opaque AST nodes не учитывают все пропущенные тела (например future представлен обычным list). Trust cast/any/ignore тоже не является показателем полноты.

## Полный inventory 44 предупреждений baseline

Позиции `строка:колонка`, относительно pinned `src/`. Для каждого файла первоисточник: [http.clj](https://github.com/context-clj/http/blob/e158b6b244c2b8e053de2a097d1071d9419adef9/src/http.clj), [formats](https://github.com/context-clj/http/blob/e158b6b244c2b8e053de2a097d1071d9419adef9/src/http/formats.clj), [mcp](https://github.com/context-clj/http/blob/e158b6b244c2b8e053de2a097d1071d9419adef9/src/http/mcp.clj), [openapi](https://github.com/context-clj/http/blob/e158b6b244c2b8e053de2a097d1071d9419adef9/src/http/openapi.clj), [routing](https://github.com/context-clj/http/blob/e158b6b244c2b8e053de2a097d1071d9419adef9/src/http/routing.clj), [uui](https://github.com/context-clj/http/blob/e158b6b244c2b8e053de2a097d1071d9419adef9/src/uui.clj), [heroicons](https://github.com/context-clj/http/blob/e158b6b244c2b8e053de2a097d1071d9419adef9/src/uui/heroicons.clj), [menu](https://github.com/context-clj/http/blob/e158b6b244c2b8e053de2a097d1071d9419adef9/src/uui/menu.clj).

| Код | Файл | Все позиции | Количество |
|---|---|---|---:|
| TSP210-01 | http.clj | 31:20, 50:33, 53:33, 56:37, 57:37, 60:35, 73:73, 103:32, 141:41, 313:61, 351:31, 395:50 | 12 |
| TSP210-01 | http/formats.clj | 138:26, 204:19, 207:58, 214:28, 221:29, 322:56 | 6 |
| TSP210-01 | http/mcp.clj | 74:67 | 1 |
| TSP210-01 | http/routing.clj | 5:45, 7:42, 46:72, 60:133, 61:112, 67:104, 74:104, 77:100, 86:42, 126:46 | 10 |
| TSP210-01 | uui.clj | 29:39, 31:22, 98:36, 136:9, 147:9 | 5 |
| TSP210-01 | uui/heroicons.clj | 25:51 | 1 |
| TSP9002 | http.clj | 111:17, 113:21 | 2 |
| TSP7031 | http.clj | 172:6 | 1 |
| TSP7031 | http/openapi.clj | 10:4 | 1 |
| TSP7031 | uui.clj | 45:4, 46:4, 47:4, 48:4 | 4 |
| TSP7023 | http/formats.clj | 309:13 | 1 |

Классификация, без неподтверждённых false-positive/bug заявлений:

- **35 × TSP210-01 — неполная информация.** Во всех сообщениях actual `t/unknown`, reason `:unknown`. Checker не доказал совместимость с ожидаемым string/CharSequence/name-like/number/map. Это не 35 доказанных ошибок программы и не 35 доказанных ложных срабатываний. Например routing arithmetic получает неизвестные значения, UI serialization — неизвестный результат внешней функции, `uui/get-fn` принимает неизвестную строку. Для разделения «не хватает контракта» и «не хватает inference» нужны отдельные минимальные тесты.
- **2 × TSP9002 — непрозрачные spec-предикаты:** `http:111` проверяет множество HTTP verbs через `contains?`; `http:113` принимает list/vector. Checker заменяет эти предикаты unknown (`env.clj:125–140`), но это ничего не говорит о runtime-валидности самих spec.
- **6 × TSP7031 — известный исходникам, но не описанный макрос `http/register-endpoint`.** Его определение `http:148–153` видно env; вызовы перечислены выше. Checker предупреждает и не обходит аргументы/тело вызова. Это реальные ограничения проверки, не ошибки приложения.
- **1 × TSP7023 — reflection advisory:** `http/formats:309` Java interop над body, тип которого checker знает как InputStream. Это не ошибка assignability и не доказательство исключения в runtime.
- `uui/menu.clj`: 0 diagnostics. Это не доказательство правильности: endpoint state/динамические значения и формы UI не имеют полной схемы.

## Реальные пропуски и ограничения: конкретные места

### Silent skip в известных core macros

`check.clj:1433–1435`: известный stdlib macro без special rule возвращает unknown **без обхода тела и без warning**.

- `http/notify-on-request`, `http.clj:263–267`: всё тело `future` (265), включая вызов subscriber `f`, пропускается.
- `http/stream`, `http.clj:353–375`: `server/with-channel` — отдельный случай ниже, но вложенный `future` (356) точно пропускается. Не проверяются send/Java writer/compression/callback/cleanup внутри него.
- `assert` в registration functions (`http/mcp.clj:59–60`, `http.clj:273,293`) не создаёт refinement или доказанный контракт на callback.

`check.clj:1136–1148`: `cond->` / `cond->>` проверяют начальное выражение и tests, но **не threaded forms**, результат unknown:
- `http/dispatch`, `http.clj:323–324`: `(authenticate enriched-req)` внутри cond-> не проверяется как вызов.
- `http/formats.clj:240–241`: threaded операция в выборе content type не проверяется.

### Известные и неизвестные внешние макросы — не одно и то же

- `http/register-endpoint` объявлен в проверяемом source: `env.clj:157–158` регистрирует его, `check.clj:1446–1448` выдаёт TSP7031 и пропускает содержимое. Определение `defmacro` само также opaque (`check.clj:877–879`), syntax quote не анализируется как expansion. Runtime `s/conform` внутри macro не становится доказательством проверки вызова.
- **Lifecycle НЕ следует описывать как полностью skipped.** `system/defmanifest`, `system/defstart`, `system/defstop` в `http:411,424,437`, `http/mcp:9,121,126`, `http/openapi:9,13`, `uui:13,50` отсутствуют в окружении восьми файлов как macro definitions. При отсутствии extern/contract они идут через fallback неизвестного вызова (`check.clj:1450–1453`), аргументы рекурсивно обходятся, но binding vector не вводит locals, macro expansion и создаваемые функции не моделируются. Поэтому отсутствие ошибок не доказывает правильные типы context/config/state. Добавление dependency source может изменить поведение: макрос станет известным и получит TSP7031 без body contract.
- Аналогично `org.httpkit.server/with-channel` в `http:354` и `http/mcp:29`: аргументы могут обходиться, но `chan`/`channel` не получают корректную binding semantics. В mcp callback выражения могут проверяться; нельзя объявлять весь mcp body пропущенным. Hiccup `html` (`uui:16`) тоже не гарантированно распознан как внешний макрос.

### defmethod: тела проверяются, dispatch-контракт — нет

`check.clj:1109–1116` явно обходит тела `defmethod`, связывая fixed params через `env/pattern-type`. Это относится к `http/formats:67,72,78,82,85,102,106,118,122,131,135,141,145,154,166` и `http/mcp:40,44,53,92,97`. Нельзя писать, что все defmethod пропущены. Но `defmulti` сам opaque (`check.clj:877`): нет проверки согласованности dispatch value, совокупности методов, общего argument/return контракта. `binding` внутри format methods поддержан (`check.clj:1118–1124`), его тело тоже не пропускается.

### Обход есть, но неизвестность ослабляет результат

- `http/dispatch:338–342`: `((apply comp middleware) f)` и затем `(handler ...)` — динамическая композиция. Для nonsymbol call head checker обходит выражения, но возвращает unknown (`check.clj:1415`); неизвестная local function также не даёт проверки signature (`1416–1419`).
- `http:162–172`, `http/mcp:67–79`: `ns-interns` + metadata + Vars динамически собирают endpoints/tools. Нет доказательства, что runtime registry содержит обработчик правильной формы.
- `uui:28–38`: `require`/`resolve` по строке и последующий вызов `f` — тип вызываемой функции статически не установлен.
- `http/mcp:97–103`: handler из system state вызывается по строковому tool name; обход тела метода не доказывает контракт выбранного handler.
- `http/routing`: рекурсивные маршруты, deref Vars и разнородные структуры остаются частично неизвестными (10 предупреждений выше).
- `uui/heroicons:13–27`: cache atom, resource lookup, slurp, raw HTML; сотни однотипных wrapper defn ниже используют apply. Число таких функций не означает аналогичное число доказанных typed boundaries.
- `uui/menu:9–17,22–43`: system state и endpoint maps без полного контракта; `for` поддержан (`check.clj:1098`), не следует объявлять его skipped.
- `comment` blocks, Var quote и macro definitions не исполняются и не проверяются как обычные expression bodies. Это ожидаемое ограничение, но включать их строки в процент coverage неверно.

## Минимальные проверенные воспроизведения (checker-only)

Запущены через `typespec.parse/parse-string` + `typespec.check/check-files`, без loading HTTP namespaces, externs или contracts:

```clojure
(ns probe)
(inc "bad")                         ; TSP2345
(future (inc "bad"))                ; [] — тело silently skipped
(cond-> 1 true (+ "bad"))           ; [] — threaded form skipped
```

Контроль спорных утверждений:

```clojure
(ns probe (:require [system]))
(system/defstart [context config] (inc "bad")) ; TSP2345, тело traversed fallback
;; В отдельном source:
(ns probe)
(defmulti f identity)
(defmethod f :x [x] (inc "bad"))     ; TSP2345, тело проверено
```

Самые полезные ограниченные follow-up: добавить coverage marker для silent core-macro skip; отдельный тест inference для cond-> threaded calls; описывать внешние binding macros только честным явно доверенным контрактом. Это направления работы, не внесённые исправления. Общие выводы об ошибках HTTP по отсутствию diagnostics недопустимы.
