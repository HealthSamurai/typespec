# Проверки

## Проверка маршрутов F280

Статическая проверка сверяет GET и POST маршруты со spec `::route`.
`type-at` на мапе GET-маршрута сохраняет `:method :get`. Отрицательный
маршрут даёт три диагностики для отсутствующего пути, недопустимого
метода и опечатки в ключе. Всего в каталоге `negative/` девять
ожидаемых ошибок.

Unit-тест вызывает `todo.app/start` с подставленным запросом к базе и
перехватывает вызовы регистрации после runtime-проверки `http/register-endpoint`.
В регистрацию попадают обе исходные мапы с Vars обработчиков.

## Регрессия F240, F270 и F220

Проверка всех четырёх файлов приложения дала 0 ошибок и 0 предупреждений.
`clojure -M:check src negative` завершился с кодом 1 и пятью ожидаемыми
диагностиками. `TSP8001` приходится на пропущенный вариант `case`,
`TSP270-02` — на неизвестное поле spec ответа, `TSP2345` — на
неверный тип параметра callback и два вызова с числовым title.
Оба статических прогона входят в `clojure -M:test` Typespec.

Unit-тесты приложения на закреплённых зависимостях прошли (3 теста,
22 проверки). В этом прогоне HTTP и PostgreSQL не поднимались.

## Обновление после независимого review

Контракт `system/defstart` уточнён: config — `(t/or map? nil?)`. Проверено запуском закреплённого `context/system` без БД и HTTP: модуль, чей манифест не объявляет `:config`, получает `nil` (так устроен и `todo.app`). Статический повтор на исправленном checker: `clojure -M:check` — 0 errors / 0 warnings, exit 0; `clojure -M:check src negative` — exit 1, два TSP2345. Общий suite Typespec: 88 тестов / 471 assertions, golden/CLI PASS. Runtime-тесты с PostgreSQL и HTTP после этого изменения не перезапускались: `typespec.edn` читает только checker. Находки и исправления: `docs/review-fable-5.2.md`.

## Обновление после refs/maps и lifecycle-контрактов

Статический повтор: `clojure -M:check` — 0 errors / 0 warnings; `clojure -M:check src negative` — ожидаемый exit 1, два TSP2345 (числовой title в обычном вызове и внутри defstart). Response body теперь явный union Todo/vector Todo/error вместо any?. Контракты defstart/defstop заданы в typespec.edn, тело проверяется без macroexpand. `defstop` допускает nil state согласно upstream.

Общий suite Typespec: 75 тестов / 386 assertions, golden/CLI PASS. Финальный независимый runtime-повтор после этих изменений прошёл: PostgreSQL 18.6 на 55449, HTTP на 18091 — integration 1/7 и unit 3/22, без ошибок. HTTP, datasource и временный кластер остановлены; весь shell job завершился exit 0. Следующие разделы сохраняют историю первого запуска: их три предупреждения уже устранены расширением проверок и уточнением контрактов, не suppressions.

Среда: macOS arm64, OpenJDK 25.0.2 (`/opt/homebrew/opt/openjdk@25`), Clojure 1.12.4. Команды из папки примера. Исходники checker не менялись.

## Unit

`clojure -M:test` — exit 0:

```text
Testing todo.unit-test
Ran 3 tests containing 22 assertions.
0 failures, 0 errors.
```

Это настоящий upstream classpath, но `pg/execute!` и repo функции переопределяются `with-redefs`; DB/listener не запускаются. В stderr присутствуют upstream предупреждения environ java-home и SLF4J no StaticLoggerBinder.

## Typespec

`clojure -M:check` — exit 0:

```text
src/todo/app.clj:10:16  TSP210-01  compatibility not checked: t/unknown to :context/context (unknown)  [warning]
src/todo/handler.clj:23:3  TSP210-01  with-result: generic call not checked (unsupported)  [warning]
src/todo/main.clj:19:25  TSP210-01  compatibility not checked: t/unknown to :context/context (unknown)  [warning]

0 error(s), 3 warning(s) in 4 file(s). Trust: 0 cast, 0 any, 0 ignore.
```

Первый warning — неизвестный ctx внутри lifecycle macro; второй — solver не поддержал этот реальный generic callback с коллекцией/ref; третий — неаннотированный lifecycle wrapper. Не добавлены cast/ignore для их сокрытия. `Trust: 0 any` — счётчик checker, НЕ утверждение, что extern/specs не содержат any?: произвольный response body, JDBC bind values и результат stop-system явно содержат any?.

Первоначальные авторские literal map types были отклонены parser (9 errors, затем 1 пропущенная literal query map). Исправлены на поддерживаемые `s/keys`, без изменения checker. Generic literal-map envelope не поддержан; заменён обычным response spec и отдельным generic callback helper. Подробности в README.

## Integration

Проверка использует отдельный локальный PostgreSQL 18.6, собственный initdb каталог агента и порт 55439, HTTP 18081. Hyper PostgreSQL не подключался. `clojure -M:integration` — exit 0:

```text
Ran 1 tests containing 7 assertions.
0 failures, 0 errors.
waiting for server to shut down.... done
server stopped
```

Проверено по реальному HTTP: 400 для неверного title, 400 для malformed JSON, 201 с точным сохранением SQL-injection-like строки и `done=false`, GET 200 возвращает созданный row. Логи показывают закрытие pg datasource и остановку http. `stop!` также явно останавливает HTTP executor. Кластер PostgreSQL остановлен shell trap; системные сервисы не менялись.

Обнаруженные при запуске особенности окружения/API: `initdb` из PATH был client-only libpq; выбран `/opt/homebrew/opt/postgresql@18/bin`. PostgreSQL требует `LC_ALL=C` в этой macOS среде. Одного `:deps` в module manifest недостаточно для запуска pg/http: в config явно перечислены `:services ["pg" "http" "todo.app"]`.

## Отрицательная статическая проверка

`clojure -M:check src negative` — ожидаемый exit 1:

```text
negative/bad_title.clj:4:25  TSP2345  42 is not assignable to string?
```

Итого 1 error, те же 3 warnings, 5 файлов. Fixture вне normal `:paths ["src"]`; он только для статического анализа и никогда не исполняется.
