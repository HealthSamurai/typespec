# Проверки

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
