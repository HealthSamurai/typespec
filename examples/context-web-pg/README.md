# context-clj: HTTP + PostgreSQL + Typespec

Небольшой реальный сервис: `GET /todos`, `POST /todos` с JSON `{"title":"learn context"}`. Используются upstream `context-clj/http`, `pg`, `system`, не Hyper DB. `src/todo/repo.clj` содержит параметризованные запросы и runtime-проверку строк; `handler.clj` проверяет ввод; `app.clj` объявляет зависимости и маршруты; `main.clj` управляет lifecycle. `resources/setup.sql` — один идемпотентный setup, не полноценная система миграций. Существующие данные не удаляются.

## Запуск

Нужны Clojure CLI, JDK, отдельный PostgreSQL. Все команды из этой папки:

```sh
export JAVA_HOME=/opt/homebrew/opt/openjdk@25/libexec/openjdk.jdk/Contents/Home
clojure -M:test                 # mocks: без HTTP listener и без БД
clojure -M:check                # Typespec из ../.., анализ src, externs из typespec.edn
clojure -M:check --strict       # warnings становятся errors; это НЕ ожидаемый green gate
```

Для своей **отдельной тестовой** БД задайте переменные (по умолчанию используются значения ниже):

```sh
export TODO_PG_HOST=127.0.0.1 TODO_PG_PORT=55439
export TODO_PG_DATABASE=context_example TODO_PG_USER=context_example
export TODO_PG_PASSWORD=local-demo-only TODO_HTTP_PORT=18081
clojure -M:run
# В другом терминале:
curl -s http://127.0.0.1:18081/todos
curl -s -H 'Content-Type: application/json' -d '{"title":"learn context"}' http://127.0.0.1:18081/todos
# Ctrl-C: stop-system + явный shutdown HTTP executor.
```

**Важно:** это публичные маршруты без авторизации. Upstream `http/start` читает `:binding`, но не передаёт его в http-kit: HTTP listener может слушать все интерфейсы. Не запускайте на публичном сервере, не используйте настоящие секреты/данные. PG ниже слушает только loopback. Upstream pg пишет SQL и параметры в лог.

## Опциональный integration на одноразовом PostgreSQL

Проверяет настоящий HTTP, JSON, INSERT/SELECT, неверный title, malformed JSON и сохранение SQL-injection-like строки как значения. Integration добавляет строку, поэтому не направляйте его на production. Должны быть свободны оба порта. `PG_BIN` должен указывать на server installation, не Homebrew libpq (у того есть initdb, но нет postgres).

```sh
export JAVA_HOME=/opt/homebrew/opt/openjdk@25/libexec/openjdk.jdk/Contents/Home
export PG_BIN=/opt/homebrew/opt/postgresql@18/bin
export PATH="$PG_BIN:$PATH" LC_ALL=C
export TODO_PG_HOST=127.0.0.1 TODO_PG_PORT=55439 TODO_HTTP_PORT=18081
export TODO_PG_DATABASE=context_example TODO_PG_USER=context_example TODO_PG_PASSWORD=local-demo-only
# Для агента: SCRATCH — его собственная .hyper/tmp/<agent-id>/.
# Для человека: любой новый личный временный каталог, без существующей БД.
export SCRATCH="${TMPDIR:-/tmp}/context-example-$$"
mkdir -p "$SCRATCH"
initdb -D "$SCRATCH/data" -A trust -U context_example --no-locale
pg_ctl -D "$SCRATCH/data" -l "$SCRATCH/postgres.log" \
  -o "-h 127.0.0.1 -p $TODO_PG_PORT -k $SCRATCH" -w start
trap 'pg_ctl -D "$SCRATCH/data" -m fast -w stop' EXIT
createdb -h 127.0.0.1 -p "$TODO_PG_PORT" -U context_example context_example
clojure -M:integration
# trap останавливает только этот кластер при выходе shell.
# В интерактивном shell остановите явно:
pg_ctl -D "$SCRATCH/data" -m fast -w stop
trap - EXIT
```

`LC_ALL=C` нужен на проверенной macOS: без него PostgreSQL 18.6 завершался с `postmaster became multithreaded during startup`. Auth trust здесь только для одноразовой loopback БД. Пароль dummy обязателен из-за manifest pg. Integration HTTP connect timeout 5 секунд, request timeout 10 секунд; pool pg connection timeout upstream 20 секунд.

## Типы и границы доверия

* Context — **map с `:system` atom**, не сам atom. Extern объявляет обязательный ключ через `s/keys`; остальные поля context открыты.
* `pg/execute!` принимает SQL vector; тип допускает различные JDBC параметры, поэтому элементы `any?`. Выход — коллекция map, **не автоматически Todo**. `checked-todo` проверяет id/title/done через `s/valid?`, иначе бросает исключение. SQL и его схема статически не доказаны.
* `http/register-endpoint` — macro. Runtime `s/conform` проверяет map endpoint/`ifn?`, но не сигнатуру callback. `system/defstart`, `defstop`, `defmanifest` тоже macros. Они не объявлены притворными обычными extern functions. Для defstart/defstop в typespec.edn явно заданы доверенные binding-body контракты: checker проверяет тело, но не разворачивает макрос и не доказывает lifecycle библиотеки. defmanifest и register-endpoint остаются отдельными границами.
* `response` принимает явный union: Todo, vector Todo или error map. `with-result` проверяется solver с поддержкой именованных refs и `s/keys`; cast/ignore не добавлены. SQL параметры и доверенные extern-границы по-прежнему не доказывают схему базы.
* Literal map types вроде `{:system Atom}` не являются языком типов этого прототипа. Первоначальный запуск это выявил; корректные контракты записаны через `s/keys`. Generic envelope с literal map return также не поддержан, поэтому используется обычный spec response.
* `pos-int?`/`s/valid?` и SQL CHECK обеспечивают runtime ограничения; отсутствие static ошибки не заменяет их.
* Handler имеет 400 для неправильного/пустого/длинного title и некорректного JSON. DB ошибки не превращаются в ложный 200. Нет pagination, auth, полноценного migrations runner, transactions между несколькими запросами, production error middleware.
* `main/stop!` отдельно завершает worker executor: upstream `http/stop` останавливает listener, но не вызывает executor shutdown. При частичном неудачном старте upstream cleanup имеет собственные ограничения; пример не патчит библиотеку.

## Первоисточники и pins

SHA получены через `curl https://api.github.com/repos/context-clj/<repo>/commits/HEAD`, затем прочитаны исходники этих SHA. В deps явно закреплён единый `context/system`, перекрывая несовпадающие transitive system revisions http/pg. Context overview и sample — источники дизайна, не дополнительные runtime deps.

* [context overview, a9d6a91aaa8095ea247e189983f7ef7fd0885047](https://github.com/context-clj/context/blob/a9d6a91aaa8095ea247e189983f7ef7fd0885047/README.md)
* [http source, e158b6b244c2b8e053de2a097d1071d9419adef9](https://github.com/context-clj/http/blob/e158b6b244c2b8e053de2a097d1071d9419adef9/src/http.clj): register-endpoint, dispatch, parse-body, format-response, lifecycle.
* [pg source, aa8ea0eb565e7f70f80f3248e6a83198628e1047](https://github.com/context-clj/pg/blob/aa8ea0eb565e7f70f80f3248e6a83198628e1047/src/pg.clj): execute!, coerce, pool, manifest. Не используется `q`: он удаляет nil параметры.
* [system source, 818ff24fed230cc75119f5ca66088b32d99af581](https://github.com/context-clj/system/blob/818ff24fed230cc75119f5ca66088b32d99af581/src/system.clj): context shape, state, lifecycle.
* [sample, daab6ca45f2e49a7fbc08731b04d4873aac1835d](https://github.com/context-clj/sample/blob/daab6ca45f2e49a7fbc08731b04d4873aac1835d/src/mysystem.clj): реальные patterns, а не единственный источник API.

Текущая статическая проверка: 0 errors / 0 warnings. Отрицательные fixtures проверяют неправильный title как в обычном вызове, так и внутри defstart. Результаты запусков и история ограничений: см. `verification.md`.

Отрицательная проверка: `clojure -M:check src negative` должна завершиться exit 1 с `TSP2345` для числового title. Fixture не входит в обычный `:paths` и не должен исполняться.
