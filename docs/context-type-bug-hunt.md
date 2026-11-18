# Реальная ошибка типов в закреплённом context/pg

## Результат

Подтверждена ошибка **DataSource вместо Connection** в `pg/with-connection`. Это исходный код библиотеки, не `bad_title`/`bad_lifecycle` и не внесённая нами поломка. Вызов реального `pg/array-of` с корректными аргументами падает с `ClassCastException`, ещё до обращения к БД. Typespec обнаруживает противоречие между типом результата `datasource` и локальной подсказкой `^Connection`, если предоставить один честный extern-контракт.

Production и example не изменялись. Единственный новый файл в проекте — этот отчёт. Scratch: `/Users/niquola/hyper-code2/.hyper/tmp/cemw/` (путь выдан `workspace.tmp`). БД, HTTP listener, credentials и сетевые JDBC-соединения не использовались.

## Проверенные версии и границы поиска

Версии из `examples/context-web-pg/deps.edn`:

- context/pg: `aa8ea0eb565e7f70f80f3248e6a83198628e1047`;
- context/http: `e158b6b244c2b8e053de2a097d1071d9419adef9`;
- context/system: `818ff24fed230cc75119f5ca66088b32d99af581`.

Проверены несколько наиболее вероятных мест: HTTP parsing/response/auth context, example handler/repo с проверкой DB rows, PG datasource/connection и вызовы array-of. Полный аудит библиотек не проводился. Отдельной доказанной ошибки HTTP не заявляем.

GitHub API для PG pin вернул дату `2025-10-31T18:44:17Z`, сообщение `Bump clj-commons/clj-yaml to 1.0.29`. SHA-256 локального gitlibs `src/pg.clj` совпал с raw GitHub на этом pin: `dca4826d414a66c22dc10ebb2a01e1d8dccaf5cf64cdfe9b8bb952987d023db5`.

## Источник и типовое противоречие

[pg.clj:48–58](https://github.com/context-clj/pg/blob/aa8ea0eb565e7f70f80f3248e6a83198628e1047/src/pg.clj#L48-L58):

```clojure
(defn datasource [ctx]
  (or (system/get-system-state ctx [:datasource])
      (throw (Exception. "No datasource in pg module. pg module probably not initialized"))))

(defn connection [ctx]
  (let [^HikariDataSource datasource (datasource ctx)]
    (.getConnection datasource)))

(defn with-connection [ctx f]
  (with-open [^Connection conn (datasource ctx)]
    (f conn)))
```

Ожидается JDBC `java.sql.Connection`. Фактически передаётся `com.zaxxer.hikari.HikariDataSource`, который не реализует `Connection`. Это не просто подозрение по названию:

- `get-pool`, строки 167–181, создаёт `(HikariDataSource. config)`;
- `system/defstart`, строки 421–428, сохраняет его под `:datasource`;
- `connection` непосредственно выше правильно вызывает `.getConnection`;
- реальный потребитель [array-of, строки 136–137](https://github.com/context-clj/pg/blob/aa8ea0eb565e7f70f80f3248e6a83198628e1047/src/pg.clj#L136-L137) требует `Connection`:

```clojure
(defn array-of [ctx type array]
  (with-connection ctx
    (fn [c] (.createArrayOf ^Connection c type (into-array String array)))))
```

Таким образом `pg/array-of` ломается и на обычном штатном pool, не только на нашем тестовом объекте. Предполагаемое исправление — получать connection, а не datasource в `with-connection`; оно здесь не вносилось и не проверялось с БД.

## Безопасное runtime-воспроизведение

Запуск из `examples/context-web-pg`, с зависимостями именно этого example:

```sh
JAVA_HOME=/opt/homebrew/opt/openjdk@25 clojure -M /Users/niquola/hyper-code2/.hyper/tmp/cemw/hunt-runtime.clj
```

Содержимое scratch-скрипта:

```clojure
(require '[pg :as pg] '[system :as system])
(import '[com.zaxxer.hikari HikariDataSource] '[java.sql Connection])
(let [pool (HikariDataSource.)
      ctx {:system (atom {:pg {:datasource pool}}) :cache (atom {})}]
  (println :datasource-class (class (pg/datasource ctx)))
  (println :connection? (instance? Connection (pg/datasource ctx)))
  (try
    (println :result
      (pg/with-connection ctx (fn [c] (.getAutoCommit ^Connection c))))
    (catch Throwable e (println :error (class e) (ex-message e))))
  (println :pool-closed-after-helper (.isClosed pool))
  (.close pool))
(shutdown-agents)
```

Результат:

```text
:datasource-class com.zaxxer.hikari.HikariDataSource
:connection? false
:error java.lang.ClassCastException class com.zaxxer.hikari.HikariDataSource cannot be cast to class java.sql.Connection (...)
:pool-closed-after-helper false
```

Дополнительно выполнен **реальный upstream caller**, без собственного callback:

```clojure
(require 'pg)
(let [pool (com.zaxxer.hikari.HikariDataSource.)
      ctx {:system (atom {:pg {:datasource pool}}) :cache (atom {})}]
  (try (pg/array-of ctx "text" ["hello"])
       (catch Throwable e (println (class e) (ex-message e)))
       (finally (.close pool))))
```

Он даёт тот же `java.lang.ClassCastException: HikariDataSource cannot be cast to java.sql.Connection`.

Здесь нет `with-redefs`, fake datasource или extracted runtime implementation: выполняются реальные функции PG из gitlibs. Отличие от production — pool создан no-arg конструктором и не настроен, context собран вручную в штатной форме. Это позволяет не открывать сокеты; mismatch зависит от класса объекта, а не от конфигурации БД. Ошибка воспроизводится до запроса connection. Наблюдение `pool-closed-after-helper false` приведено как факт, не как дополнительный finding.

## Typespec: минимальная проверка того же тела

Scratch root: `/Users/niquola/hyper-code2/.hyper/tmp/cemw/check`.

`typespec.edn`:

```clojure
{:paths ["src"] :externs ["externs.edn"]}
```

`externs.edn`:

```clojure
{pg/datasource [map? :-> com.zaxxer.hikari.HikariDataSource]}
```

Контракт основан на реальном `get-pool` и `defstart`; не утверждает, что `datasource` возвращает Connection, и не описывает ошибку заранее. `map?` здесь лишь грубое описание context: полноту его структуры этот тест не доказывает. Return type описывает нормальный результат штатно запущенного PG; исключения и произвольное ручное повреждение state не моделируются.

`src/hunt.clj`:

```clojure
(ns hunt (:require [pg]) (:import [java.sql Connection]))
;; Verbatim pg.clj:56-58, only datasource qualified for extracted namespace.
(defn with-connection [ctx f]
  (with-open [^Connection conn (pg/datasource ctx)]
    (f conn)))
```

Это **extracted static check**, не проверка всего upstream namespace: тело скопировано без изменения, только `datasource` квалифицирован как `pg/datasource`. Runtime выше, напротив, использует исходные функции напрямую.

Запуск из example, чтобы JVM видела Hikari и Typespec:

```sh
JAVA_HOME=/opt/homebrew/opt/openjdk@25 clojure -M:check \
  --root /Users/niquola/hyper-code2/.hyper/tmp/cemw/check
```

Фактический вывод, exit 1:

```text
src/hunt.clj:4:15  TSP2322  hint Connection contradicts the type: com.zaxxer.hikari.HikariDataSource
src/hunt.clj:4:47  TSP210-01  compatibility not checked: t/unknown to map? (unknown)  [warning]

1 error(s), 1 warning(s) in 1 file(s). Trust: 0 cast, 0 any, 0 ignore.
```

С `--strict --strict-nil` — то же TSP2322 плюс TSP7010 (нет полной сигнатуры функции), а unknown-контекст становится error: всего 3 errors, 0 warnings. Это не три runtime bug: два сообщения относятся к неполноте аннотаций. Обходов через cast/ignore/any не использовалось.

## Example и ограничения

Обычный `clojure -M:check` исходного example: `0 error(s), 0 warning(s) in 4 file(s). Trust: 0 cast, 0 any, 0 ignore.` Это **не** доказательство исправности зависимостей: example не вызывает `pg/array-of` и не проверяет тело upstream `with-connection`.

`--strict --strict-nil` для example дал 4 ошибки: нет сигнатур `config` и `-main`, nullable `(Runtime/getRuntime)`, nullable `first` результата `pg/execute!` в `repo/create-todo`. Последнее — честный недостаток гарантии в extern `(coll-of map?)`, допускающем пустой результат. Это не подтверждённая runtime ошибка на показанном `INSERT ... RETURNING`: БД не запускалась, реальные отсутствующие rows не воспроизведены. Runtime/getRuntime тоже не объявляем багом: это ограничение модели Java nullability.

`negative/bad_title.clj` и `negative/bad_lifecycle.clj` не использовались как находки. Единственный подтверждённый finding этого отчёта — исходный `pg/with-connection`, с прямым воспроизведением через `pg/array-of`. Typespec ловит его при явном контракте datasource, но это не автоматическое end-to-end выведение типов всего context/pg.
