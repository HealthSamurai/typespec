# Приёмка примера context-clj web + pg

Проверяемый пример: `examples/context-web-pg` (в работе).

## Первоисточники

- HTTP: https://github.com/context-clj/http/tree/e158b6b244c2b8e053de2a097d1071d9419adef9
- PostgreSQL: https://github.com/context-clj/pg/tree/aa8ea0eb565e7f70f80f3248e6a83198628e1047
- Образец lifecycle: https://github.com/context-clj/sample/tree/daab6ca45f2e49a7fbc08731b04d4873aac1835d

`http/register-endpoint` — macro со spec conform endpoint, не доказательство типов handler. Handler получает context и request. `pg/execute!` принимает `{:sql [sql & params]}`, использует JDBC и возвращает coerce-строки. Не использовать `pg/q`: его format-sql удаляет nil из params. Lifecycle задаётся system/defmanifest, defstart, defstop; запуск/остановка — start-system/stop-system.

## Что проверить перед объявлением готовности

1. Реальные закреплённые зависимости загружаются, не только mock namespaces.
2. Unit: корректное создание/список, некорректный ввод, SQL placeholders и отдельные параметры.
3. Integration: HTTP запрос проходит через настоящий context http и pg в отдельную временную базу. Строка, похожая на SQL injection, хранится как значение; таблица остаётся целой.
4. Ошибка или успех теста останавливают сервисы и временный PG. Не использовать БД Hyper или существующие пользовательские таблицы.
5. Typespec запускается по app source с extern-файлом; реальный вывод сохраняется/описывается. Нельзя выдавать доверенный generic contract БД за проверку SQL-схемы.
6. Отрицательный типовой пример действительно выдаёт ожидаемый код ошибки.
7. README различает unit, integration и статическую проверку, описывает установку/порты и ограничения.

## Обнаруженные риски upstream

HTTP и PG закрепляют разные system SHA; проекту нужна явная согласованная версия. Jackson core/databind также различаются — совместимость проверяется runtime, не предполагается по deps.

По проверке автора примера, HTTP defstart не передаёт :binding в http-kit run-server. Нельзя обещать loopback-only сервер только по настройке :binding; до исправления upstream пример не предназначен для незащищённой публичной машины. HTTP defstop и worker-pool cleanup нужно проверить при завершении integration.

## Первое ревью кода примера

Context runtime — карта с `:system` и `:cache` atom, не самостоятельный atom. HTTP JSON разбирается через `http/parse-body`; integration должен проверять malformed JSON, scalar/array/null и неверный title. HttpClient должен иметь ограниченные connect/request timeouts.

Первый черновик использовал literal map types, которых Typespec пока не читает: нужны именованные spec `s/keys`. Generic response envelope с параметром внутри map выходит за поддержанный kernel (primitive/coll/fixed fn). Это найденные ограничения, а не повод объявить SQL/HTTP полностью проверенными или заменить всё на any. Проверка runtime-строк через spec и параметризованные SQL сохраняются.

## Независимые результаты родительского агента

В `examples/context-web-pg`: `clojure -M:test` — 3 теста / 22 assertions, без ошибок. `clojure -M:check` — exit 0, 0 errors / 3 warnings: неизвестный context в lifecycle (`app.clj`, `main.clj`), unsupported generic call с картой результата (`handler.clj`). Предупреждения не подавлены. Summary trust 0 any — это счётчик инструмента, НЕ отсутствие `any?` в контрактах: response body явно any?. Контракты extern доверенные; SQL schema не доказана.

Независимый повтор HTTP → PostgreSQL 18.6 завершён успешно: 1 тест / 7 assertions, отдельный кластер на PG 55449 и HTTP 18091. Повтор unit — 3/22; обычный checker — 0 errors/3 warnings, отрицательный fixture — ожидаемый exit 1 с TSP2345. HTTP и datasource остановлены, PostgreSQL остановлен trap (server stopped). Общий suite Typespec — 60/309 плюс golden/CLI PASS. Пример и команды: `examples/context-web-pg/README.md`, подробные результаты — `verification.md` рядом.
