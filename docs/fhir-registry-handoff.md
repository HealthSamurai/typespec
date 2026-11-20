# get-ig: заметки для Александра (@octoshikari)

Живой документ передачи работы. Checker не меняем: его дорабатывает Александр. Работа здесь — воспроизводимый пример, внешние контракты, проверки и наблюдения. Ничего из этого документа автоматически не отправляется в Telegram.

## Проект и baseline

- Репозиторий: https://github.com/fhir-clj/fhir-package-registry
- Pin: ea79b6a18d9ae759593c3824f502bc1200209e22.
- Команда из Typespec: `sh examples/fhir-registry-check/run.sh` (exit1 ожидаем при диагностических ошибках).
- Все11 файлов src,2405строк,204формы: **2errors/121warnings**, парсер без сбоев, trust0/0/0, upstream namespaces не загружены.
- Полный classpath; точные submodule gitlinks, отдельно разрешён конфликт context/system в пользу root pinned local dependency. Upstream не изменён.
- Java27.0.1.1, Clojure1.12.4 для каталога Typespec; upstream просит1.12.0 — это не идентичное runtime окружение.
- baseline-initial сохранён отдельно с SHA256SUMS; current — повторные прогоны. Подробности в README примера.

## Два диагностических дефекта checker

### 1. Java generic self-type

`src/fhir/registry/gcs.clj:49:76`: TSP2339 ServiceOptions$Builder has no method build.

Минимальный checker-only repro:
```clojure
(ns probe (:import [com.google.cloud.storage StorageOptions]))
(defn f [credentials]
  (-> (StorageOptions/newBuilder) (.setCredentials credentials) .build))
```

StorageOptions$Builder наследует ServiceOptions$Builder<Storage,StorageOptions,StorageOptions$Builder>. setCredentials возвращает generic B; erased return — ServiceOptions$Builder. Checker теряет подстановку B и не видит public build на конкретном builder. Прямой `.build` без setCredentials проходит. Подтверждено javap/reflection на pinned google-cloud-storage2.32.1/core2.31.0, без вызовов облака. Детали: fhir-registry-java-triage.md. Предпочтительное направление — подстановка параметров generic superclass, не hardcoded правило по имени setCredentials.

### 2. Nested def: Var существует, но unbound

`src/fhir/registry/diff.clj:148:14`: TSP2305 fhir.registry has no var context.

context определён через def внутри -main (:846) и start-dev (:855). Безопасный синтетический REPL эксперимент подтвердил: компиляция `(defn init [] (def context :ready))` интернирует Var, но `(bound? v)` до init false. Checker двухфайлового примера сообщает «нет var». Нельзя исправлять присвоением доказанного типа :ready: инициализация зависит от вызова. Нужны раздельные понятия объявления и связанного значения, либо явная неполнота. Подробности/repro: fhir-registry-context-triage.md.

## Предупреждения и покрытие

Полный inventory121 с позициями и источниками: fhir-registry-coverage.md.72unknown boundaries,43unknown Java receivers,6known reflective calls. Не заявляем их багами приложения. Ноль opaque AST nodes не означает проверку всех тел; macro skips разобраны в coverage report. Позднее подтверждён логический баг semver: сравнение alpha.1/alpha.2 и alpha/alpha.1 возвращает0. Подтверждено REPL, не Typespec; см. fhir-registry-semver-bug.md.

## Дорасстановка типов — первый проход завершён

Отдельный externs.edn:8функций semver/IO; baseline остаётся. Nullable входы/выходы проверены по исходнику и безопасным probes. Результат2/121→2/134:7 Java receivers стали известными, дополнительные аргументы и результаты теперь проверяются. Тесты overlay6/61, общий suite166/1037 PASS. Два дефекта выше не скрываем, context не initialized. NDJSON polymorphism не моделировали. Команды, delta и ограничения: fhir-registry-contracts.md; итог: fhir-registry-result.md.

## Безопасность экспериментов

Поднят приватный Unix-socket nREPL через clojure-repl plugin/hyperd, без TCP. Сервис get-ig, DB, Docker, GCS не запускаются; настоящие -main/start-dev не вызываются. Можно воспроизводить синтетические формы, parser/checker и Java metadata. Учётные данные не читаются.
