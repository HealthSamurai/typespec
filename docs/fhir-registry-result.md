# get-ig: итог первого прохода с разметкой

Проект fhir-clj/fhir-package-registry, pin ea79b6a18d9ae759593c3824f502bc1200209e22. Проверены все11 src-файлов,2405строк,204формы; зависимости по закреплённым gitlinks. Baseline2errors/121warnings; отдельный overlay8функций даёт2errors/134warnings. Неполноту не скрывали:7 неизвестных Java receiver стали известными, появились дополнительные проверки входов и возвращаемых значений. Подробный delta — fhir-registry-contracts.md.

Checker не изменялся по просьбе пользователя; upstream тоже. Две диагностики checker исследованы и передаются Александру: generic self-type Java builder и nested def, создающий unbound Var. Это не подтверждённые ошибки приложения.

## Перепроверка после F530

На тех же 11 файлах и полном закреплённом classpath статический прогон
даёт **1 ошибку и 121 предупреждение** без контрактов и **1 ошибку и
134 предупреждения** с semver/IO-контрактами.

- В `gcs.clj:49:76` ушёл ложный `TSP2339` на `.build`. На
  `.getService` в `49:7` вместо неизвестного получателя (`TSP7020`)
  теперь указан известный `StorageOptions` и предупреждение о рефлексии
  `TSP7023`.
- В `diff.clj:148:14` остался `TSP2305` для `fhir.registry/context`.
  Остальные диагностики совпали по коду, позиции и сообщению.

Исторические снимки сохранены. Прогон статический, с нулевыми escape
счётчиками и без загрузки upstream namespace.

После `prepare.sh` из `examples/fhir-registry-check` можно запустить
`clojure -Srepro -M java-return-probe.clj` на том же classpath. Проба
проверяет типы builder и результата `.build`, а также ошибку для
действительно отсутствующего метода.

[Проверка Java-результатов](../features/F530-java-generics-nullability.md)

## Перепроверка после F721

На том же pin проверены все 11 файлов и 2405 строк с полным
закреплённым classpath. Без контрактов получено 0 ошибок и 123
предупреждения, с semver/IO-контрактами — 0 ошибок и 136 предупреждений.

- В `src/fhir/registry/diff.clj:148:14` вместо `TSP2305` появился
  `TSP721-01`. Чекер видит объявленный Var `fhir.registry/context`
  и предупреждает о чтении его корня на верхнем уровне до доказанной
  инициализации.
- Второй `TSP721-01` указывает на чтение того же Var в `stop-dev`,
  `src/fhir/registry.clj:858:23`. Чекер не доказывает порядок
  вызовов `start-dev` и `stop-dev`.

Остальные диагностики совпали с прогоном после F530 по коду,
позиции и сообщению. Оба прогона завершились без загрузки upstream
namespace и с нулевыми счётчиками `cast`, `any` и `ignore`.
Для повтора запустить `sh examples/fhir-registry-check/run.sh`
и `sh examples/fhir-registry-check/run.sh contracts` после `prepare.sh`.
[Карточка F721](../features/F721-var-initialization.md)

## Перепроверка после F611 и F312

На всех 11 файлах того же pin получено 0 ошибок и 124 предупреждения
без контрактов, 0 ошибок и 137 предупреждений с semver/IO-контрактами.
В обоих прогонах добавился один `TSP210-01` в
`src/fhir/registry.clj:494:68`. Чекер дошёл до `str/replace` внутри
шага `cond->`. Тип `q` из query-параметров остаётся неизвестным.
Остальные диагностики совпали с прогоном после F721.
[Условные шаги](../features/F611-conditional-threading.md)

## Баг сравнения версий

Реальная логическая ошибка compare-pre-release: alpha.1 и alpha.2, а также alpha и alpha.1 сравниваются как равные. В loop отсутствует переход к следующему identifier после равенства. Подтверждено на настоящем pure semver namespace через clojure-repl plugin, без запуска приложения. Это находка чтения/REPL, не статического Typespec. Repro и ограничения: fhir-registry-semver-bug.md. Исправлять upstream и отправлять PR в этой работе не стали.

## Независимые прогоны родителя

- `sh examples/fhir-registry-check/run.sh`: exit1 ожидаемо,2errors/121warnings.
- `sh examples/fhir-registry-check/run.sh contracts`: exit1 ожидаемо,2errors/134warnings;11parsed/0failed,trust0/0/0,upstream-loaded[]. Контракты сохраняются в current-contracts, baseline не перезаписывается.
- Отдельный namespace typespec.fhir-registry-contracts-test:6tests/61assertions PASS. Требует подготовленного pinned checkout и проверяет SHA перед исполнением reviewed pure defns; в общий runner не подключён, чтобы не скрывать prerequisite.
- `clojure -M:test`:166tests/1037assertions PASS, все goldens. Этот прогон включает свежие изменения Александра в main9a99729; наши изменения src/resources отсутствуют.

## Как повторить контрактные тесты

Из корня Typespec после `sh examples/fhir-registry-check/prepare.sh`:
```sh
clojure -Sdeps '{:paths ["src" "resources" "test"]}' -M -e '(require (quote typespec.fhir-registry-contracts-test)) (let [r (clojure.test/run-tests (quote typespec.fhir-registry-contracts-test))] (shutdown-agents) (System/exit (+ (:fail r) (:error r))))'
```

## Границы

Все121 исходное предупреждение классифицированы в fhir-registry-coverage.md. Разбор файлов не гарантирует проверку macro bodies; silent skips остаются. IO overlay — выбранные поддержанные arities/types, не исчерпывающая модель clojure.java.io; неподдержанный корректный вызов не считается доказанным багом библиотеки. Return unknown в semver оставлен как неполнота. Инициализация context не объявляется доказанной. Java27/Clojure1.12.4 отличается от upstream Clojure1.12.0, это зафиксировано.

Приватный Unix-socket nREPL остаётся под hyperd для дальнейших экспериментов. Приложение get-ig, серверы DB/GCS/Docker не запускались, credentials не читались. Сводные заметки для передачи: fhir-registry-handoff.md. Отдельного независимого review overlay не проводилось. Сохранение — локальное, без push и без сообщений Александру.
