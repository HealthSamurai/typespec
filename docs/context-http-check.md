# Проверка context-clj/http

## Результат

Проверен неизменённый pin e158b6b244c2b8e053de2a097d1071d9419adef9: все8 production файлов,1757строк; SHA каждого в examples/context-http-check/manifest.edn. JVM27.0.1.1, Clojure1.12.4 (upstream просит1.12.0; выбрана версия каталога checker). Исходники, тесты upstream и окружение сервера не исполнялись.

Baseline: **7 ошибок /44 предупреждения**. После одного общего исправления checker: **0 ошибок /44 предупреждения**. Это не доказательство отсутствия ошибок библиотеки: неполнота и silent skips остаются. Новых externs, macro models, any/cast/ignore не добавляли, trust0/0/0.

## Подтверждённый дефект checker

call-fn при unknown первого аргумента не находил полностью доказанной перегрузки и выбирал последнюю, даже если остальные аргументы ей противоречат. Для string/replace это давало ложные требования Pattern/callback вместо допустимых string replacement.

Семь ложных диагностик на шести местах: http103:47,141:52; mcp74:76 и74:80; uui137:36,148:36; heroicons25:65. Runtime probes стандартной clojure.string подтвердили допустимость операций без загрузки HTTP. Теперь кандидаты, у которых только непроверенные границы TSP210-01, предпочтительнее несовместимых. Unknown предупреждения остаются. При неоднозначных возвращаемых типах результат unknown, а не произвольная перегрузка. Неверные известные аргументы по-прежнему дают ошибку.

У шести предупреждений routing46,60,61,67,74,77 изменилось ожидаемое number? на int?: выбран первый incomplete-кандидат вместо последнего fallback. Это изменение текста диагностик, НЕ доказательство integer-типа.

## Проверки родителя

- `clojure -M:test`: **129 тестов /833 assertions**,0failures/errors, все прежние goldens.
- `bash examples/jar-declarations/verify.sh`: bundled/split runtime и static проходят; negative TSP2345, namespace библиотеки не исполняется checker.
- context-web-pg: checker0/0, unit3/22.
- `JAVA_HOME="$(/usr/libexec/java_home)" sh examples/context-pg-check/acceptance.sh`: прежние2errors/8warnings, SHA и точные позиции, trust0/0/0. Обе реальные находки сохранены.
- Plain HTTP baseline команда `sh examples/context-http-check/run.sh` независимо выполнена до исправления (exit1 ожидаем из-за7ошибок).

## Классификация и blockers

Все44 предупреждения с позициями и исходниками: [context-http-coverage.md](context-http-coverage.md).35 — недостаток типов TSP210-01;6 — неизвестная семантика register-endpoint TSP7031;2 — opaque spec TSP9002;1 — reflective Java call TSP7023. Это непроверенные участки, не подтверждённые ошибки upstream.

- future и некоторые cond-> могут пропускать ошибки в теле: подтверждено checker-only repro; не исправлено в этой итерации.
- defmethod тела проверяются, но с неизвестными/выведенными из pattern параметрами.
- lifecycle defstart/defstop без dependency source обрабатываются как неизвестные вызовы с обходом аргументов, НЕ как корректно раскрытые binding-макросы.
- Разбор8файлов и trust0 не означают проверку всех путей исполнения. Реальных ошибок HTTP этими результатами не доказано.

Исходный baseline отдельно сохранён в baseline-initial; current и reviewed expected не должны перезаписывать исторический снимок. Родитель независимо выполнил `sh examples/context-http-check/run.sh --accept`: exit0. Автотест сверяет все8SHA,8parsed/0failed,1757строк/521форму, точные44 диагностических tuple, отсутствие загрузки namespace и trust0. Независимое review запрошено у Claude Fable5.2, но к моменту сохранения этого частичного результата заключение не получено. Это незавершённый критерий приёмки; отсутствие подтверждённых high не означает их отсутствие. Результат сохраняется локально без push; после review нужны отдельное решение по находкам и при необходимости повторные прогоны.
