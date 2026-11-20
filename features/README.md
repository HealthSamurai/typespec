# Feature cards

Каталог Typespec: существующий Clojure, вывод типов, Java, данные, доверие и инструменты.

Основной маршрут: F110 → F120 → F510 → F310 → F220 → F230 → F210. Карточки сгруппированы по смыслу, а не по источнику идеи.

| Группа | Тема |
|---|---|
| F1xx | Источники типов |
| F2xx | Система типов и вывод |
| F3xx | Данные и коллекции |
| F4xx | Функции и диспетчеризация |
| F5xx | Java |
| F6xx | Макросы |
| F7xx | Модули и библиотеки |
| F8xx | Диагностика и доверие |
| F9xx | CLI, редактор и агенты |

Номера сгруппированы по сотням; десятки обозначают темы, единицы — детализацию. Есть резерв для новых карточек. Будущий код диагностики: `TSP<номер карточки>-<номер правила>`, например `TSP210-01`. После выпуска кодов ID постоянны: удалённые номера не переиспользуем. Номер группы не означает порядок реализации. Текущие TSP-коды пока не мигрированы.

`status` — зрелость дизайна; `implementation` — реализация. Примеры без меток example/output иллюстративны, тестовый раннер их не исполняет.
To discuss a feature, open an issue named after its card, for example `F240: narrowing and exhaustiveness`.

| Status | Cards | Meaning |
|---|---|---|
| `implemented` | 17 | works in the prototype; the example in the card is checked by `clojure -M:test` |
| `partial` | 0 | design partly settled; see implementation for code status |
| `designed` | 36 | accepted design; implementation status is separate |
| `proposed` | 8 | an idea under discussion, not yet accepted into the design |

This file is generated from the card headers by `clojure -M:test --update`.

| Card | Feature | Design | Implementation | Area | Origin |
|---|---|---|---|---|---|
| [F110](F110-existing-clojure.md) | Источники типов и их согласование | `designed` | `partial` | sources | Typespec |
| [F111](F111-type-hints.md) | Type hints как контракты | `designed` | `partial` | sources | Clojure |
| [F112](F112-spec.md) | Spec: s/def и s/fdef | `designed` | `partial` | sources | Clojure |
| [F113](F113-destructuring.md) | Деструктуризация | `designed` | `partial` | sources | Clojure |
| [F114](F114-preconditions.md) | Предусловия :pre | `designed` | `partial` | sources | Clojure |
| [F115](F115-explicit-signatures.md) | Явные сигнатуры t/sig | `designed` | `partial` | sources | Clojure |
| [F120](F120-key-is-type.md) | Ключ задаёт тип значения | `designed` | `partial` | core | Typespec |
| [F130](F130-namespace-sidecars.md) | Файлы типов рядом с исходником | `designed` | `partial` | declarations | Flow |
| [F150](F150-schema-providers.md) | Генераторы типов из схем | `designed` | `not-implemented` | sources | Clojure |
| [F210](F210-unknown-any-never.md) | Unknown, any и never | `designed` | `partial` | core | Typespec |
| [F220](F220-local-inference.md) | Локальный и контекстный вывод типов | `implemented` | `implemented` | core | Typespec |
| [F230](F230-generics-and-hof.md) | Generics и функции высшего порядка | `designed` | `partial` | core | Typespec |
| [F231](F231-generic-obligations.md) | Ограничения generics и проверка обязательств | `proposed` | `not-implemented` | types | Rust |
| [F232](F232-associated-types.md) | Связанные типы контрактов | `proposed` | `not-implemented` | types | Rust |
| [F233](F233-function-composition.md) | Композиция функций и частичное применение | `implemented` | `implemented` | core | Clojure |
| [F234](F234-transducers-into.md) | Трансдьюсеры map и filter в into | `implemented` | `implemented` | core | Clojure |
| [F235](F235-transducer-composition.md) | Композиция трансдьюсеров в into | `implemented` | `implemented` | core | Clojure |
| [F240](F240-narrowing-and-exhaustiveness.md) | Сужение, tagged unions и полнота веток | `implemented` | `implemented` | core | Typespec |
| [F250](F250-checked-guards.md) | Проверяемые guard-функции | `implemented` | `implemented` | narrowing | Flow |
| [F251](F251-refinement-evidence.md) | Область действия и инвалидация фактов о типах | `proposed` | `not-implemented` | types | Research |
| [F260](F260-opaque-refinements.md) | Уточнения spec как номинальные свидетельства | `designed` | `not-implemented` | types | Flow |
| [F270](F270-type-operators.md) | Операторы над типами мап | `implemented` | `implemented` | core | Typespec |
| [F271](F271-mapped-types.md) | Mapped types: преобразование полей типа | `proposed` | `not-implemented` | types | TypeScript |
| [F272](F272-conditional-types.md) | Условные типы и извлечение компонентов | `proposed` | `not-implemented` | types | TypeScript |
| [F273](F273-template-literal-types.md) | Типы строковых шаблонов | `proposed` | `not-implemented` | types | TypeScript |
| [F280](F280-literal-preservation.md) | Литералы и проверка без потери типа | `implemented` | `implemented` | core | Typespec |
| [F290](F290-brands.md) | Доменные brands | `designed` | `not-implemented` | types | Clojure |
| [F295](F295-recursive-types.md) | Рекурсивные типы | `designed` | `partial` | types | Clojure |
| [F310](F310-maps-and-destructuring.md) | Мапы, пути и деструктуризация | `designed` | `partial` | core | Typespec |
| [F311](F311-row-polymorphism.md) | Row polymorphism — сохранение остатка мапы | `proposed` | `not-implemented` | data | TypeScript and research |
| [F312](F312-literal-map-paths.md) | Литеральные пути мап | `implemented` | `implemented` | data | Clojure |
| [F313](F313-named-map-transformations.md) | Сохранение формы мапы через именованные функции | `implemented` | `implemented` | data | Clojure |
| [F320](F320-nil-and-platform.md) | Nil, коллекции и платформенные типы | `designed` | `partial` | core | Typespec |
| [F330](F330-tuples-and-nonempty.md) | Tuples, длина и непустые коллекции | `designed` | `partial` | data | Clojure |
| [F331](F331-fixed-vector-positions.md) | Длина и позиции в s/tuple | `implemented` | `implemented` | data | Clojure |
| [F340](F340-mutation-and-variance.md) | Изменяемые значения и variance | `designed` | `not-implemented` | core | Typespec |
| [F350](F350-records.md) | Records и номинальные классы | `designed` | `not-implemented` | data | Clojure |
| [F410](F410-arities-and-apply.md) | Арности, перегрузки и apply | `designed` | `partial` | functions | Clojure |
| [F411](F411-apply-fixed-tail.md) | Проверка apply с известными позициями хвоста | `implemented` | `implemented` | functions | Clojure |
| [F420](F420-control-flow.md) | Throw, assert и try | `designed` | `partial` | functions | Clojure |
| [F430](F430-protocols.md) | Протоколы и reify | `designed` | `not-implemented` | functions | Clojure |
| [F431](F431-protocol-method-specs.md) | Проверка reify и extend-type по s/fdef | `implemented` | `implemented` | functions | Clojure |
| [F440](F440-multimethods.md) | Multimethod dispatch | `designed` | `not-implemented` | functions | Clojure |
| [F450](F450-effects-and-purity.md) | Эффекты и проверяемая чистота | `proposed` | `not-implemented` | functions | Typespec |
| [F510](F510-jvm-contracts.md) | Чтение контрактов JVM | `designed` | `partial` | java | Clojure |
| [F520](F520-java-overloads.md) | Java-вызовы и перегрузки | `designed` | `partial` | java | Clojure |
| [F530](F530-java-generics-nullability.md) | Java generics и nullability | `designed` | `partial` | java | Clojure |
| [F610](F610-macro-contracts.md) | Макросы и границы проверяемого кода | `designed` | `partial` | core | Typespec |
| [F611](F611-conditional-threading.md) | Условные шаги cond-> и cond->> | `implemented` | `implemented` | core | Clojure |
| [F612](F612-core-wrapper-bodies.md) | Проверка тел time, lazy-seq и future | `implemented` | `implemented` | core | Clojure |
| [F710](F710-externs-and-exports.md) | Контракты библиотек и экспорт интерфейсов | `designed` | `partial` | core | Typespec |
| [F711](F711-external-declarations.md) | Внешние декларации для библиотек и core | `designed` | `partial` | modules | TypeScript |
| [F720](F720-module-interfaces.md) | Интерфейсы ns и инкрементальная проверка | `designed` | `partial` | core | Typespec |
| [F721](F721-var-initialization.md) | Объявление и инициализация Var | `implemented` | `implemented` | modules | Clojure |
| [F722](F722-project-contract-index.md) | Индекс публичных контрактов и использований | `implemented` | `implemented` | modules | Clojure |
| [F730](F730-strict-namespaces.md) | Строгий режим по namespace | `designed` | `not-implemented` | modules | Flow |
| [F810](F810-type-coverage.md) | Покрытие типами | `designed` | `not-implemented` | diagnostics | Flow |
| [F820](F820-suspicious-nil-lints.md) | Линты подозрительной обработки nil | `designed` | `not-implemented` | diagnostics | Flow |
| [F830](F830-targeted-suppressions.md) | Подавление конкретной диагностики | `designed` | `partial` | trust | Flow |
| [F840](F840-trust-budget.md) | Бюджет доверия и baseline в CI | `designed` | `partial` | core | Typespec |
| [F910](F910-agent-and-editor.md) | Диагностика, редактор и цикл агента | `designed` | `partial` | core | Typespec |
