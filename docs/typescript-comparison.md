# TypeScript и Typespec: возможности и реальное состояние

Сравнение статической проверки TypeScript и нашего движка для Clojure. **Дизайн Typespec не равен реализации.** «Частично» означает ограниченные правила прототипа, а не полную поддержку механизма. Источники и исследование: [TS/Flow](../features/TS-FLOW-RESEARCH.md), [находки ревью](../features/REVIEW-FINDINGS.md).

| Возможность | TypeScript | Наш движок сейчас | Цель / карточка |
|---|---|---|---|
| Типы из существующего кода | Аннотации TS, JSDoc/checkJs для JS | Читает spec, hints, :pre и t/sig; согласование ограничено | [F110](../features/F110-existing-clojure.md) |
| Сигнатуры функций | Параметры, результат, проверка тела и вызовов | Поддержаны основные s/fdef и t/sig | [F112](../features/F112-spec.md), [F115](../features/F115-explicit-signatures.md) |
| Безопасный unknown | Любое значение можно положить; для конкретных операций нужно сужение | **Пока небезопасен:** совместимость часто допускает unknown как любой тип | [F210](../features/F210-unknown-any-never.md) |
| Явный any | Отключает ограничения типов, распространяется по операциям | Есть any?, но граница с unknown недостаточно разделена | [F210](../features/F210-unknown-any-never.md) |
| Never / недостижимый код | never для невозможного нормального результата | Есть представление never; распространение по потоку неполно | [F420](../features/F420-control-flow.md) |
| Локальный вывод типов | Из выражений и ожидаемого контекста | Работает частично; есть специальные правила core-функций | [F220](../features/F220-local-inference.md) |
| Generics с ограничениями | Параметры типов, bounds и inference | Общий механизм не реализован | [F230](../features/F230-generics-and-hof.md) |
| Функции высшего порядка | Контекстные callbacks, generic functions; вывод имеет ограничения | map/filter/reduce частично; comp/transducers не гарантированно проверены | [F230](../features/F230-generics-and-hof.md) |
| Арности и overloads | Overload signatures и implementation signature | Основные арности; выбор перегрузок эвристический; apply не реализован как точная проверка | [F410](../features/F410-arities-and-apply.md) |
| Tuples и variadic tuples | Типы позиций, rest/optional, вариадика | Ограниченная поддержка последовательностей; точные длины и apply требуют работы | [F330](../features/F330-tuples-and-nonempty.md) |
| Unions и литеральные типы | Объединения типов и литералов | Базовые unions и literals работают | [F240](../features/F240-narrowing-and-exhaustiveness.md) |
| Сужение по условиям | typeof, instanceof, in, equality, control flow | Базовые predicates, nil-проверки и ветки; есть пробелы | [F240](../features/F240-narrowing-and-exhaustiveness.md) |
| Tagged unions | Связь discriminant с остальными полями | Корреляция полей мап пока не реализована полноценно | [F240](../features/F240-narrowing-and-exhaustiveness.md) |
| Полнота ветвления | Обычно проверяется через never; не каждый switch автоматически обязан быть полным | Есть ограниченные проверки case; не все cond/dispatch | [F240](../features/F240-narrowing-and-exhaustiveness.md) |
| Пользовательские guards | Type predicates; явная сигнатура не доказывает тело | Есть built-in predicates; общие checked guards не реализованы | [F250](../features/F250-checked-guards.md) |
| Nullable-типы | strictNullChecks различает null/undefined | nil и platform-типы, strict-nil; остаются ложные успехи | [F320](../features/F320-nil-and-platform.md) |
| Optional property ≠ nullable value | Уточняется exactOptionalPropertyTypes | req/opt моделируются; get/default ошибочно может удалять явный nil | [F310](../features/F310-maps-and-destructuring.md) |
| Безопасность индексации | noUncheckedIndexedAccess добавляет undefined | Границы из Spec, позиции tuple, непустота после `seq` и проверка литеральных индексов | [F330](../features/F330-tuples-and-nonempty.md) |
| Структурные объекты / мапы | Структурная совместимость | Основные s/keys и map-операции работают частично | [F310](../features/F310-maps-and-destructuring.md) |
| Общий тип доменного ключа | Обычно поле объявляется в каждом интерфейсе или через переиспользуемый тип | Qualified key получает тип из s/def; это не доказывает присутствие/валидность в неизвестной мапе | [F120](../features/F120-key-is-type.md) |
| Exact object | Нет общего exact-object режима; есть excess-property check свежих литералов | t/exact в дизайне; не считать проверку опечаток полной exactness | [F310](../features/F310-maps-and-destructuring.md) |
| Деструктуризация | Перенос типов полей/позиций, defaults | Частично; :strs/:syms, defaults и новые директивы имеют ограничения | [F113](../features/F113-destructuring.md) |
| Pick/Omit/Partial/keyof/indexed access | Поддержаны | Пять операторов работают для одной объявленной формы `s/keys` в `:t/sig` и EDN | [F270](../features/F270-type-operators.md) |
| Conditional/mapped/template-literal types | Поддержаны | Не реализованы; не обязательны для первого выпуска | [Исследование](../features/TS-FLOW-RESEARCH.md) |
| as const / satisfies | Сохранение литералов и проверка совместимости с сохранением исходного типа | `t/const` сохраняет литеральные элементы вложенных коллекций. `t/satisfies` проверяет поддерживаемые контракты | [F280](../features/F280-literal-preservation.md) |
| Рекурсивные типы | Recursive aliases с ограничениями | Регулярные `s/keys`, коллекции и объединения сравниваются структурно; ошибка указывает путь до поля | [F295](../features/F295-recursive-types.md) |
| Номинальные brands | Обычно паттерн через unique symbol и пересечение | В дизайне, не реализованы; brand не доказательство валидации | [F290](../features/F290-brands.md) |
| Уточнения runtime-предикатами | Произвольная логика валидатора статически не доказывается | s/and аппроксимируется; свидетельства валидации — план | [F260](../features/F260-opaque-refinements.md) |
| Изменяемые контейнеры / variance | readonly и правила совместимости с известными компромиссами | Общая модель atom/volatile/channel пока отсутствует | [F340](../features/F340-mutation-and-variance.md) |
| Java interop | JS APIs описываются declarations | Reflection-контракты, вывод номинального результата наследуемых generic-методов, platform-типы и частичные Java-перегрузки | [F510](../features/F510-jvm-contracts.md), [F520](../features/F520-java-overloads.md), [F530](../features/F530-java-generics-nullability.md) |
| Макросы Clojure | Прямого аналога нет | Встроенные правила; неизвестные тела могут пропускаться | [F610](../features/F610-macro-contracts.md) |
| Protocols / multimethods | Не совпадают с TS interfaces или overloads | Полные механизмы не реализованы | [F430](../features/F430-protocols.md), [F440](../features/F440-multimethods.md) |
| Отдельные аннотации библиотек и core | .d.ts, стандартные lib declarations | Встроенный core-каталог есть; внешний загрузчик аннотаций пока нет | [F130](../features/F130-namespace-sidecars.md), [F710](../features/F710-externs-and-exports.md) |
| Модули и инкрементальность | Declarations, project references, incremental builds | sig есть; кэш интерфейсов и инвалидация ещё не реализованы | [F720](../features/F720-module-interfaces.md) |
| Строгий режим | strict и отдельные опции; строгая политика зависит от конфигурации | --strict требует публичные сигнатуры; strict по ns — план | [F730](../features/F730-strict-namespaces.md) |
| Подавление ошибок | @ts-ignore, @ts-expect-error; второй замечает отсутствие ошибки, но не задаёт конкретный код | ignore и unused-проверка есть; фильтр по коду не реализован | [F830](../features/F830-targeted-suppressions.md) |
| Покрытие типами | Не стандартная команда tsc; возможны сторонние инструменты | Есть записи типов, но полноценного покрытия нет | [F810](../features/F810-type-coverage.md) |
| Бюджет доверия | Не стандартная функция tsc | Счётчики есть; baseline/CI-gate пока нет | [F840](../features/F840-trust-budget.md) |
| CLI / редактор | tsc и language service | check/sig/type-at/JSON; полноценная LSP-интеграция — план | [F910](../features/F910-agent-and-editor.md) |

## Что делать первым

1. Разделить безопасный unknown, явный any и состояние «анализ не завершён».
2. Убрать ложный успех при достижении глубины сравнения типов.
3. Зафиксировать регрессионные тесты для nil/default, overloads и HOF.
4. Реализовать внешние аннотации библиотек/core, затем развивать общие generics.

Это не таблица «кто лучше»: языки и зрелость инструментов различны. Наличие типа результата ещё не означает, что проверены все выражения, давшие этот результат. Подробные первичные ссылки на документацию TypeScript собраны в исследовании выше.
