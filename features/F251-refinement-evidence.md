---
id: F251
title: Область действия и инвалидация фактов о типах
status: proposed
decision: proposed
implementation: not-implemented
area: types
origin: Research
---

# F251 · Область действия и инвалидация фактов о типах

## Зачем

Сужение относится к конкретному значению в конкретной ветке, не к произвольному повторению выражения. Нужна общая модель для guards, spec-свидетельств, изменяемых объектов и инкрементального кэша. Отдельные сужения уже работают; единый протокол происхождения и срока действия фактов — нет.

## Примеры целевого поведения

```clojure
(let [snapshot @state]
  (when (string? snapshot)
    (do-other-work)
    (clojure.string/upper-case snapshot)))
;; Локальная строка стабильна: произвольный вызов не меняет её тип.
```

```clojure
(when (string? @state)
  (clojure.string/upper-case @state))
;; Второй deref получает новое значение; факт первого не переносится.
```

```clojure
(when (s/valid? ::config value)
  (use-config value))
;; Свидетельство относится к этому value и проверенному определению spec.
;; Для mutable-содержимого или stateful-предиката одной identity недостаточно.
```

```clojure
(let [x "text"]
  (when (string? x)
    (let [x 42]
      (consume x))))
;; Новое связывание x не наследует факт старого x.
```

Примеры иллюстрируют требования, не являются golden-тестами текущего checker.

## Проект алгоритма

1. Назначать идентификатор каждому binding и вычисленному snapshot, отдельно от текстового имени.
2. Хранить факт как subject/path, утверждение, область ветки, источник доказательства и зависимости. Отмечать verified и trusted отдельно.
3. Создавать факты только из поддержанных проверок. Custom predicate требует summary F250; validated-spec — правил F260.
4. На стабильных immutable-значениях сохранять факт через вызовы. На новом deref создавать новый subject. Для mutable path инвалидировать при потенциальной записи/aliasing; без модели эффектов выбирать консервативное ограничение.
5. На соединении веток сохранять лишь факты, верные на всех достижимых путях; не путать объединение типов и объединение доказательств.
6. При изменении spec/Var-контракта инвалидировать зависимые summaries в кэше. Это действие checker между снимками проекта, не автоматическая гарантия против runtime redef.
7. Возвращать тип и provenance, чтобы диагностика объясняла, почему факт больше неприменим.

Это проект общего протокола; soundness произвольных mutable graphs, stateful predicates и dynamic redefinition не заявляется. Для pure-предикатов связь с F450 требует отдельной спецификации.

### Planned: зависимости от destructuring и сохранённого условия

Связь `local ↔ [subject, literal-key]` создавать при деструктуризации без default; отсутствие ключа при чтении всё ещё отображается в nil. Положительный `string?` local исключает отсутствие и сужает путь владельца. При `:or` это уже не identity: локальная строка могла прийти из default. Представлять такую связь отдельным условным ребром либо на первом этапе не переносить факт обратно.

```clojure
(let [{:keys [name] :as person} input]
  (when (string? name)
    (clojure.string/upper-case (:name person)))) ; planned: связь с владельцем

(let [{:keys [name] :or {name "guest"} :as person} {}]
  (when (string? name) (:name person))) ; nil, не string?
```

Ограниченный граф зависимостей связывает subjects, paths и boolean-импликации F240. Worklist распространяет поддержанные факты до стабилизации; обратный индекс находит зависимые факты при инвалидации. Join не сохраняет ребро, гарантированное только одной достижимой веткой. При лимите отбросить недоказанное уточнение, не заменить его `any` или выводом unreachable. Mutable referent не становится стабильным из-за immutable binding.

Дополнительные тесты: оба примера, nested literal path, boolean alias после shadowing, цикл зависимостей/лимит и совпадение warm/cold результата после смены внешнего summary. Внешние декларации core/библиотек обязательны и входят в зависимости; никакого автоматического доверия boolean-return.

Источники: [Elixir `Pattern.of_head`](https://github.com/elixir-lang/elixir/blob/v1.20.5/lib/elixir/lib/module/types/pattern.ex), [Sorbet flow-sensitive typing](https://sorbet.org/docs/flow-sensitive). [Отчёт Elixir](../docs/research-elixir.md) читает описание и начало `of_head`, не весь solver; [отчёт Sorbet](../docs/research-sorbet.md) — структуры `KnowledgeFact`/`TestedKnowledge` в `environment.h`, не весь inference. Здесь прочитаны отчёты; граф для Clojure — предложение, без повторного аудита первоисточников и гарантии soundness.

## Матрица тестов

Stable local через вызов; shadowing; повторный deref; alias на mutable объект; ветки с разными фактами; spec reload при тёплом запуске; trusted guard; stateful predicate. Для инкрементального анализа результат должен совпадать с чистой проверкой того же снимка.

## Источники и связи

- [Practical Optional Types for Clojure](https://arxiv.org/abs/1812.03571v1): occurrence typing и пути в Clojure. В обзоре прочитаны отдельные разделы, не всё доказательство.
- [Revisiting Occurrence Typing](https://arxiv.org/abs/1907.05590v8): прочитан abstract; не источник готового алгоритма инвалидации Clojure-spec.
- [Flow: refinement invalidations](https://flow.org/en/docs/lang/refinements/): различие стабильного binding и изменяемого свойства.
- [F250](F250-checked-guards.md), [F260](F260-opaque-refinements.md), [F340](F340-mutation-and-variance.md), [F450](F450-effects-and-purity.md), [F720](F720-module-interfaces.md).

## Статус

Предложение, не реализовано как единая модель. Это уточнение уже существующих идей сужения и доверия, а не обещание новой runtime-инструментации.
