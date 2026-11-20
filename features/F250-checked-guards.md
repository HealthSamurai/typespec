---
id: F250
title: Проверяемые guard-функции
status: implemented
decision: accepted
implementation: implemented
area: narrowing
origin: Flow
flags: strict strict-nil
---

# F250 · Проверяемые guard-функции

Обычный Clojure-предикат может уточнять тип своего аргумента в месте вызова.
Checker читает тело функции, использует существующие spec, сигнатуры и type
hints, затем вычисляет отдельные гарантии для truthy- и falsy-результатов.

```clojure example
(ns example
  (:require [clojure.spec.alpha :as s]
            [clojure.string :as str]))

(s/def ::label string?)
(s/def ::entry (s/keys :req-un [::label]))

(defn- entry? [x]
  (s/valid? ::entry x))

(defn- nonblank-string? [x]
  (and (string? x) (not (str/blank? x))))

(defn label-or-default
  {:t/sig '[t/unknown :-> string?]}
  [x]
  (if (entry? x)
    (str/trim (:label x))
    "unknown"))

(defn normalize-label
  {:t/sig '[t/unknown :-> string?]}
  [x]
  (if (nonblank-string? x)
    (str/trim x)
    "unknown"))

(defn normalize-on-failure
  {:t/sig '[string? :-> string?]}
  [x]
  (if (nonblank-string? x) x (str/trim x)))

(mapv inc (filter some? [1 nil 2]))
(mapv inc (filterv #(some? %) [1 nil 2]))
(mapv inc (remove nil? [1 nil 2]))
(remove nonblank-string? ["" "a" 1])
```

```text output
(no diagnostics)
```

`entry?` уточняет вход до `::entry` в истинной ветке. У `nonblank-string?`
истинный результат гарантирует строку, а ложный допускает пустую строку.
`filter`, `filterv` и `take-while` используют гарантию истинной ветки.
`remove` использует гарантию ложной ветки. `some?` отсеивает `nil` и
сохраняет `false`. Для этих операций нужны вызовы с коллекцией, включая
именованные предикаты, простые `fn` и `#(...)`.

## Явные контракты

```clojure
(defn string-value?
  {:t/guard '{:type string? :mode :checked :two-sided true}}
  [x]
  (string? x))

(defn looks-like-entry?
  {:t/guard '{:type ::entry :mode :checked}}
  [x]
  (map? x)) ; TSP250-01
```

`:mode :checked` требует доказать тип `:type` при истинном результате.
`:two-sided true` дополнительно требует доказать, что ложный результат
исключает этот тип в области входов функции. `map?` сам по себе не
гарантирует обязательный ключ `:label`. Статическое представление
`s/and` описывает первый компонент spec. Отказ `s/valid?` может быть связан
с любым из остальных условий, поэтому ложная ветка сохраняет входной тип.

`{:t/guard '::entry}` и `{:t/guard '{:type ::entry :mode :trusted}}`
задают доверенную гарантию только для истинной ветки. CLI считает каждую
принятую доверенную декларацию один раз. В JSON это поле `trust.guard`.
Проверяемый контракт не использует доверенную гарантию как доказательство.

| Код | Причина |
|---|---|
| `TSP250-01` | Истинный результат не гарантирует заявленный тип |
| `TSP250-02` | Ложный результат не исключает заявленный тип |
| `TSP250-03` | Некорректная декларация, неподдержанное тело, цикл или ошибка типизации тела |

Диагностика guard указывает на значение `:t/guard`. JSON содержит код,
позицию и причину отказа. При автоматическом выводе отсутствие гарантии
не создаёт отдельную диагностику.

## Область анализа

- Один позиционный параметр, одна арность, без вариадика и полиморфизма.
  Факты выводятся для самого параметра. На стороне вызова уточняется
  стабильный локальный binding или поддерживаемое чтение ключа из него.
  Для `@a` можно сначала привязать результат чтения через `let`. Повторный
  `@a` может вернуть другое значение.
- Анализ умеет читать встроенные проверки типа, `some?`, `nil?`, сравнение
  с литералом, `instance?` и `s/valid?` с литеральным ключом. Для
  `and`, `or`, `not`, `if`, `when`, `let` и `do` учитываются пути
  нормального возврата. `throw` завершает путь. Для обычной неизвестной
  функции учитываются обе возможности результата.
- Сводки предикатов пересчитываются из исходников при каждом запуске.
  Вызовы через namespace aliases и `:refer` используют ту же сводку.
  Циклы, неподдержанные макросы, `t/cast`, `t/any` и динамическое
  переопределение в теле не дают проверяемого контракта. Вывод рассчитан
  на стабильные Var и spec между вызовом и потреблением результата,
  в том числе для ленивых коллекций.

[Тесты](../test/typespec/checked_guards_test.clj) закрепляют типы веток,
фильтрацию, диагностики, затенение имён и зависимости между namespace.
Разделение гарантий основано на [Flow Type Guards](https://flow.org/en/docs/types/type-guards/)
и [propositions в Typed Racket](https://docs.racket-lang.org/ts-guide/occurrence-typing.html).
