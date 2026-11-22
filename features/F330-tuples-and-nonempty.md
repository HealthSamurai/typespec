---
id: F330
title: Tuples, длина и непустые коллекции
status: implemented
decision: accepted
implementation: implemented
area: data
origin: Clojure
flags: strict-nil
---

# F330 · Длина и непустые коллекции

Длина берётся из литералов, `s/tuple` и `s/coll-of`. Обычный
`typespec check` учитывает `:min-count` и `:max-count` при передаче
аргументов и проверке возвращаемого значения. Для `:kind vector?` и
`:kind set?` опция `:count` задаёт точную длину. Для последовательности
без гарантии `counted?` из `:count` следует нижняя граница, как
в проверке через `bounded-count` в Clojure Spec.

`(partition-all n coll)` и `(partition-all n step coll)` возвращают
последовательность групп, каждая из которых сохраняет тип элемента входа.
При известном положительном целом `n` размер группы составляет от одного
до `n` с учётом длины исходной коллекции. Известные длина входа и
положительный целый шаг дают границы числа групп. `pg --accept` проверяет
типы трёх вызовов `(partition-all 2 pairs)` в SQL-обработчиках.

```clojure example
(ns example (:require [clojure.spec.alpha :as s] [clojure.string :as str]))

(s/fdef label :args (s/cat :xs (s/coll-of string? :kind vector? :min-count 1)) :ret string?)
(defn label [xs] (first xs))

(label ["ready"])
(label [])

(s/fdef guarded :args (s/cat :xs (s/coll-of string? :kind vector?)) :ret (s/nilable string?))
(defn guarded [xs]
  (when-let [items (seq xs)]
    (str/upper-case (first items))))
```

```text output
7:8  TSP330-01  expected collection length at least 1, got 0
```

В ветке успеха `(seq xs)` длина исходного устойчивого значения и
результата составляет минимум один элемент. Если элемент может
быть `nil`, `first` сохраняет этот вариант. `rest` может вернуть
пустую последовательность. `next` и остаток в `& tail` при
исчерпанном входе возвращают `nil`. Деструктуризация по отсутствующей
позиции связывает `nil`.

`nth` и числовой шаг `get-in` используют известные границы. Default
добавляется для отсутствующей позиции и сохраняется отдельно от
допустимого значения `nil`. Для `nth` без default доказанный выход
за границу даёт `TSP330-02`. `apply` проверяет фиксированные позиции
tuple и однородные хвосты известной длины до 64 элементов.

Вычисляемые индексы и арифметические сравнения с `count` остаются
за пределами анализа границ. Результат `filter` может быть пустым.
Операции с неподтверждённым изменением длины сбрасывают её ограничения.

[Точные позиции s/tuple](F331-fixed-vector-positions.md) · [Регрессии F330](../test/typespec/collection_length_test.clj)
