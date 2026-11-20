---
id: F331
title: Длина и позиции в s/tuple
status: implemented
decision: accepted
implementation: implemented
area: data
origin: Clojure
---

# F331 · Длина и позиции в `s/tuple`

`s/tuple` сохраняет порядок и число элементов. Литералы векторов
дают ту же позиционную информацию. Чекер использует её при передаче
аргументов, деструктуризации, `first`, `second`, `nth` и числовом
шаге `get-in`.

```clojure example
(ns example (:require [clojure.spec.alpha :as s]))

(s/fdef text :args (s/cat :pair (s/tuple int? string?)) :ret string?)
(defn text [[id value]] (subs value id))

(text [1 "ready"])
(text ["ready" 1])
(text [1])
```

```text output
7:7  TSP2345  vector element 0: "ready" is not assignable to int?
8:7  TSP330-01  expected vector length 2, got 1
```

Деструктуризация короткого вектора связывает отсутствующую позицию
с `nil`. `nth` с известным индексом за границей выдаёт `TSP330-02`,
если default отсутствует. Присутствующий `nil` сохраняется и при
заданном default. Вектор неизвестной длины даёт `TSP210-01` на
границе tuple-контракта.

[Длина и непустые коллекции](F330-tuples-and-nonempty.md) · [Регрессии](../test/typespec/tuples_and_rows_test.clj)
