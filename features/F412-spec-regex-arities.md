---
id: F412
title: Арности из s/+ и конечных s/alt
status: implemented
decision: accepted
implementation: implemented
area: functions
origin: Clojure
---

# F412 · Арности из `s/+` и конечных `s/alt`

`s/fdef` и `s/fspec` читают позиционные аргументы из Clojure Spec.
Повторение `s/+` требует хотя бы один аргумент, `s/*` допускает ноль.
Конечный `s/alt` описывает альтернативные списки аргументов. Правила
работают внутри `s/cat`, при прямых вызовах и при `apply` с хвостом
известной длины. `partial` вычитает уже переданные аргументы из минимума.

```clojure example
(ns example (:require [clojure.spec.alpha :as s]))

(s/fdef total :args (s/cat :items (s/+ int?)) :ret int?)
(defn total [& xs] (reduce + xs))

(s/fdef label :args (s/alt :id int? :pair (s/cat :id int? :name string?)) :ret string?)
(defn label ([id] (str id)) ([id name] (str id " " name)))

(total 1)
(apply total [])
(label 1 "name")
(label "name")
```

```text output
10:1  TSP2554  apply total expects 1+ argument(s), got 0
12:8  TSP2345  "name" is not assignable to int?
```

Конечные ветви могут комбинировать `s/cat`, `s/alt` и `s/?`.
Однородный `s/*` или `s/+` поддерживается в конце списка.
Именованные regex из `s/def` раскрываются в `:args` и внутри `s/cat`
с учётом namespace объявления. Обычный именованный предикат по-прежнему
проверяет одно значение внутри `s/cat`. Самостоятельное использование
regex в качестве типа значения пока даёт `TSP9002` на его объявлении.
Раскрытие ограничено 64 различными ветвями и 256 переходами по ссылкам.
Для неподдержанного regex, включая рекурсию и повторение в середине
списка, checker выдаёт `TSP9002`.

Одно тело Clojure может обслуживать несколько альтернатив. Checker
объединяет типы соответствующих позиций при проверке тела. Связи между
разными позициями из одной альтернативы при этом не сохраняются.

[Регрессии](../test/typespec/spec_args_test.clj)
