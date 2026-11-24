---
id: F235
title: Композиция трансдьюсеров в into
status: implemented
decision: accepted
implementation: implemented
area: core
origin: Clojure
---

# F235 · Композиция трансдьюсеров в `into`

В трёхаргументном `into` чекер проверяет прямой `(comp ...)` из
`map`, `map-indexed`, `filter`, `remove`, `keep`, `keep-indexed`,
`mapcat`, `cat`, `take`, `drop`, `take-while` и `drop-while`.
Тип элемента передаётся от первого шага к последнему. `filter` и
`take-while` передают тип из истинной ветки предиката, `remove`
передаёт тип из ложной, а `drop-while` сохраняет тип входа для
оставшихся элементов. `keep` и `keep-indexed` исключают `nil` из
входа следующего колбэка.
Колбэки `map-indexed` и `keep-indexed` получают индекс и текущий элемент.
`mapcat` передаёт дальше тип элемента возвращённой коллекции.
`(comp (map f) cat)` выводит тот же тип элемента. `take` и `drop`
сохраняют этот тип. После `(take 0)` колбэки следующих шагов
проверяются как значения, а их вызовы пропускаются. Вложенный
`comp` обрабатывается по тем же правилам, а пустой сохраняет
тип элемента источника.

```clojure example
(ns example)

(into [] (comp (filter string?) (map count)) [42 "text"])
(into [] (comp (map str) (map inc)) [1])
```

```text output
4:31  TSP2345  inc takes number?, but gets string?
```

Если чекер встречает вычисляемый или неподдержанный шаг, он
выдаёт `TSP210-01` на этом шаге. Последующие шаги проверяются
с неизвестным входным типом. Итоговый тип элементов остаётся
`t/unknown`, поскольку произвольный трансдьюсер может изменить
поведение завершающего шага.

[Прямые `map` и `filter`](F234-transducers-into.md) · [Регрессии](../test/typespec/contextual_callback_test.clj)
