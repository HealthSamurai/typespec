---
id: F612
title: Проверка тел time, lazy-seq и future
status: implemented
decision: accepted
implementation: implemented
area: core
origin: Clojure
---

# F612 · Тела `time`, `lazy-seq` и `future`

Чекер анализирует исходные тела этих макросов и привязывает диагностики
к выражениям внутри них. `time` сохраняет тип вычисляемого выражения.
`lazy-seq` возвращает ленивую последовательность с типом элементов
из последнего выражения тела. `future` имеет тип
`java.util.concurrent.Future`.

```clojure example
(ns example)

(time (inc 1))
(lazy-seq (map inc [1 2]))
(future (inc "bad"))
```

```text output
5:14  TSP2345  "bad" is not assignable to number?
```

`lazy-seq` с телом, возвращающим `nil`, даёт пустую последовательность.
Если тело возвращает заведомо неподходящий скаляр, чекер выдаёт
`TSP2345`. Исключение внутри `future` или `lazy-seq` не меняет тип
самого объекта. Тип значения после `deref` и момент реализации
ленивой последовательности остаются за рамками карточки.

Непосредственный `def` внутри `time` учитывается при проверке
инициализации Var. Объявление внутри `future` или `lazy-seq`
само по себе не доказывает инициализацию корня для вызывающего кода.

[Макросы](F610-macro-contracts.md) · [Результаты get-ig](../docs/fhir-registry-result.md)
