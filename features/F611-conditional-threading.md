---
id: F611
title: Условные шаги cond-> и cond->>
status: implemented
decision: accepted
implementation: implemented
area: core
origin: Clojure
---

# F611 · Условные шаги `cond->` и `cond->>`

Чекер передаёт результат предыдущего шага в первый аргумент
`cond->` или последний аргумент `cond->>`. Каждое условие
проверяется в порядке записи. Шаг проверяется в ветке, где условие
истинно. Если условие может быть ложным, тип результата включает
и тип накопленного значения, переданного без этого шага.

```clojure example
(ns example)

(defn options [tls?]
  (cond-> {:port 80}
    tls? (assoc :tls true)
    true (assoc :name "app")))

(cond-> 1 false (+ "skipped") true inc)
(cond->> "abc" true count true inc)
(cond-> 1 true (+ "bad"))
```

```text output
10:19  TSP2345  "bad" is not assignable to number?
```

До первого возможного шага условие может сузить исходный локальный
аргумент. После объединения вариантов связанные условия повторно
анализируются в обычном окружении. При числе альтернатив больше 32
чекер расширяет результат до `t/unknown` и продолжает проверять
оставшиеся формы.

В `options` поле `:name` есть в обеих ветках. Поле `:tls` есть только
при истинном `tls?`. Для построения шагов используется то же правило,
что и для `->` и `->>`.

[Макросы](F610-macro-contracts.md) · [Регрессии](../test/typespec/macro_contracts_test.clj)
