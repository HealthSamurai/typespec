---
id: F722
title: Индекс публичных контрактов и использований
status: implemented
decision: accepted
implementation: implemented
area: modules
origin: Clojure
---

# F722 · Публичные контракты по всему проекту

`typespec contracts src --json` запускает полную проверку выбранных
исходников и возвращает публичные Vars, сигнатуры, места их объявления,
транзитивные зависимости от `s/def` и места использования. В записи
зависимости есть тип и исходная позиция spec. Поэтому изменение
поля видно в отчёте при прежнем тексте `s/fdef`.

```clojure example
(ns example (:require [clojure.spec.alpha :as s]))

(s/def ::id string?)
(s/def ::account (s/keys :req-un [::id]))
(s/fdef account :args (s/cat :id ::id) :ret ::account)
(defn account [id] {:id id})
(clojure.string/upper-case (:id (account "A-1")))
```

```text output
(no diagnostics)
```

Индекс различает прямой вызов, передачу значения функции, доступ
к самому Var и реализацию метода протокола. Поддерживаются
квалифицированные имена, alias и явный `:refer` с `:rename`.
Для вычисляемых имён через `resolve` и непрозрачных макросов
полнота индекса не гарантируется.

Несовместимые `s/def` или `s/fdef` с одним именем в разных файлах
дают `TSP722-01`. При полном прогоне изменения обязательных полей,
типов полей и аргументов проверяются и в объявлении, и у потребителей.

[Модульные интерфейсы](F720-module-interfaces.md) · [Сравнение ревизий](F723-public-contract-compatibility.md) · [Трёхфайловый тест](../test/typespec/project_contracts_test.clj)
