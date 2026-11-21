---
id: F320
title: Nil, коллекции и платформенные типы
status: implemented
decision: accepted
implementation: implemented
area: core
origin: Typespec
flags: strict-nil
---

# F320 · Nil и платформенные типы

В режиме `--strict-nil` ссылка Java с неизвестной nullability
допускает `nil`. Проверка учитывает этот вариант внутри мап, кортежей,
коллекций и сигнатур функций. Те же правила действуют при проверке
аргументов, возвращаемых значений и `t/satisfies`.

```clojure example
(ns example (:require [clojure.spec.alpha :as s]))

(s/def ::parent #(instance? java.io.File %))
(s/def ::settings (s/keys :req-un [::parent]))

(s/fdef consume :args (s/cat :settings ::settings) :ret any?)
(defn consume [settings] settings)

(consume {:parent (.getParentFile (java.io.File. "file"))})
(when-let [parent (.getParentFile (java.io.File. "file"))]
  (consume {:parent parent}))
```

```text output
9:19  TSP2531  at [:parent]: java.io.File may be nil, expected java.io.File
```

В аргументе возможный `nil` даёт `TSP2531`, в возвращаемом значении
`TSP2322`. Для поддерживаемых структурных форм диагностика содержит
путь из ключей и индексов. При передаче литерала диапазон указывает
на вложенное выражение. В JSON путь доступен в `type-path`.

`when-let` и `some?` сужают устойчивое значение в ветке успеха.
`when-some` пропускает `false` и исключает `nil`. Повторный
Java-вызов проверяется самостоятельно. При объединении веток
возможный `nil` сохраняется независимо от их порядка.

В обычном режиме platform по-прежнему допускает Java-вызовы по
статическому типу. Явный `s/nilable` и известные nullable-результаты
Java учитываются в обоих режимах. Default в `get` и `nth` применяется
при отсутствии позиции, а допустимый `nil` внутри данных сохраняется.
Непустота после `seq` описана в [F330](F330-tuples-and-nonempty.md).

Флаг опирается на отражённый JVM-тип, известные nullable-результаты
и явные контракты. Общий анализ Java-аннотаций и вывод nullability
для значений, стёртых до `Object`, остаются за пределами проверки.

[Межмодульные регрессии](../test/typespec/platform_nullability_test.clj)
