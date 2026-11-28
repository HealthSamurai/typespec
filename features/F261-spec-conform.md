---
id: F261
title: Тип результата s/conform для s/or
status: implemented
decision: accepted
implementation: implemented
area: types
origin: Clojure
---

# F261 · Тип результата `s/conform` для `s/or`

Тип результата содержит тег и тип значения той же ветки `s/or`,
а также `:clojure.spec.alpha/invalid`. После проверки `s/invalid?`
можно разобрать успешный результат векторным destructuring.
`case` сужает значение по тегу и проверяет полноту веток.

```clojure example
(ns example (:require [clojure.spec.alpha :as s]))

(s/def ::input (s/or :text string? :count int?))

(defn render [raw]
  (let [result (s/conform ::input raw)]
    (if (s/invalid? result)
      "invalid"
      (let [[tag value] result]
        (case tag
          :text (inc value))))))
```

```text output
10:9  TSP8001  case is not exhaustive: :count is not handled
11:22  TSP2345  string? is not assignable to number?
```

Спецификация может быть объявлена в другом namespace, задана через alias
или передана как литеральный `s/or`. Успешная ветка поддерживает
`key`, `val`, `first`, `second` и `nth` с известным индексом.
Если `s/and` заканчивается `s/or`, его результат сохраняет те же теги.
Внутри ветки внешнего `s/or` форма `s/and` даёт тип результата своего последнего Spec.
`s/with-gen` сохраняет результат вложенного Spec. Для анализа тегов генератор не вызывается.
`s/nonconforming` возвращает исходное значение либо `:clojure.spec.alpha/invalid`.
После проверки `s/invalid?` доступны ограничения входного Spec, включая
исходные значения веток `s/or`.
Тип вложенного значения выводится для простых предикатов, множеств
литералов, ссылок, `s/keys`, `s/tuple`, `s/nilable` и вложенного `s/or`.
Если весь Spec задан через `s/keys` или `s/tuple`, успешный
`s/conform` тоже сохраняет форму и типы преобразованных полей или позиций.
У `s/map-of` со стандартным результатом-мапой значения конформируются
по своему Spec. Ключи сохраняют исходный тип при стандартных опциях
и получают преобразованный тип при `:conform-keys true`.
Преобразованные ключи могут совпасть, поэтому число записей может уменьшиться.
Для `s/coll-of` с известным видом вектора, множества или последовательности
результат содержит преобразованные элементы. Ограничение на исходную
уникальность не переносится на преобразованные элементы. Множество
может уменьшиться при совпадении результатов преобразования.
У произвольного `s/conformer` тип значения остаётся `t/unknown`.

`typespec contracts` выводит поле `conform` для таких spec, когда
публичный контракт на них ссылается. Изменение тега отражается как
в индексе, так и в диагностике использующих его namespace.

[Регрессии](../test/typespec/spec_conform_test.clj)
