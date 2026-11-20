---
id: F431
title: Проверка reify и extend-type по s/fdef
status: implemented
decision: accepted
implementation: implemented
area: functions
origin: Clojure
---

# F431 · Реализации методов по `s/fdef`

Чекер собирает методы и арности из `defprotocol`. `s/fdef` задаёт
типы параметров и результата для вызовов и тел `reify` и `extend-type`.
Протокол и реализация могут быть в разных namespace.

```clojure example
(ns example (:require [clojure.spec.alpha :as s]))

(defprotocol Display (render [this label]))
(s/fdef render :args (s/cat :this any? :label string?) :ret string?)

(reify Display (render [_ label] 42))
```

```text output
6:34  TSP2322  returns 42, but the signature says string?
```

В `reify` тело видит захваченные локальные значения. Для
`extend-type` receiver получает известный тип класса, если его
можно разрешить. Receiver учитывается в арности метода. Повторная
реализация, лишний метод и конфликт арности дают `TSP430-01`.
Отсутствующее тело даёт `TSP430-02`, отсутствие сигнатуры для тела
даёт `TSP430-03`. В строгом режиме последние два кода становятся ошибками.

Область правила ограничена Clojure-протоколами из выбранных
исходников. Допустимость runtime-диспетчеризации, Java-интерфейсы,
`extend-protocol` и расширения через metadata остаются отдельными задачами.

[Протоколы](F430-protocols.md) · [Регрессии](../test/typespec/protocol_contracts_test.clj)
