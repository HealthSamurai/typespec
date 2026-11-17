---
id: F320
title: Nil, коллекции и платформенные типы
status: designed
decision: accepted
implementation: partial
area: core
origin: Typespec
---

# F320 · Nil, коллекции и платформенные типы

## Мотивация и границы

В Clojure `nil` — обычный результат поиска, чтения необязательного ключа и работы с пустой последовательностью. Запрещать его повсюду неудобно; считать любое чтение успешным опасно. Проверка должна различать отсутствие значения, допустимый `nil` внутри данных, пустую коллекцию и неизвестную Java-nullability. `false` — значение, а не отсутствие: `when-let` и `when-some` имеют разную семантику.

Карточка задаёт общую nil-семантику на пересечении коллекций, narrowing и Java. Она не проектирует заново структурные карты, Java overload resolution или lint стиля F820. `platform` — разрешённое допущение, **не доказательство non-nil**. `--strict-nil` усиливает проверки прототипа, но пока не даёт полной гарантии отсутствия NPE.

## Что действительно реализовано

По исходникам, а не только по обещаниям reference:

- Типы содержат `:nil`, `:union`, `:coll` с `:of`, карты `:req`/`:opt`; platform представлен меткой `:platform true`. `t/platform` не помечает примитивные hints, `nil` и `any`.
- `key-info`/`read-key` добавляют `nil` для optional-ключа и для required-ключа nullable-карты. Чтение известного spec-ключа из неизвестной карты может вернуть platform; обещанный reference запрет любого чтения из unknown пока не универсален.
- `eff` превращает **верхнеуровневый** platform в union с nil при `:strict-nil`. Его вызывают при проверке совместимости и receiver Java-вызова; это не рекурсивная нормализация всех вложенных типов.
- `first`/`second`/`last` и несколько других операций используют `indexed`: элемент становится platform. `seq`/`next` возвращают nullable-коллекцию, но факта непустоты нет. Для `first nil` получается `never` через `elem-type`, а не обещанный reference `nil`: известное расхождение, не целевое правило.
- `nth` возвращает тип элемента, не учитывает границы и тип default. `get` с keyword-default удаляет весь nil из результата чтения — это теряет различие «ключ отсутствует» и «ключ присутствует со значением nil».
- `narrow`, `if-let-form`, `when-let-form` удаляют nil. `false` отдельно не исключается; параметр `some?` в `if-let-form` не используется. Удаление nil также не снимает platform, поэтому guard не всегда помогает в strict-режиме.
- Java-результаты берутся из reflection; для нескольких методов (`Map.get`, `Queue.poll`, `System.getenv` и др.) есть явный список nullable. Полная обработка nullability-аннотаций и Java generic-return из reference здесь не реализована. `Object` превращается в `any`, что дополнительно ослабляет проверку.

## Примеры и ожидаемое поведение

Это обычные иллюстрации, не golden-тесты. `Current` ниже — вывод из указанных исходников, не утверждение, что эти блоки прогонялись тестовым раннером. Каждый блок самодостаточен.

### 1. Корректный guard над явно nullable строкой — current

```clojure
(ns nil-safe (:require [clojure.spec.alpha :as s]
                       [clojure.string :as str]))
(defn label
  {:t/sig '[(s/nilable string?) :-> string?]}
  [x]
  (if (some? x) (str/upper-case x) "unknown"))
;; Current: в then x имеет string?, результат string?, nil-ошибки нет.
;; Это явный union, не platform: пример не обещает снятия Java-допущений.
```

### 2. Ошибка чтения необязательного значения — current

```clojure
(ns nil-optional (:require [clojure.spec.alpha :as s]
                           [clojure.string :as str]))
(s/def ::caption string?)
(s/def ::entry (s/keys :opt [::caption]))
(defn caption
  {:t/sig '[::entry :-> string?]}
  [entry]
  (str/upper-case (::caption entry)))
;; Current: аргумент upper-case может быть nil, TSP2531.
;; Исправление: связать чтение с локальной переменной и проверить some?.
;; Optional-ключ и nullable-значение required-ключа — разные причины nil.
```

### 3. Пустота и false — пограничные случаи, planned уточнение

```clojure
(ns nil-edges (:require [clojure.spec.alpha :as s]))
(defn head-value
  {:t/sig '[(s/coll-of boolean?) :-> (s/nilable boolean?)]}
  [xs]
  (when-let [ys (seq xs)] (first ys)))
(head-value [])      ; Clojure: nil
(head-value [false]) ; Clojure: false — непустота не означает truthy-элемент
(when-let  [v false] v) ; Clojure: nil
(when-some [v false] v) ; Clojure: false
(first nil)            ; Clojure: nil
;; Current: нет NonEmpty-факта; first использует platform, first nil — never.
;; Current: when-let/when-some имеют одинаковую схему удаления только nil.
;; Planned: различать truthy и non-nil, пустоту контейнера и nil его элемента.
```

### 4. Default не заменяет присутствующий nil — ошибка модели current

```clojure
(ns nil-default (:require [clojure.spec.alpha :as s]
                          [clojure.string :as str]))
(s/def ::caption (s/nilable string?))
(s/def ::entry (s/keys :opt [::caption]))
(defn caption
  {:t/sig '[::entry :-> string?]}
  [entry]
  (str/upper-case (get entry ::caption "untitled")))
(get {} ::caption "untitled")              ; Clojure: "untitled"
(get {::caption nil} ::caption "untitled")  ; Clojure: nil
(nth [] 0 :missing)                        ; Clojure: :missing
;; Current: get удаляет nil целиком, поэтому пропускает ошибку caption.
;; Current: nth игнорирует тип default; это не проверка безопасности индекса.
;; Planned: сохранить nil присутствующего значения; default включать
;; только в исход «отсутствует / индекс вне границ» соответствующей операции.
```

### 5. Java hint — допущение, а не non-nil

```clojure
(ns nil-platform)
(defn size [^String x] (.length x))
(defn guarded-size [^String x]
  (if (some? x) (.length x) 0))
(defn primitive-id [^long x] x)
;; Current: size без strict-nil допускает receiver platform String.
;; Current: с --strict-nil size даёт TSP2531.
;; Current: guarded-size тоже может получить TSP2531: remove-nil сохраняет
;; platform, eff снова добавляет nil. Это пробел, а не рекомендуемое поведение.
;; Planned: guard доказывает non-nil для данного локального значения.
;; ^long не становится nullable из-за включения strict-nil.
```

## Алгоритм проверки и интеграция

### Текущий путь (воспроизводимое описание, не sound-алгоритм)

1. **Вход:** AST из `parse`, окружение specs/signatures/hints из `env`, локальные типы в `ctx`, опция `:strict-nil`, ожидаемый тип аргумента или результата. Выражение проходит `infer` и правила `special` в `check.clj`.
2. **Представление:** раскрыть ссылки через `members`/`resolve-t`; nil — член union, platform — метка типа. `:why` и metadata `:null-return` дают происхождение риска, но не являются системой логических доказательств.
3. **Чтение данных:** `lookup-key` различает required/optional/missing/unknown; `key-info` и `read-key` строят результат. `elem-type` получает элемент коллекции, `indexed` ослабляет обещание наличия до platform. Правила `seq` добавляют nil без факта непустоты.
4. **Ветвление:** `narrow` создаёт контексты веток, `remove-nil` удаляет явный nil; binding-формы привязывают уточнённый тип. Результаты веток объединяются `union`. False-фильтров и общей системы presence-фактов нет.
5. **Граница использования:** `problems` применяет `eff` к actual, затем `sub?`. Если совместимость восстановилась бы удалением nil, выдаётся существующий `TSP2531`; иначе возможен `TSP2345` для несовместимого типа. Java receiver отдельно проходит `eff` и nil-проверку в `method-call`. Отсутствующий ключ — существующий `TSP2339`, не nil-ошибка.
6. **Результат:** выведенные типы и диагностики с позицией и доступным объяснением причины nil. Отсутствие ошибки при platform/any не равно доказательству безопасности.

### Требуемое расширение (planned, нужны отдельные исследования)

1. **Разделить информацию:** тип значения `T | nil`, наличие ключа `present/absent/unknown`, пустоту `empty/nonempty/unknown`, nullability платформенного источника и доказательство guard. Это проектируемые факты, не существующие поля AST. Согласовать их с narrowing и типами коллекций, не создавать параллельную систему внутри F320.
2. **Нормализовать источник:** различать reference и primitive Java-типы; nullable-контракт должен сохраняться и в обычном режиме. Platform-политика управляет допущением, но не подменяет `unknown` доказанным типом. Определить рекурсивное поведение для union, refs и вложенных функций до изменения `eff`.
3. **Применить точную семантику операции:** keyword/get допускают nil-контейнер; default выбирается по отсутствию ключа, не по nil значения. `first` пустого входа даёт nil; непустой вход даёт тип элемента, который сам может содержать nil/false. Для `nth` отдельно описать нормальный возврат, default и исключение вне границ — не добавлять nil автоматически вместо исключения.
4. **Добавить фильтры:** truthy исключает nil и false, non-nil исключает только nil. Успешный `(seq xs)` даёт факт непустоты результата seq; перенос на исходную коллекцию требует отдельного обоснования. Guard локального platform-значения должен предотвращать повторное добавление nil, сохраняя происхождение доверия.
5. **Ограничить срок факта:** только стабильная локальная привязка; новое чтение Java-коллекции, atom или другого изменяемого источника не наследует прежнюю проверку. На join сохранять лишь общие факты. Полнота таких правил для aliasing, lazy seq и повторных вычислений пока не доказана.
6. **Проверить использование и объяснить:** сохранить текущий `TSP2531` там, где проблема именно nil; показывать источник (optional key, пустой результат, Java contract/platform) и место guard. Для выхода за границы понадобится отдельное решение о диагностике; новых кодов эта карточка не вводит.

Этот план — архитектурное направление. До реализации нужны модель presence и truthiness, правила переноса фактов и контрпримеры к каждому правилу. Нельзя называть sketch sound или обещать, что `--strict-nil` покрывает все ошибки Java/коллекций.

## Конкретная матрица тестов

Каждый новый случай проверять отдельно в обычном и strict-nil режиме; проверять не только число ошибок, но тип результата, код, позицию и причину. «Регрессия» ниже — нужный тест, а не уже добавленный.

| Случай | Сейчас по исходникам | Целевое ожидание / проверка |
|---|---|---|
| `some?` над `string? | nil`, затем `upper-case` | Сужение работает | Пример 1 без TSP2531 в обоих режимах |
| Optional string key → `upper-case` | TSP2531 | Пример 2, причина optional |
| Required string key у nullable map | Чтение nullable | TSP2531 до guard карты, отсутствие ошибки после |
| Required nilable key + default | nil удаляется ошибочно | Регрессия: nil сохранить; TSP2531 при строковом потребителе |
| Optional non-nil key + default string | Объединение с default | Строка без nil; отдельно проверить отсутствие и присутствие |
| `get nil :k default` | Обрабатывается через nil-тип | Результат default; без default — nil |
| `first nil`, `first []` | Возможен never | Регрессия: ровно nil, не unreachable |
| `first` коллекции строк | Platform элемент | Зафиксировать обычное допущение и TSP2531 в strict-потребителе |
| `seq` пустого/непустого входа | Nullable coll без NonEmpty | В then непустой результат; nil/false элемента не теряются |
| `when-let`/`when-some` с false | Одинаковая схема типов | Разные достижимые ветки и результаты, пример 3 |
| `nth [] 0 :missing` | Default не учтён | Тип включает :missing; `nth [] 0` — исключение, не nil |
| Hint `^String` → `.length` | Обычный режим принимает, strict сообщает | Пример 5; отдельно guard должен снять риск после исправления |
| Hint `^long` | Без platform nil | Одинаковая nullability в обоих режимах |
| `System.getenv` → строковый потребитель | Явный nullable из списка | TSP2531 в обоих режимах; после local guard принять |
| `Map.get`, return Object | any ослабляет результат | Не объявлять доказанным non-nil; тестировать отдельно от String-return |
| Повторное чтение изменяемого источника | Общей модели фактов нет | Guard первого чтения не оправдывает второе |

Существующий `demo_test.clj` сравнивает snapshots проектов, включая `demo` с `:strict-nil true`; `test/features/src/f/core.clj` содержит nil/narrowing и optional-key примеры. Это полезная база, но не свидетельство покрытия всей таблицы. Особенно нужны регрессии для default, false, platform guard и пустого `first`.

## Ограничения и критерий готовности

`any`/unknown, erased Java generics, неполная обработка annotations, верхнеуровневый `eff` и общие приближённые правила коллекций ограничивают гарантии. Общая ветка для `first`, `rand-nth`, `ffirst` не отражает различия исключений и вложенности. Не обещаем анализ границ индексов, завершения lazy seq или гонок в mutable Java-объектах.

Готовность: согласованы правила presence/truthiness/platform; устранены перечисленные расхождения с runtime; матрица стала исполняемыми проверками без маскировки через any; reference явно различает реализованное и план. `implementation: partial` сохраняется: базовая nil-проверка есть, полная семантика пока отсутствует.

## Проверенные локальные источники

- [Reference](../docs/reference.md): разделы вывода/nil-punning, hints, Java nullability и strict-nil — целевой контракт, не инвентаризация реализации.
- [check.clj](../src/typespec/check.clj): `eff`, `key-info`, `read-key`, `problems`, `narrow`, `elem-type`, `indexed`, binding-формы, `get`, `nth`, `java-ret`, `method-call`.
- [types.clj](../src/typespec/types.clj): `platform`, `class-type`, `nilable?`, `remove-nil`, `sub?`; [env.clj](../src/typespec/env.clj): `hint-type`.
- [java.clj](../src/typespec/java.clj): reflection и `nullable-returns`; [core_types.clj](../src/typespec/core_types.clj): контракты строковых потребителей.
- [Тестовый раннер](../test/typespec/demo_test.clj), [feature-примеры](../test/features/src/f/core.clj): существующий способ проверки и частичное покрытие.
