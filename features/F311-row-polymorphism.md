---
id: F311
title: Row polymorphism — сохранение остатка мапы
status: proposed
decision: proposed
implementation: not-implemented
area: data
origin: TypeScript and research
---

# F311 · Row polymorphism — сохранение остатка мапы

## Мотивация и история

Идея уже была в reference §9 как опциональное расширение, с аналогией TypeScript `T extends …` и `T & …`. Это не новая идея из arXiv: исследование предлагает уточнить механизм. Открытая мапа разрешает дополнительные поля; row-переменная связывает их типы между входом и выходом. TypeScript позволяет выразить многие такие сценарии через generics, intersections и utility types, но это не буквальная система row variables из статьи.

## Примеры целевого поведения

```clojure
(defn with-version [m]
  (assoc m :version 1))
(with-version {:id 7 :name "Alex"})
;; Результат сохраняет :id int? и :name string?, добавляет :version int?.
```

```clojure
(with-version {:id 7 :version "old"})
;; :version теперь int?, а не пересечение string? и int?.
;; assoc перезаписывает поле: простое T & {:version int?} здесь неверно.
```

```clojure
(defn without-token [m] (dissoc m :token))
(without-token {:id 7 :token "secret" :active true})
;; Сохраняются :id и :active; :token удалён.
```

```clojure
(defn rename-user [m]
  (assoc m :name (clojure.string/upper-case (:name m))))
;; Требуется обязательное строковое :name; остальные поля сохраняются.
;; Optional :name и присутствующий nil — разные ограничения.
```

Примеры — требования, не результаты проверенной реализации. Синтаксис row-binders пока не утверждён.

## Алгоритм и границы

1. Сначала сравнить два варианта: generic целой мапы с операторами обновления либо явная row-переменная. Не вводить новую грамматику без необходимости.
2. Для ограниченного подмножества представить required/optional/known-absent поля и остаток row. Свежая переменная остатка относится к одному инстанцированию generic-функции.
3. При вызове сопоставить обязательные поля и связать остаток с прочими полями, сохранив типы и presence-факты.
4. Для assoc выполнить right-biased replacement. Для dissoc исключить ключ также из остатка через ограничение отсутствия. merge должен учитывать возможность присутствия правого optional-ключа.
5. Переносить связанную row в результат, не превращая её в any. При dynamic keys, конфликте ограничений или неподдержанной рекурсии явно показывать предел проверки.
6. Унификацию rows, отрицательные ограничения и взаимодействие с unions проектировать отдельно: это не готовый sound solver.

### Planned: закрытость формы не равна завершению inference

Разделить три независимых свойства: (1) разрешены ли дополнительные ключи, (2) связан ли их тип одной row-переменной входа/выхода, (3) завершён ли вывод формы. Luau `sealed` допускает дополнительные поля; не переносить это имя как флаг `exact`. Закрытие inference при возврате из функции не должно само запрещать extra keys или стирать связанную row. Наличие generic table у Luau и open map у Elixir не доказывает готовую row polymorphism для Typespec.

```clojure
(defn with-version [m] (assoc m :version 1))
(let [f with-version]
  (:extra (f {:id 1 :extra "kept"}))) ; planned: string?, связь переживает вызов
```

Тестировать результат через именованную функцию/alias, а не только inline `assoc`: открытость без row допускает `:extra`, но не обещает сохранить его строковый тип. Отдельный отрицательный тест — попытка доказать `exact` лишь по завершённому inference. Представление F310 required/optional/absent и ограничение отсутствия в хвосте остаются прежними; новые флаги не заменяют унификацию rows. При нехватке информации результат безопасно ослабляется, `unknown` не даёт права на произвольные операции. Внешние декларации core и библиотек обязательны; перенос row через extern должен быть заявлен его контрактом, не угадан по имени.

Источники: [Luau Table Types](https://luau.org/types/tables/), [Sealed Table Subtyping RFC](https://rfcs.luau.org/sealed-table-subtyping.html); [отчёт Luau](../docs/research-luau.md), §3.1, документация и RFC прочитаны, solver исследован выборочно, без экспериментов. Здесь прочитан отчёт, первоисточники повторно не аудированы. Это уточнение выбора представления, не решение всех row constraints.

## Матрица тестов

Сохранение неизвестного поля; два независимых вызова с разными остатками; перезапись поля другим типом; удаление ключа; optional overwrite; конфликт required/known-absent; dynamic key; union мап. Проверять связь входа и выхода, а не только допустимость вызова.

## Связи и источники

- [Исходная идея в reference §9](../docs/reference.md#9-optional-extensions-ideas-from-ocaml--f).
- [F230 — generics](F230-generics-and-hof.md), [F310 — мапы](F310-maps-and-destructuring.md), [F270 — операции типов](F270-type-operators.md).
- [TypeScript: generics и constraints](https://www.typescriptlang.org/docs/handbook/2/generics.html).
- [TypeScript: intersection types](https://www.typescriptlang.org/docs/handbook/2/objects.html#intersection-types).
- [An Executable Specification of Typing Rules for Extensible Records based on Row Polymorphism](https://arxiv.org/abs/1707.07872v2). В нашем обзоре прочитан abstract; правила унификации и доказательства ещё не проверены.
- [Practical Optional Types for Clojure](https://arxiv.org/abs/1812.03571v1): HMaps как исходная Clojure-модель; это не доказательство необходимости rows.

## Статус

Не реализовано. Цель сохранения остатка уже обсуждалась; выбор отдельной row-системы остаётся proposed. Сначала определить, какие примеры не решает более простой generic map update.
