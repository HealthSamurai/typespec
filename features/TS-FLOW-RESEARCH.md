# TypeScript и Flow: проверка полноты каталога

## Область и метод

Сопоставление официальной документации с текущими карточками Typespec. Цель — Clojure в целом, не отдельный продукт. Документация описывает возможности TS/Flow, но не доказывает их корректность и не обещает лёгкость переноса. Бинарники TS/Flow в этом исследовании не запускались. Локальные копии прочитанных страниц сохранены в `.hyper/tmp/cegl/ts-flow/`.

Номера ниже соответствуют существующим файлам до предлагаемого перехода на девять групп. Их перенумерация — отдельное механическое действие.

## Сопоставление

| Механизм | TypeScript | Flow | Clojure и карточки |
|---|---|---|---|
| Безопасный неизвестный тип | unknown требует сужения, any отключает ограничения | unknown и any также разделены | F210: критично; прототип пока допускает unknown как любой тип |
| Локальный/контекстный вывод | ожидаемый тип помогает типизировать callback | вывод локальный, ограничения аннотаций документированы | F220: сохранять простой пользовательский интерфейс, не обещать полный HM |
| Generics и ограничения | параметры типов, bounds, inference | generics, bounds, variance; документация предупреждает, что полиморфизм не всегда выводится автоматически | F230: нужен общий механизм, не только специальные правила map/reduce |
| Перегрузки и арности | overload signatures отдельно от implementation signature | intersection/overloaded callable types | Нужна самостоятельная карточка: multi-arity defn, rest, apply, неоднозначность |
| Tuples и вариадика | variadic tuples поддерживают concat/partial-подобные функции | tuples, optional elements и spread | Нужна карточка: s/tuple, вектор фиксированной длины, & args и связь с s/cat |
| Tagged unions и never | discriminated unions, exhaustive never idiom | unions, empty и match с проверкой полноты | F240: case и закрытые наборы вариантов; guards не должны автоматически считаться исчерпывающими |
| Guards | type predicates и assertion signatures | проверяемые guards, в том числе one-sided | F250: предпочтителен вывод проверенного постусловия; отдельно обсудить assert/:pre |
| Сброс сужений | control-flow analysis с собственными компромиссами | явно описан сброс для изменяемых свойств/aliasing, локальные значения устойчивее | F340: сохранять знание о immutable snapshot, не о повторном deref |
| Optional и nullable | exactOptionalPropertyTypes разделяет отсутствие и undefined | optional property допускает отсутствие/void, не null автоматически | F310/F320: отсутствующий ключ и присутствующий nil различаются через contains? |
| Индексация | noUncheckedIndexedAccess добавляет undefined к потенциально отсутствующему элементу | точные tuple-арности, отдельные правила массивов/объектов | F320: get/nth/first, пустота и non-empty; нельзя копировать JS undefined |
| Открытые/точные формы | структурные типы, excess-property checks для свежих литералов; не общая exactness | exact objects по умолчанию, inexact явно | F310: открытые Clojure-мапы естественны; exact и typo-check — отдельные режимы |
| Readonly и variance | readonly, variance параметров и известные компромиссы assignability | readonly types и variance | F340: persistent коллекции отличаются от atom, Java array и mutable collection |
| Номинальные различия | brands обычно кодируются структурно | opaque aliases, прозрачные внутри владельца | F260/F150: brand не доказательство валидатора; отделить identity от validation evidence |
| Сохранение литералов | as const; satisfies проверяет, не заменяя результирующий тип выражения | const expressions и satisfies также документированы | F280: это уже идея обоих языков, не только TS; учитывать contextual typing |
| Операторы типов | keyof, indexed access, utility/mapped/conditional/template literal types | utility, indexed access, conditional infer, mapped и template literal types; key remapping в текущей документации | F270 покрывает базовые операции. Общий язык вычислений типов пока не нужен; не утверждать, что Flow этого не умеет |
| Рекурсивные типы | recursive aliases с ограничениями | рекурсивные aliases через конструктор | Нет карточки: деревья, JSON, рекурсивный spec, циклы разрешения и ограничения вычисления |
| Библиотечные контракты | .d.ts и библиотечные declarations | libdefs и .flow declarations | F710/F130: декларация зависимости доверенная; собственное тело нельзя пропускать из-за sidecar |
| Границы модулей | project references, declaration outputs; isolatedDeclarations — отдельный режим | signature extraction требует нужных аннотаций на границах | F720: graph/cache/invalidation, не просто печать sig |
| Постепенное внедрение | JS/JSDoc/checkJs и строгие опции | strict/strict-local, coverage и suppressions | F110/F730/F810/F830: для Clojure использовать spec/hints/pre, не новую обязательную разметку |

## Что действительно отсутствует как самостоятельные карточки

### Первая очередь

1. **Сигнатуры функций: арности, перегрузки, variadic и apply.** Это не то же, что generics. Пример: `(defn f ([x] ...) ([x y & more] ...))`; проверить выбор ветки, число аргументов и неоднозначность результата.
2. **Tuples, длина и non-empty коллекции.** Пример: после `(seq xs)` что доказано о `(first xs)`; после деструктуризации `[a b]` что известно о длине. Связать с nil-семантикой, не дублировать её.
3. **Рекурсивные типы и завершение проверки.** Пример: дерево с `:node/children` того же типа. Повторная ссылка на тип не должна молча превращаться в успешную проверку unknown.
4. **Контракты ошибок и завершения управления.** `throw`, `assert`, ранний выход, `try/catch/finally`: как меняется результат и достижимость. У TS есть assertion signatures, у Flow guards; буквального соответствия синтаксиса нет. Нужны Clojure-правила, в частности возврат nil из catch.

### Уточнить границы существующих карточек

- Разделить F150 на **генераторы схем** и **номинальные brands**: разные причины использования и критерии готовности.
- Разделить F350 на **records**, **protocols/reify** и **multimethod dispatch**: разные модели открытости.
- Разделить F510 на **чтение JVM-контрактов**, **выбор методов/перегрузок** и **Java nullability/generics**. Type hints перенести в собственную карточку источников.
- F110 раскрыть через hints/spec/destructuring/:pre/t/sig; обзор оставить местом согласования источников. F220 — отдельный вывод из тела.
- F280 уточнить: satisfies — проверка совместимости, не cast и не runtime validation. Flow тоже поддерживает её в прочитанной документации.

### Не включать автоматически

- Полные conditional/mapped/template-literal types: сначала реальные задачи Clojure, которые нельзя закрыть простыми операторами.
- Новый enum или match-синтаксис: сначала использовать keywords, literal unions, case и обычные макросы.
- JS/React/DOM-специфику и TS emit: Typespec не транспилятор и не новый язык.
- Обещание «нет ложных ошибок» или «строгие зависимости полностью проверены»: ни каталог фич, ни strict-режим этого не доказывают.

## Исправления прежних выводов

- Opaque types сами по себе не обеспечивают проверку произвольного spec-предиката.
- Flow strict допускает доверенные library definitions: strict не доказательство чужого тела.
- Coverage не сводится к подсчёту верхнеуровневого unknown. Нужны пропущенные тела и вложенные дыры.
- Количество npm-загрузок и GitHub-звёзд не доказывает причину популярности или размер покрытия библиотек.
- «Flow не даёт ничего для HOF» слишком сильно: generics, inference, tuples и variance полезны как материал. Готовой модели Clojure comp/transducers оттуда не получить.

## Первичные источники

### TypeScript

- [Generics](https://www.typescriptlang.org/docs/handbook/2/generics.html)
- [Functions: overloads, rest, inference](https://www.typescriptlang.org/docs/handbook/2/functions.html)
- [Narrowing, predicates, never](https://www.typescriptlang.org/docs/handbook/2/narrowing.html)
- [Object types](https://www.typescriptlang.org/docs/handbook/2/objects.html)
- [Conditional types](https://www.typescriptlang.org/docs/handbook/2/conditional-types.html)
- [Mapped types](https://www.typescriptlang.org/docs/handbook/2/mapped-types.html)
- [Template literal types](https://www.typescriptlang.org/docs/handbook/2/template-literal-types.html)
- [4.9: satisfies](https://www.typescriptlang.org/docs/handbook/release-notes/typescript-4-9.html)
- [4.0: variadic tuples](https://www.typescriptlang.org/docs/handbook/release-notes/typescript-4-0.html)
- [3.7: recursive aliases/assertion functions](https://www.typescriptlang.org/docs/handbook/release-notes/typescript-3-7.html)
- [noUncheckedIndexedAccess](https://www.typescriptlang.org/tsconfig/noUncheckedIndexedAccess.html)
- [exactOptionalPropertyTypes](https://www.typescriptlang.org/tsconfig/exactOptionalPropertyTypes.html)
- [Project references](https://www.typescriptlang.org/docs/handbook/project-references.html)
- [Checking JS](https://www.typescriptlang.org/docs/handbook/type-checking-javascript-files.html)

### Flow

- [Generics](https://flow.org/en/docs/types/generics/), [functions](https://flow.org/en/docs/types/functions/), [tuples](https://flow.org/en/docs/types/tuples/)
- [Unknown](https://flow.org/en/docs/types/unknown/), [refinements](https://flow.org/en/docs/lang/refinements/), [type guards](https://flow.org/en/docs/types/type-guards/)
- [Objects](https://flow.org/en/docs/types/objects/), [match](https://flow.org/en/docs/match/), [enums](https://flow.org/en/docs/enums/)
- [Aliases, including recursive](https://flow.org/en/docs/types/aliases/), [opaque aliases](https://flow.org/en/docs/types/opaque-types/)
- [Mapped types](https://flow.org/en/docs/types/mapped-types/), [conditional types](https://flow.org/en/docs/types/conditional/), [utilities](https://flow.org/en/docs/types/utilities/)
- [Casts and satisfies](https://flow.org/en/docs/types/casting/), [const expressions](https://flow.org/en/docs/types/const-expression/)
- [Annotation requirements](https://flow.org/en/docs/lang/annotation-requirement/), [declarations](https://flow.org/en/docs/declarations/), [strict](https://flow.org/en/docs/strict/), [coverage](https://flow.org/en/docs/cli/coverage/)

Старые URL некоторых страниц дали 404; выводы основаны на перечисленных доступных страницах. Medium-посты ранее дали 403 и не использовались как прочитанные источники.
