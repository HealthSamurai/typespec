# Luau → Typespec: полезные механизмы, а не перенос языка

Дата проверки: **2026-11-17 UTC**. Объект: статическая проверка существующего Clojure в Typespec, без новых runtime checks. Это не исследование Aidbox. Изменён только этот документ; карточки, source, index и commits не менялись.

## Summary

Из Luau стоит брать прежде всего **архитектуру локального вывода и правила проверки**, а не синтаксис или весь solver: expected types для callbacks, независимую инстанциацию generics, явные зависимости ограничений, ограниченную нормализацию unions/intersections и честное сообщение о незавершённой проверке. Эти направления уже имеют карточки Typespec — новых карточек не требуется.

Три наиболее важных предостережения: (1) `sealed` в Luau не означает exact; (2) Lua смешивает отсутствие поля и nil, Clojure обязан их различать; (3) Luau type functions действительно выполняют код при анализе. Последнее не добавляет runtime checks приложению, но перенос в виде произвольного Clojure `eval` противоречил бы модели Typespec «типы — инертные данные». Начинать нужно с закрытого набора операторов F270, не с пользовательского вычислителя типов.

## 1. Источники и границы проверки

### Прочитанные первоисточники

**Текущая официальная документация:**

1. [An introduction to Luau types](https://luau.org/types/) — gradual typing, strict/nonstrict/nocheck, structural typing, casts. Старый `/typecheck` отдаёт HTML-переадресацию на `/types`.
2. [Table Types](https://luau.org/types/tables/) — unsealed/sealed/generic tables, exactness, width subtyping, indexers.
3. [Generics and Polymorphism](https://luau.org/types/generics/) — generic aliases/functions, inference и уже документированная явная инстанциация `func<<Type>>(...)`.
4. [Type Functions](https://luau.org/types/type-functions/) — выполнение во время анализа, доступное окружение; дополнительно просмотрена [Type Function Library](https://luau.org/types-library/).

**RFC, прочитанные как документы дизайна, не как автоматическое доказательство текущей реализации:**

5. [Generic Functions](https://rfcs.luau.org/generic-functions.html).
6. [Expanded Subtyping for Generic Function Types](https://rfcs.luau.org/generic-function-subtyping.html) — инстанциация при subtyping, опасность контравариантной позиции и mutable tables. Вводная фраза об отсутствии explicit instantiation историческая: текущая документация выше уже описывает синтаксис.
7. [Sealed Table Subtyping](https://rfcs.luau.org/sealed-table-subtyping.html).
8. [User-Defined Type Functions](https://rfcs.luau.org/user-defined-type-functions.html) — помечен **Implemented**, описывает программирование над типами и ограничения среды. Это больше, чем roadmap; наличие runtime подтверждается исходниками.

**Техническая статья:**

9. [Semantic Subtyping in Luau, 2022-10-31](https://luau.org/news/2022-10-31-luau-semantic-subtyping) — прочитаны объяснение неполноты синтаксического subtyping, нормализация, NP-hardness, кэширование и `Code Too Complex`. Это статья авторов об инженерном решении, не замер Typespec и не доказательство применимости всех правил к JVM.

**Исходники Luau — выборочные участки, не полный аудит:**

10. [ConstraintGenerator.cpp](https://github.com/luau-lang/luau/blob/c87565b8b0db5d5d5bfd2b8b00ad6573e5d06d9f/Analysis/src/ConstraintGenerator.cpp): передача `expectedTypes` при локальных bindings, создание `GeneralizationConstraint`, blocked results и зависимости. Файл загружен с `master` в день проверки; SHA ниже — HEAD, полученный API в ту же сессию, не отдельная побайтовая сверка файла с pinned URL.
11. [ConstraintSolver.cpp](https://github.com/luau-lang/luau/blob/c87565b8b0db5d5d5bfd2b8b00ad6573e5d06d9f/Analysis/src/ConstraintSolver.cpp): прочитан основной цикл `runSolverPass`, снятие блокировок, generalization после зависимостей, time/cancellation/constraint limits, incomplete error и finalize type functions. Та же оговорка о загрузке с `master`.
12. [Subtyping.cpp](https://github.com/luau-lang/luau/blob/c87565b8b0db5d5d5bfd2b8b00ad6573e5d06d9f/Analysis/src/Subtyping.cpp): выборочный просмотр `normalizationTooComplex`, normalizer, recursion limiter, отношения с free type bounds. Не заявляется аудит soundness. Та же оговорка о `master`.
13. [TypeFunctionRuntime.cpp](https://github.com/luau-lang/luau/blob/c87565b8b0db5d5d5bfd2b8b00ad6573e5d06d9f/Analysis/src/TypeFunctionRuntime.cpp): загружен именно по SHA; просмотрены `luaL_sandbox`, отдельные sandboxed threads/private globals, readonly и ошибка лимита при копировании типов.

**Локальный контекст:** README.md, docs/reference.md (язык типов, источники аннотаций, structural types, generics, immutable collections, optional row extensions); каталог features; подробно F220, F230, F270, F271, F311; разделы current/planned и примеры F310, F340, F295. Утверждения о текущем Typespec ниже взяты из карточек, а не из нового независимого аудита implementation.

### Что не проверялось

Luau и Typespec не собирались, примеры ниже не запускались, benchmark отсутствует. Из HTML-страниц часть интерактивных code snippets не попала в текстовую выгрузку: выводы по ним не реконструировались; использованы пояснения, статические RFC-примеры и исходники. Документ о поддержке generic function types в UDTF, дополнительные страницы refinements/considerations и RuntimeBuilder были скачаны, но не использованы как подробно изученные доказательства. Доклады и abstracts научных работ в этой сессии не изучались. Не подтверждён rollout конкретных solver flags у всех потребителей Luau/Roblox.

## 2. Состояние проекта

GitHub metadata получена **через `curl https://api.github.com`**, без поисковых сниппетов:

- Репозиторий: [luau-lang/luau](https://github.com/luau-lang/luau), MIT, `archived: false`.
- Проверка началась `2026-11-17T20:22:00Z`.
- Stars: **6121** — моментальный снимок, не мера качества type checker.
- `pushed_at`: `2026-11-17T19:16:46Z`; `updated_at`: `2026-11-17T17:27:02Z`.
- Default branch: `master`.
- Последний commit default branch: **`c87565b8b0db5d5d5bfd2b8b00ad6573e5d06d9f`**, `2026-11-13T21:01:47Z`, `Sync to upstream/release/747 (#3128)`.
- Предыдущие четыре: `f46d879e28` (11 ноября, CI fix), `12a6ba7588` (7 ноября, include), `91acc8decf` (6 ноября, release/746), `e3275c2097` (2 ноября, type error при iteration).
- Commit 747 содержит исправления stack overflow на unions/intersections, неверно разрешённой итерации и ICE в type functions. Это конкретное свидетельство продолжающейся работы над анализатором, не доказательство отсутствия других багов.

API: [metadata](https://api.github.com/repos/luau-lang/luau), [последние commits](https://api.github.com/repos/luau-lang/luau/commits?per_page=5). `pushed_at` не тождественен дате HEAD: push мог касаться другой ветки/тега. Пять commits не дают статистики contributor diversity, responsiveness или SLA. Luau — живой проект с регулярными обновлениями; Typespec не должен считать его безошибочным oracle.

## 3. Findings: таблицы и persistent maps

### 3.1 Shape inference полезен; модель изменяемой таблицы переносить нельзя

Luau различает:

- **unsealed table**: literal, форма накапливается при записи новых полей; документация называет его exact;
- **sealed table**: аннотация/выход из соответствующей области/возврат закрывают изменение формы; тип **inexact**, допускает дополнительные поля и width subtyping;
- **generic table**: требования к форме извлекаются из обращений к параметру.

Ключевая разница: «перестали расширять выводимый тип» и «в значении запрещены дополнительные ключи» — разные свойства. Typespec не должен использовать один флаг для openness и завершённости inference.

Для Clojure полезна передача формы через выражения, уже начатая в **F310**, с дальнейшим сохранением остатка через функцию в **F311**:

```clojure
(defn with-version [m] (assoc m :version 1))
(with-version {:id 7 :name "Ada" :version "old"})
;; Цель: сохранить :id/:name; :version заменить на int?.
```

Предлагаемый алгоритм: представить известные required/optional/absent поля и остаток; `assoc` строит **новый** тип результата, удаляет прежнее ограничение на записанный ключ и добавляет новое. `dissoc` удаляет гарантию поля. Вызов generic функции должен сохранять связь с остатком конкретного аргумента. Не выражать overwrite простым `T & {:version int?}`: при старом string это неверное пересечение.

**Цена:** средняя для локальных transfer rules, высокая для generic row preservation, unions, неизвестных ключей и alias-recursion. Luau generic tables не являются готовой реализацией Clojure row polymorphism. F311 уже описывает нужную развилку «generic всей мапы + operators или row variable»; новое предложение — проверить раздельность shape-state и openness, а не создать ещё одну карточку.

### 3.2 Отсутствие ключа не равно nil

В Lua присваивание nil убирает поле. Документация Luau использует это при optional properties. В Clojure различаются:

```clojure
(contains? {} :name)          ; false
(contains? {:name nil} :name)  ; true
```

Поэтому `optional<string>` нельзя реализовать как «поле присутствует со значением string|nil». При чтении `(get m :name)` оба случая могут дать nil, но факт `contains?` уточняет наличие, а не обязательно non-nil. Для `{:name nil}` значение после проверки наличия остаётся nil.

Это обязательные regression cases **F310/F240/F270**, а не новая feature. Особенно важно для `t/partial`: делает ключ optional, не расширяет declared value type до nil. Для открытой мапы исключение ключа из объявленной формы (`t/omit`) также не доказывает его физического отсутствия — это уже верно выделено F270.

### 3.3 Indexers и variance

Luau объединяет массивы и records одной table-моделью (`{T}` сокращает `{[number]: T}`). В Clojure vector, map, seq и tuple имеют разные операции и гарантии; integer-key map не следует объявлять vector. Общую идею «фиксированные поля + typed tail» можно использовать в дизайне reference/F310, но не объединять коллекции.

Mutable table pitfalls из RFC generics относятся скорее к **F340**: read/write контейнеры инвариантны. Persistent maps можно расширять безопаснее, но atom/Java mutable collections требуют отдельной модели. Не переносить инвариантность всех Luau tables на все Clojure maps.

## 4. Findings: generics и inference

### 4.1 Сначала корректный вызов и callback, затем сложный solver

Luau inference сохраняет связь параметра и результата, а не заменяет generic на `any`. Текущая документация описывает как implicit, так и explicit instantiation. Для Typespec важен не синтаксис `<<...>>`, а различие **жёсткого binder при проверке определения** и **свежей переменной при каждом вызове**:

```clojure
(defn same {:t/sig '(t/all [a] [a :-> a])} [x] x)
(same 1)
(same "x")
;; Независимые инстанциации, не общий a для двух вызовов.

(defn bad {:t/sig '(t/all [a] [a :-> a])} [x] "fixed")
;; Отклонить: тело должно работать для произвольного a.
```

Это **F230**, уже не новая идея. По карточке общий механизм пока `not-implemented`; отдельные специальные правила `map`/`mapv` не равны generics.

### 4.2 Expected types должны идти внутрь произвольного callback

В `ConstraintGenerator.cpp` наблюдаются `expectedTypes`, передаваемые в проверку значений, и отдельные generalization constraints. Это инженерное свидетельство двух направлений информации: от выражения вверх и от контекста внутрь.

```clojure
(defn use-text
  {:t/sig '[(t/fn [string? :-> number?]) :-> number?]}
  [f] (f "hello"))
(use-text (fn [s] (count s))) ; цель: вывести s как string?
(use-text (fn [s] (inc s)))   ; цель: ошибка внутри inc
(use-text (fn [s] s))        ; цель: ошибка результата callback
```

Для **F220/F230** предлагается малый алгоритм, не переписывание checker:

1. Получить сигнатуру вызываемой функции и свежие переменные generics.
2. Проверить доступные обычные аргументы, получить ограничения для типов параметров callback.
3. Для inline `fn` связать параметры с ожидаемыми типами; проверить тело; сопоставить результат с ожидаемым return type.
4. Сохранить ограничения, если часть ожидания пока неизвестна; не фиксировать окончательный `unknown` слишком рано.
5. Проверить bounds и арности; сохранить provenance диагностик.

По F220 сейчас контекст передают специальные HOF-правила, не любой `call-fn`; по F230 совместимость `:fn` ещё не сравнивает полноценные сигнатуры. Исправить это важнее глобального HM inference. Цена — средняя/высокая: sequencing, overload candidates и отказ от общего кеша mutable inference variables.

### 4.3 Polarity — причина не спешить с higher-rank generics

RFC показывает ошибку наивного правила «если generic совместим с инстанциацией, разрешить это всюду». Если ожидается функция, работающая для **любого** `a`, нельзя передать функцию только над string. В mutable поле также нельзя незаметно заменить полиморфное значение мономорфным и сохранить старое обещание другим читателям.

Для F230: MVP — rank-1, явное различие проверки/инстанциации, контравариантность параметров и ковариантность результата. Higher-rank и polymorphic mutable fields отложить. Добавить негативный тест «string-only callback не удовлетворяет universally quantified callback», когда такой синтаксис действительно будет принят. Не обещать его поддержку сейчас и не переносить RFC-правило без контекста variance.

### 4.4 Constraint dependency graph — полезная следующая ступень

Прочитанный цикл Luau решает готовые constraints, откладывает зависящие от unresolved типов, после успешного шага снимает блокировки и generalizes освобождённые переменные. Есть bounded work, cancellation, forced pass при остановке прогресса и сообщение incomplete при остатке.

Для **F220/F230/F270** стоит заимствовать меньшую архитектуру:

- явный результат `resolved / blocked(dependencies) / error`;
- worklist, возобновляющий constraint после уточнения нужного типа;
- generalization только после решения релевантных зависимостей;
- отдельное `inconclusive` при лимите или неподдержанном цикле, не «успех через any»;
- источник ограничения: аргумент, return, spec, body — чтобы показывать одну причину ошибки.

Это **предложение для Typespec**, не утверждение, что Luau выдаёт ровно такие публичные статусы. Полный Luau solver намного сложнее этого эскиза; forced pass и эвристики нельзя копировать без понимания инвариантов. Цена высокая; вводить только после простого contextual checking и измерения конкретных тупиков вывода.

## 5. Findings: type functions

### 5.1 Реально реализованы, но это выполнение кода

Luau type functions принимают типы, программно строят другие типы и работают **во время анализа**. Официальная страница показывает задачу `keyof`; RFC описывает изменение полей, доступ к read/write types и type objects. Исходники содержат выделенный sandboxed runtime. Это не просто синтаксический sugar над type aliases и не только roadmap.

Соблазн для Clojure — написать обычную функцию над map-представлением типа. Но запуск `eval`, `require` или произвольных macros при анализе открывает I/O, effects, зависимость от среды и зависание. «Нет runtime checks» само по себе не запрещает compile-time computation; здесь отказ от unrestricted evaluation обусловлен также отдельным обещанием Typespec об инертных аннотациях.

### 5.2 Взять идею reducer, не VM

**F270** уже содержит `pick/omit/partial/keys-of/get`, **F271** — будущие mapped types. Нужен внутренний нормализатор с фиксированным набором операций:

1. Прочитать quoted форму как данные, разрешить aliases, не исполнять Clojure.
2. На конкретной конечной map shape вычислить оператор с сохранением optional/required.
3. На unresolved generic сохранить узел операции с зависимостями, не свести к `unknown`.
4. Ограничить число reduction steps/размер результата; диагностировать неразрешимость отдельно.
5. Кешировать лишь относительно стабильных входных типов и среды; не использовать устаревший результат после уточнения mutable inference variable.

```clojure
;; Тип исходной записи имеет обязательный :title string?.
;; (t/partial Record) допускает {}, но не {:title nil}.
;; (t/get Record :title) даёт string?.
;; Если :title optional, чтение даёт string?|nil.
```

Сначала конечные карты F270. Произвольный обход ключей, recursive type computation и key remapping F271 — только при задаче, не решаемой этим набором. Цена F270 умеренная, общий безопасный type-function язык — высокая: evaluator, бюджеты, reproducibility, security, диагностика и кэш invalidation. Lua sandbox не является готовой JVM sandbox для Clojure.

## 6. Findings: semantic subtyping и пределы сложности

Статья Luau разбирает простой контрпример синтаксическому алгоритму:

```text
(number | nil) & (string | nil) = nil
```

Проверка только формальных ветвей union/intersection может не доказать направление слева направо. Luau нормализует типы, группируя компоненты по категориям значений. Статья прямо признаёт NP-hardness общей задачи и использует cache/`Code Too Complex`; актуальный Subtyping.cpp также содержит normalizer и отдельный признак `normalizationTooComplex`.

Для Typespec это мотивирует узкую нормализацию в **F210/F240/F270/F295**, не проект «полный semantic subtyping»:

```clojure
;; Семантический тест для языка типов, не runtime s/and:
(t/and (s/nilable number?) (s/nilable string?))
;; При заявленной непересекаемости number? и string? эквивалентно nil?.
```

Минимум: flatten/deduplicate unions, убрать never, обработать известные disjoint primitive категории и nil, сохранить канонический порядок, ограничить распределение intersections по unions. Для recursive graphs нужны отдельные корректные правила пар и циклов; лимит глубины не должен возвращать true. Эта проблема уже выделена F295 — Luau добавляет пример архитектуры и источники, а не новый issue.

Цена растёт резко при добавлении функций, рекурсивных maps, Java interfaces и arbitrary predicates. JVM-классы/interfaces не образуют простую непересекающуюся классификацию Lua values; неизвестное пересечение нельзя объявлять пустым по отсутствию доказательства. Не переносить NP-hardness конкретного языка статьи как точную оценку ограниченного MVP Typespec.

## 7. Предложения и приоритеты — только в существующие карточки

| Приоритет | Карточки | Конкретное уточнение/работа | Проверка готовности | Цена |
|---|---|---|---|---|
| P0 | F220, F230 | Проверять произвольный inline callback против expected signature; сначала корректный function subtyping, свежие generic instances, rigid binders | Три `use-text` выше; два вызова identity разных типов; неверное generic тело отклонено | Средняя/высокая |
| P0 | F310, F240, F270 | Развести openness, известное отсутствие, optional, nilable; не копировать Luau nil-property equivalence | `{}` против `{:k nil}`, `contains?`, optional read, partial без нового nil | Средняя; фундаментальная семантика |
| P0 | F295, F210 | Лимит не означает доказательство; добавить complexity/inconclusive outcome, не успешный unknown | Глубокое несовпадение не принято; малый budget даёт incomplete, больший — содержательный результат | Средняя/высокая |
| P1 | F270 | Закрытый reducer type operators, затем отложенное вычисление generic-зависимых узлов | Finite-map pick/omit/partial; alias; blocked input; bounded evaluation | Средняя |
| P1 | F311, F310 | Сохранение остатка мапы через assoc/dissoc; overwrite не intersection | `with-version` сохраняет :id/:name и заменяет прежний string :version | Высокая для общего случая |
| P1 | F220, F230 | Малый worklist только там, где sequential contextual checking недостаточен | Изменение порядка независимых ограничений не меняет вывод; нет преждевременной generalization | Высокая, вводить по evidence |
| P2 | F210, F240, F295 | Ограниченная нормализация unions/intersections с бюджетом | nil/intersection пример, перестановка ветвей, рост размера и рекурсивные случаи | Высокая вне primitive subset |
| P2 / отложить | F271, F230, F340 | Пользовательский type-level язык и higher-rank — не MVP; сохранить негативные сценарии variance | Реальный кейс, который не решается F270/rank-1, прежде новой грамматики | Очень высокая |

Эти приоритеты — рекомендация исследования, не изменение статуса карточек. По прочитанным карточкам F220 и F310 частичны, общие F230/F270 не реализованы, F271/F311 остаются предложениями. Наличие подробного дизайна не превращает примеры в работающие golden tests.

## 8. Что не брать

- Три состояния Luau table как готовую семантику persistent Clojure map; особенно не путать sealed с exact.
- Равенство absent и nil, одно представление map/vector/seq и Lua metatable/OOP machinery.
- Nonstrict `any` как средство устранить тупик inference; Typespec должен показывать пробел проверки и trust budget.
- Unrestricted Clojure type functions, `eval` аннотаций или выполнение пользовательских macros ради типов.
- Higher-rank instantiation до правильных rank-1 функций, polarity и mutation rules.
- Полный C++ solver, semantic subtyping всех JVM типов или type packs Lua multiple returns как ближайший milestone. У Clojure один возвращаемый value; apply/variadic арности имеют отдельную F410 и не равны Lua packs.
- Использование Luau как готового backend checker существующего Clojure: различия reader/macros, JVM interop, коллекций и specs делают это отдельным крупным проектом.

**Итог:** Luau полезен как проверяемый источник инженерных правил и отрицательных примеров. Ближайший выигрыш Typespec — корректная проверка HOF и generics на существующем Clojure, точная семантика maps/nil и честная остановка при нехватке информации. Type-level программирование и большой solver — последующие инвестиции, а не необходимые условия этого выигрыша.
