# Elixir: что перенести в Typespec

Проверено **17 ноября 2026, 20:22 UTC**. Цель — статическая проверка существующего Clojure, **без добавления runtime checks**. Это исследование для Typespec, не проект интеграции с Aidbox. Изменён только этот документ; карточки, исходники и index не менялись.

## Краткий вывод

У Elixir стоит взять не новый синтаксис аннотаций, а три механизма: **отсутствие ключа как отдельный факт**, **сужение связанных значений через граф зависимостей**, **алгебру пересечения/разности типов**. Их можно вводить постепенно внутри уже существующих F310/F311, F240/F250/F251 и F210. Отдельно полезны strong arrows — проверяемые свойства существующего кода, позволяющие иногда сохранять точный результат на нетипизированной границе без вставки проверок.

Однако основной режим Elixir 1.20 ищет заведомые ошибки, а не запрещает все потенциально небезопасные вызовы. Перенести его compatibility rule в строгий Typespec было бы ослаблением проверки. Не нужно также копировать весь set-theoretic solver или обещать готовые row polymorphism и generics на основании Elixir: это разные задачи, часть из них у самого Elixir остаётся исследованием.

## 1. Источники и состояние проекта

### Метаданные GitHub

Получены непосредственно через `curl https://api.github.com/...`, без логинов:

| Показатель | Результат на дату проверки |
|---|---|
| Репозиторий | [elixir-lang/elixir](https://github.com/elixir-lang/elixir) |
| Stars | 26 727 |
| Default branch | `main` |
| Последний commit в default branch | `9b1dfe38365ec795ca12e5cc9882c162d8370ec6`, 2026-11-17 15:34:28 UTC |
| Сообщение | `Do not assign macro expansion counters to caller-supplied aliases (#16140)` |
| `pushed_at` | 2026-11-17 15:34:40 UTC |
| Последний опубликованный release по API | [v1.20.5](https://github.com/elixir-lang/elixir/releases/tag/v1.20.5), 2026-10-29 19:51:12 UTC |

API: [repository](https://api.github.com/repos/elixir-lang/elixir), [commits?per_page=1](https://api.github.com/repos/elixir-lang/elixir/commits?per_page=1), [releases/latest](https://api.github.com/repos/elixir-lang/elixir/releases/latest). Stars относятся ко всему языку, а последний commit — не к type checker. Эти числа подтверждают живой репозиторий, но не скорость работы над отдельными механизмами. Commit frequency и число активных разработчиков checker не измерялись. Исходники ниже закреплены на **v1.20.5**, не на движущемся main.

### Реально прочитано

- **[S1] [Gradual set-theoretic types, документация 1.20.5](https://hexdocs.pm/elixir/1.20.5/gradual-set-theoretic-types.html)**: разделы maps, domain/mixed keys, функции, dynamic, inference, false positives, roadmap.
- **[S2] [Elixir v1.20 released: now a gradually typed language, 2026-06-03](https://elixir-lang.org/blog/2026/06/03/elixir-v1-20-0-released/)**: compatibility, narrowing, guards, ограничения и следующие milestones. Производительность/benchmark — заявления авторов, здесь не воспроизводились.
- **[S3] [Strong arrows: a new approach to gradual typing, 2023-09-20](https://elixir-lang.org/blog/2023/09/20/strong-arrows-gradual-typing/)**: мотивация, dynamic/static boundary, исходное и уточнённое определение strong arrows, ограничения полиморфизма. Это техническая статья, а не актуальная спецификация реализованного синтаксиса.
- **[S4] [Module.Types.Descr](https://github.com/elixir-lang/elixir/blob/v1.20.5/lib/elixir/lib/module/types/descr.ex)**: выборочно представление типов, bitmap/BDD/DNF, constructors, optional/not_set, upper/lower bounds. Не прочитан целиком и не доказана корректность solver.
- **[S5] [Module.Types.Pattern](https://github.com/elixir-lang/elixir/blob/v1.20.5/lib/elixir/lib/module/types/pattern.ex)**: описание и начало `of_head`, обработка предыдущих clauses и зависимостей patterns/guards.
- **[S6] [Module.Types](https://github.com/elixir-lang/elixir/blob/v1.20.5/lib/elixir/lib/module/types.ex)**: начало inference, переключение infer/traverse, cache/protocol ограничения, сбор inferred signatures.
- **[S7] [Module.Types.Apply](https://github.com/elixir-lang/elixir/blob/v1.20.5/lib/elixir/lib/module/types/apply.ex)**: strong arrows стандартной библиотеки, варианты signature и dispatch к `apply_strong`. Это подтверждает наличие механизма, но не автоматическое доказательство strongness всех пользовательских функций.

Прочитан контекст Typespec: README, начало и доступные разделы reference; карточки F210, F220, F240, F250, F251, F310, F311 — целевые разделы current/planned, алгоритмы и примеры. Их описания текущей реализации не проверялись повторным запуском прототипа. Поиск `Elixir`, `strong arrow`, `not_set`, `BDD`, `set-theoretic` по features на момент проверки совпадений не дал; сами функциональные требования уже есть в карточках, поэтому новых карточек не предлагаю.

Доклад и paper **The Design Principles of the Elixir Type System**, на которые ссылается S1, не изучены; выводы не выдаются за результат чтения доказательств или видео. Файлы Expr/Of и release 1.19 загружены для навигации, но не являются основаниями самостоятельных выводов. Локальных экспериментов с компилятором не было.

### Реализованное и roadmap

**Реализовано в 1.20.5:** set-theoretic внутреннее представление; open/closed maps, optional и forbidden keys; narrowing по guards; inference по языковым конструкциям; best-effort gradual checking без пользовательских аннотаций. Наличие кода проверено выборочно, полнота всех конструкций — заявление документации и release announcement.

**Не следует считать выпущенным:** новый пользовательский синтаксис set-theoretic signatures, typed structs как следующий milestone, полноценные parametric/recursive types. S2 прямо перечисляет эффективную реализацию recursive types, parametric types и обхода key/value pairs мап как условия дальнейшего движения. Примеры `$ a -> a` из статьи 2023 года — иллюстрация будущего языка, не инструкция для текущего Elixir. Старые Erlang `@spec` нельзя считать эквивалентом новых проверяемых контрактов; S1 планирует их последующее вынесение в отдельную библиотеку.

## 2. Findings

### 2.1. Presence — не nullable: три состояния поля

**Факт [S1,S4].** `%{..., age: not_set()}` запрещает ключ `:age`, даже при открытом остатке. `if_set(integer())` допускает отсутствие, но ограничивает присутствующее значение. Closed map перечисляет все разрешённые поля; open map допускает другие. Это разные оси: открытость объекта, наличие ключа, тип значения.

**Для Typespec:** уже предусмотрено F310/F311. Уточнение реализации — поле с presence `required | optional | absent` и value type, плюс open/closed/typed-tail. Отсутствие не кодировать Clojure `nil` и не терять удаление ключа в открытом хвосте.

```clojure
(let [m {:id 1 :name nil}]
  [(get m :name "unknown")          ; nil
   (get (dissoc m :name) :name "unknown")]) ; "unknown"
```

`dissoc` должен записывать known-absent, `assoc` удалять прежние presence-факты и заменять тип, а не пересекать его. Это особенно важно для `(assoc {:version "old"} :version 1)`: результат не bottom.

Для `merge` required слева + optional справа означает required результата с объединением возможных значений; тип левого значения остаётся возможен при отсутствии правого ключа. В F310 этот участок уже назван ограничением current — это работа по существующей карточке, не новая feature.

**Цена:** средняя: type representation, subtyping, lookup/defaults, joins, map transforms, отображение диагностик. Полный BDD не нужен для первого этапа. Нельзя объявить `dissoc` row-polymorphic только потому, что он умеет запретить ключ: сохранение неизвестного остатка между входом/выходом — отдельная F311.

### 2.2. Разность типов для else и последующих веток

**Факт [S1,S4,S5].** Типы — множества значений; union/intersection/negation имеют семантический смысл. Реализация разделяет непересекающиеся семейства значений, использует bitmap для простых семейств, специальные структуры для остальных и BDD/DNF. Pattern checker вычитает уже покрытые clauses из следующих.

**Предложение для F210/F240:** сначала ограниченная внутренняя операция `subtract(T, P)` для literals, nil/false, простых predicates и tagged-map unions. Не вводить публичный синтаксис произвольного `not` в типах до работающей семантики. Проверки `A <: B` можно понимать как пустоту `A ∩ ¬B`, но это спецификация результата, не требование сразу реализовать полный solver.

```clojure
;; e имеет union {:kind :ok, :value string?} | {:kind :error, :code int?}
(if (= :ok (:kind e))
  (clojure.string/upper-case (:value e))
  (inc (:code e)))
```

Нужно сужать **e**, а не только результат `(:kind e)`. После первой ветки из возможных вариантов вычитается `:ok`; остаток определяет else и exhaustiveness.

Для пользовательского predicate `false` не означает `not T`, если доказано лишь `truthy => T`. Это уже граница F250. Вычитать можно только гарантированно покрытый домен. Для неподдержанного условия сохранить исходный тип и отметить неполноту, а не объявить ветку unreachable.

**Цена:** средняя для конечных unions, высокая для произвольных вложенных отрицаний и функций. Ограничить размер unions и число шагов; fallback должен ослаблять точность явно, а не превращать неизвестное в доказанную совместимость. BDD полезен как будущая оптимизация после профилирования, не обязательный фундамент прототипа.

### 2.3. Guards как зависимости между binding и путём

**Факт [S5].** Комментарий `of_head` описывает четыре шага: построить pattern tree и зависимости переменных; пересечь patterns с ожидаемыми типами; обработать guards и новые зависимости; распространить зависимости для refinement. На шагах учитываются предыдущие clauses, при ошибке ищется первопричина.

**Перенос в F240/F251:** subject — ID immutable binding/snapshot, path — literal ключ/индекс. Из деструктуризации создаётся связь локального binding с путём исходного объекта. Refinement передаётся в обе стороны с консервативным join на выходе веток.

```clojure
(let [{:keys [name] :as person} input]
  (when (string? name)
    (clojure.string/upper-case (:name person))))
```

Это предлагаемый acceptance-test, не проверенный результат Typespec. Если есть `:or`, связь уже не identity: local может быть default при отсутствующем ключе. Нельзя из проверки local вывести, что поле исходной мапы существует.

В Clojure нет отдельного языка guards с теми же гарантиями, что в Elixir. Произвольный predicate может иметь эффекты; `contains?` работает не только с maps. Сначала нужна проверка `map?`, затем presence refinement. `@state` при повторном чтении — новый snapshot. Guard summaries остаются verified/trusted по F250 и F251, а не автоматически доверяются из-за boolean-return.

**Цена:** средняя/высокая: binding identities, зависимости, worklist propagation, provenance, ограничения размера графа и инвалидация. Не переносить иммутабельность BEAM на Java mutable fields.

### 2.4. Dynamic compatibility — полезный lint, но не строгая проверка

**Факт [S1,S2].** Для статического union аргумент должен целиком подходить функции. Для `dynamic(A | B)` Elixir допускает вызов, если совместима хотя бы часть диапазона; выдаёт нарушение, когда accepted/supplied disjoint. Dynamic можно сужать, это не просто «выключить checker». Внутреннее представление имеет upper/lower bounds. Dynamic распространяется к корню контейнера: `{:ok, dynamic()}` превращается в `dynamic({:ok, term()})`.

```clojure
(let [x (if flag 1 "bad")]
  (inc x))
```

Для **строгого Typespec** это ошибка: строковая ветка возможна. Модель Elixir для gradual union может промолчать, потому что числовая ветка совместима. «Не найден verified bug» не означает «все выполнения типобезопасны».

**Рекомендация F210/F810/F840:** не отождествлять `unknown`, `any` и Elixir dynamic. Unknown требует сужения; any — явный расход trust budget. Если нужен режим миграции, добавить отдельно labelled «definite-error lint» с диапазоном известных возможностей и собственной метрикой coverage. Не менять строгий subtype на compatibility; пониженная уверенность должна попадать в JSON, а не скрываться.

S1 документирует false-positive исключения, включая предположение о хотя бы одной итерации comprehension и требования к struct update. Поэтому обещание S2 про verified bugs не следует пересказывать как абсолютную гарантию отсутствия ложных срабатываний.

**Цена:** высокая для второго полноценного режима; низкая для явного разделения понятий в API/диагностике. В ближайшем плане нужен второй вариант, а не новый gradual checker.

### 2.5. Strong arrows: результат без новых runtime checks

**Факт [S3,S7].** Первоначальное объяснение: вне объявленного domain функция падает. Уточнение статьи: вне domain функция либо не возвращает нормально, либо всё равно возвращает значение нужного codomain. Для `A -> B` достаточно доказать, что анализ тела на `¬A` не даёт нормального результата вне B. В реализации есть strong signatures стандартной библиотеки и отдельное применение; это не доказательство, что любой пользовательский `@spec` получает такое свойство автоматически.

```clojure
(defn annotated-id [x] x) ; декларация number? -> number? сама по себе не защита
(defn plus-one [x] (inc x)) ; существующий inc уже ограничивает normal return
(defn constant-ok [_] :ok) ; любой normal return :ok, без проверки аргумента
```

Для `annotated-id`, вызванного из неизвестного кода, числовой результат нельзя обосновать одной аннотацией. Для `constant-ok` результат обосновывается телом. Для `plus-one` надо точно знать контракт **существующего** `inc`, его числовой домен, поведение normal return и исключений; приведённый комментарий — направление анализа, не доказанная здесь strong signature Clojure.

**Предложение F250/F251/F710/F840:** отдельное поле summary `normal-return guarantee`, не смешанное с predicate refinement. Начать с небольшой проверенной модели core-функций и тел без динамической диспетчеризации. При невозможности доказательства возвращать неизвестный/ослабленный тип на нетипизированной границе; не вставлять `assert` и не просить агента добавлять guard для удовлетворения checker.

Это не делает небезопасный вызов безопасным: функция может выбросить исключение. Strong arrow сохраняет тип **нормального результата**, а не гарантирует totality. Для `catch`, который возвращает произвольное значение, гарантию нужно пересчитывать. `:pre` зависит от компиляции assertions; type hints и `s/fdef` не доказывают защиту на runtime; redef Var и extern-контракты требуют явно зафиксированного снимка/границы доверия.

**Цена:** высокая для общего анализа дополнения domain и эффектов; средняя для фиксированных extern summaries. Приоритет ниже исправления map semantics. Generic/overloaded Java и протоколы не включать в первый этап.

### 2.6. Ограниченный inference вместо глобального solver

**Факт [S1,S6].** Документация описывает двухэтапную стратегию: inference с текущим модулем, стандартной библиотекой и dependencies; межмодульные вызовы внутри того же проекта при inference считаются dynamic; затем проверка проекта с собранными типами. Это сознательная граница точности ради скорости/инкрементальности. Исходник подтверждает раздельный inference entry point, сбор signatures и зависимость режима от cache; без cache и для protocols есть ограничения.

**Для F220/F720:** сохранить локальную/контекстную inference и явные публичные сигнатуры Typespec; использовать summaries и fingerprints зависимостей. Не обещать универсальную реконструкцию всех межnamespace типов. Принцип «stable summaries прежде whole-program inference» полезен, но точное правило Elixir про dynamic между модулями не требуется копировать: Typespec уже рассчитан на аннотации от агента.

**Цена:** средняя для кеширования проверенных summaries, высокая для полного межпроцедурного inference. Нужны cold/warm equivalence tests, а не только ускорение на удачном проекте.

## 3. Предложения по приоритетам — без новых карточек

| Приоритет | Существующие карточки | Конкретная работа | Проверка результата |
|---|---|---|---|
| **P0** | F310, F311, F320 | Required/optional/known-absent; корректные `get`/default, `assoc`, `dissoc`, optional-overwrite в `merge` | Nil ≠ missing; удалённый ключ не возвращается из open tail; default входит в результат |
| **P0** | F210, F810, F840 | Зафиксировать strict subtype отдельно от compatibility и opt-out | Union с плохой альтернативой отклоняется; any/unknown не скрывают отсутствие доказательства |
| **P1** | F240, F250, F251 | Ограниченная difference и граф subject/path refinements | Tagged-map else; nested destructuring; `:or`; однонаправленный guard; повторный deref |
| **P1** | F220, F720 | Summary-first inference и dependencies fingerprint | Холодная и тёплая проверки одного снимка совпадают, смена контракта инвалидирует пользователей |
| **P2** | F250, F251, F710, F840 | Исследовательский прототип normal-return guarantees на core externs | Identity не получает сертификат из аннотации; constant-return получает из тела; catch учитывается |
| **P3** | F210, F240, F295 | Оценить BDD/semantic subtyping только после замеров | Нагрузка с unions/negations, контролируемое расширение и честный fallback |

P0/P1 — в основном реализация уже принятых требований, не новая заявка. Presence и row polymorphism не объединять в одно обещание; strong-return и checked-predicate не считать одним сертификатом. Примеры выше — предлагаемые регрессии, не прошедшие тесты прототипа.

## 4. Что не брать

1. **Не заменять строгую проверку поиском только заведомых ошибок.** Полезный migration lint не должен снижать основной барьер CI.
2. **Не копировать новый синтаксис типов Elixir** и не полагаться на `@spec` как уже реализованный set-theoretic контракт. Typespec сохраняет spec-style data и существующий Clojure.
3. **Не ставить полный BDD solver первой задачей.** Сначала presence, конечные unions, пути и понятные диагностики; затем измерения.
4. **Не выдавать map domain keys за row polymorphism.** Elixir сам ограничивает точность domain keys верхними типами; это полезная граница, но не готовая унификация остатка Clojure maps.
5. **Не переносить BEAM semantics буквально.** Elixir map dot access требует ключ; Clojure keyword lookup может вернуть nil. Clojure `contains?`, коллекции, числовая и Java-модель отличаются; отдельный набор Clojure transfer functions обязателен.
6. **Не добавлять проверки ради soundness.** Разрешено учитывать уже существующие predicates/операции; аннотация и проверка Typespec не должны менять поведение программы.
7. **Не заявлять универсальный soundness из прочтения статьи.** Открыты mutable Java, dynamic vars, unknown macros, исключения, polymorphic collections и неточные externs. Сроки разработки здесь не оценивались; low/medium/high означают архитектурную сложность, не календарный прогноз.
