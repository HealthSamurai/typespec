# Typed Racket: что перенести в Typespec

Дата проверки: **2026-11-17 UTC**. Цель — статическая проверка существующего Clojure без добавления runtime checks; не интеграция с Aidbox. Изменён только этот документ, карточки и код не менялись.

## Summary

Полезнее всего взять не синтаксис Typed Racket, а его **логическую модель результата выражения**: тип, положительное и отрицательное утверждения, объект/путь, к которому они относятся. Она соединяет сужение веток, проверяемые summaries предикатов и передачу фактов через функции. В Typespec это развитие **F240 + F250 + F251**, а не новая отдельная feature.

Для исполнения ориентир — **Typed Racket Optional**, а не стандартный Deep и не Shallow: Optional сохраняет статическую проверку, но не обеспечивает динамическую защиту на границе с нетипизированным кодом. `no-check` — совершенно другой режим: он отключает саму статическую проверку. Никаких контрактных обёрток, вставленных assert или instrumentation Typespec не требуется.

## Источники и глубина проверки

Все ссылки проверялись прямым HTTP, без платных сервисов. Документация прочитана в исходных Scribble-файлах текущего репозитория; это первоисточник генерируемого руководства, а не пересказ.

1. [Occurrence Typing, Guide](https://docs.racket-lang.org/ts-guide/occurrence-typing.html). Прочитаны разделы Basic Occurrence Typing, Propositions and Predicates, One-sided Propositions, Other conditionals and assertions, caveat about `set!`. Исходник: `typed-racket-doc/typed-racket/scribblings/guide/occurrence.scrbl`.
2. [Deep, Shallow, and Optional Semantics](https://docs.racket-lang.org/ts-reference/behavior-of-types.html). Прочитаны определения трёх режимов и примеры импорта списков/изменяемых векторов с неверными декларациями. Исходник `reference/behavior-of-types.scrbl`.
3. [Typed-Untyped Interaction](https://docs.racket-lang.org/ts-guide/typed-untyped-interaction.html). Прочитаны разделы импорта через `require/typed`, opaque types, экспорта в untyped и начало объяснения защиты границ. Не заявляется прочтение всех случаев взаимодействия режимов.
4. [Typed Racket Syntax Without Type Checking](https://docs.racket-lang.org/ts-reference/no-check.html): прочитан весь небольшой раздел.
5. Tobin-Hochstadt, Felleisen, **Logical Types for Untyped Languages**, ICFP 2010: [полный PDF](https://www2.ccs.neu.edu/racket/pubs/icfp10-thf.pdf). Прочитаны выбранные фрагменты §2–3 про составные предикаты, selectors, propositions/objects и substitution при вызове. Это **не только abstract**, но и **не аудит полного доказательства soundness**. Статья объясняет базовую конструкцию; она не доказывает корректность переноса на Clojure/spec.
6. Исходники Typed Racket, закреплённые на `7fe7e11fae2b04e04525d63b01b33efe5ffad075`:
   - [tc-if.rkt](https://github.com/racket/typed-racket/blob/7fe7e11fae2b04e04525d63b01b33efe5ffad075/typed-racket-lib/typed-racket/typecheck/tc-if.rkt) — прочитан целиком: извлечение `PropSet p+ p-`, две ветки расширенного окружения, unreachable, merge результатов.
   - [prop-rep.rkt](https://github.com/racket/typed-racket/blob/7fe7e11fae2b04e04525d63b01b33efe5ffad075/typed-racket-lib/typed-racket/rep/prop-rep.rkt) — прочитаны определения `TypeProp`, `NotTypeProp`, нормализация пустого объекта/top/bottom и intern-таблицы. Наличие линейных неравенств видно, но их алгоритм не исследован.
   - [tc-lambda-unit.rkt](https://github.com/racket/typed-racket/blob/7fe7e11fae2b04e04525d63b01b33efe5ffad075/typed-racket-lib/typed-racket/typecheck/tc-lambda-unit.rkt) — выборочно прочитан путь построения `Arrow`: проверка тела в окружении параметров, `abstract-results` по именам аргументов. Полный inference функций и все его ограничения не проверены.

Локальный контекст: прочитаны README, относящиеся к задаче части `docs/reference.md`, каталог features и карточки F240, F250, F251; описания текущих ограничений F220 использованы для `filter`. Статусы текущего Typespec ниже взяты из этих карточек, **не из запуска checker**. Примеры в этом документе — проектные acceptance cases; ни Racket, ни Clojure здесь не запускались. Скачивание файла не считается его полным прочтением.

## Состояние проекта

GitHub API, вызванный через `curl api.github.com`:

- [`GET /repos/racket/typed-racket`](https://api.github.com/repos/racket/typed-racket): **578 stars**, `archived: false`, default branch `master`, `open_issues_count: 347`.
- `pushed_at`: **2026-11-13T18:59:07Z**; `updated_at`: **2026-11-11T14:32:39Z**.
- [`GET /repos/racket/typed-racket/commits?per_page=1`](https://api.github.com/repos/racket/typed-racket/commits?per_page=1): верхний commit default branch **7fe7e11fae2b04e04525d63b01b33efe5ffad075**, committer date **2026-11-12T22:09:06Z**. Сообщение: revert изменения типа `thread-wait`, сломавшего пакет `digimon`.
- Время получения: **2026-11-17T20:22:00Z**.

Это живой поддерживаемый проект с недавним изменением и реальной заботой о совместимости. Но одна запись не измеряет частоту разработки, число активных maintainers или скорость закрытия ошибок. `pushed_at` не равен дате последнего commit default branch; `updated_at` — метаданные, а не метрика разработки. `open_issues_count` GitHub включает pull requests. Stars — снимок популярности, не свидетельство качества. Точный release/version Racket и CI status не проверялись.

Optional, Shallow, occurrence typing и односторонние propositions документированы как **существующие возможности**, не roadmap. Последний revert полезен как напоминание: изменение библиотечного типа может сломать клиентов даже без изменения их runtime-кода.

## Findings

### 1. Проверка ветки должна получать факты от выражения, а не узнавать только текст предиката

В `tc-if.rkt` условие уже имеет результат с `PropSet p+ p-`; `if` не перечисляет имена `string?`, `number?` и пользовательских функций. Истинная ветка проверяется с p+, ложная с p−. Это отделяет производство факта от его использования.

Полезная внутренняя модель для Typespec (проект, не новый публичный синтаксис):

```text
Result = {type, truthy-prop, falsy-prop, object}
Object = BindingId + immutable-selector-path | none
Prop   = Top | Bottom | Is(Object, Type) | Not(Object, Type)
       | And(Prop...) | Or(Prop...)
```

В реализации Typed Racket `TypeProp`/`NotTypeProp` с пустым object сводятся к тривиальному утверждению: нельзя сужать неидентифицированное повторное вычисление. Это хорошая безопасная граница и для Typespec.

Для Clojure обязательна адаптация: **falsy = nil или false**, а не только Racket `#f`. `(some? false)` истинно. Поэтому `some?`, `boolean?`, обычный `if x` и `true?` не взаимозаменяемы.

```clojure
(defn use-flag [x]
  (if (some? x)
    x ; исключён nil, но НЕ false
    nil))
```

F240 сейчас описывает частичные положительные сужения и неполную truthiness-модель. Приоритет — точное пересечение/вычитание известных union-членов и обработка `nil/false`, а не сложная логика поверх замены исходного типа.

### 2. Summary предиката — два независимых доказательства

Guide явно показывает `(-> Any Boolean : String)` и положительное-only `#:+ Symbol`. Второй случай не позволяет исключать Symbol в false-ветке: предикат отвергает некоторые символы. Отрицательное-only утверждение тоже поддерживается.

Для Typespec:

```clojure
(defn nonempty-string? [x]
  (and (string? x) (not (empty? x))))

(defn f [x]
  (if (nonempty-string? x)
    (clojure.string/upper-case x) ; truthy => string
    x))                         ; falsy НЕ => not-string
```

Предлагаемый алгоритм F250:

1. Типизировать тело с известными аргументами и Result-моделью.
2. Собрать факты на truthy/falsy выходах с учётом короткого замыкания.
3. Удалить факты о локальных объектах, не выразимых через параметры/разрешённые пути.
4. Абстрагировать BindingId параметров в позиции аргументов; сохранить p+ и p− отдельно.
5. Если запрошено объявленное постусловие, доказать implication из полученного summary; не повышать trusted декларацию до verified.
6. В месте вызова подставить objects фактических аргументов. Если object отсутствует, не выдумывать identity.

Шаги 1–4 согласуются с прочитанными `tc-if`, `abstract-results` и объяснением latent propositions в статье; точный алгоритм implication и формат сертификата **предлагаются для Typespec**, а не скопированы из полностью изученной реализации.

Важное ограничение: Guide показывает, что явно объявленный тип `(-> (Listof Any) Boolean)` не сохраняет требуемое proposition для `listof-string?`, а расширенная сигнатура сохраняет. Нельзя утверждать, что Typed Racket автоматически экспортирует любые свойства любого предиката. В Typespec summary стоит хранить отдельно от обычного возвращаемого `boolean?`, чтобы редактор сигнатур случайно не стирал проверенное уточнение.

### 3. Objects/paths связывают проверки селектора и владельца значения

Статья рассматривает `(number? (car p))`, уточнение пары и функцию-предикат над её компонентом. Для Clojure полезный аналог — стабильный путь по immutable map:

```clojure
(defn render [event]
  (if (= :text (:kind event))
    (clojure.string/upper-case (:text event))
    "other"))
```

Предусловие примера: `event` имеет закрытый union maps с корреляцией `:kind :text` → обязательный строковый `:text`. Одной декларации `:kind` как union литералов недостаточно. Предложение: записать факт о path `[BindingId(event), :kind]`, затем отфильтровать несовместимые варианты всего union. Это развивает F240/F310, а не обещает общий dependent typing.

Цена переноса средняя/высокая: отсутствующий ключ, явно nil, `get` с default, keyword-вызов, destructuring и Java Map имеют разные правила. На первом этапе разрешать известные persistent maps и литеральные ключи; не переносить механизм Racket pair selectors буквально.

### 4. Стабильность факта важнее агрессивности inference

Guide запрещает occurrence typing переменной, если она изменяется через `set!` в области определения: факт может нарушить concurrent mutation. В Clojure lexical binding обычно стабилен, но его referent может быть mutable.

```clojure
(let [snapshot @state]
  (when (string? snapshot)
    (do-other-work)
    (clojure.string/upper-case snapshot))) ; допустимый stable local

(when (string? @state)
  (clojure.string/upper-case @state))       ; другое чтение, нет доказательства
```

Не следует сбрасывать тип локальной строки после любого вызова. Но пути через atom/deref, mutable Java field/array нельзя считать immutable. Новое связывание с тем же текстовым именем получает новый BindingId.

Это уже центральная задача F251/F340. Для MVP достаточно conservative whitelist стабильных объектов; полный анализ эффектов F450 не должен блокировать проверяемые guards над immutable данными. Clojure Vars, `with-redefs`, динамическая загрузка spec остаются границей гарантии: summary верен для проверенного snapshot, а не для всех будущих runtime переопределений.

### 5. Higher-order narrowing — отдельный этап

Guide использует `andmap string?` как источник propositions. F220 Typespec прямо фиксирует, что текущий `filter` сохраняет исходный тип элемента, даже для `some?`.

```clojure
(mapv inc (filter some? [1 nil 2]))
```

Целевой результат: элемент после `filter` имеет тип int. Для `(filter nonempty-string? xs)` достаточно положительного summary; отрицательное не нужно. Для `remove` нельзя использовать отрицание положительного summary: нужен именно достоверный falsy-факт.

Предлагается ограниченное правило F230 + F250: инстанцировать summary callback для символического элемента коллекции и уточнять его на truthy-ветке `filter`. Не выдавать это за уже работающую возможность Typespec. Для lazy seq дополнительно оговорить стабильность функции/Var между созданием и реализацией последовательности, отсутствие runtime подмены и mutation зависимостей. Полноценная поддержка transducer arities — отдельная работа, не автоматическое следствие правила обычного `filter`.

### 6. Optional — правильная граница гарантий, а не отсутствие типов

| Режим Typed Racket | Статическая проверка | Что происходит при взаимодействии с untyped |
|---|---|---|
| Deep | Да | Контракты/защита границы; для mutable values возможны обёртки чтений/записей |
| Shallow | Да | Проверки внешней формы; дополнительные проверки при использовании элементов |
| Optional | Да, те же static types/checks по reference | Типы ничего не обеспечивают динамически; zero runtime cost именно от enforcement |
| no-check | Нет | Сохраняет синтаксис без статической проверки; не аналог Typespec |

Reference приводит неверный импорт `string->list` как функции, возвращающей список строк вместо символов. Optional не ловит взаимодействие во время исполнения. У Typespec аналог — неверная внешняя декларация Java/библиотеки: статическая проверка использования не подтверждает истинность неподтверждённого контракта.

Следствие для F710/F711/F840: у импортированного типа и predicate summary должны быть происхождение, версия и класс доверия. Verified body, встроенная модель и ручной trusted extern — разные основания. Проверка без runtime checks не должна рекламироваться как защита от произвольного untyped кода. Уже существующие проверки Clojure анализировать можно; вставлять новые ради прохождения checker нельзя.

## Предложения и приоритеты — без новых дублирующих карточек

| Приоритет | Существующие карточки | Конкретное уточнение | Цена / критерий готовности |
|---|---|---|---|
| P0 | F240, F210 | Result с раздельными truthy/falsy facts; точные nil/false; conservative intersection/subtraction union | Средняя; тесты `some? false`, невозможная ветка, отрицание, and/or short circuit |
| P0 | F251, F340 | BindingId + stable path + snapshot; never narrow repeated deref | Средняя; shadowing, immutable alias, mutable referent, повторный deref |
| P1 | F250, F720, F840 | Checked summaries, обе стороны отдельно; подстановка параметров; verified/trusted provenance; hash зависимостей | Высокая; ложный guard отвергнут, one-sided не сужает else, warm/cold check совпадают после смены тела/контракта |
| P1 | F240, F310 | Обратное сужение закрытого union maps по immutable path/tag | Средняя/высокая; missing против nil, default get, разные ветки с одинаковым тегом |
| P2 | F230, F250 | Специальные higher-order правила filter/remove на основе summaries | Средняя после P1; filter принимает p+, remove не изобретает p−, lazy/mutable ограничения задокументированы |
| P0 документации | F710, F711, F840 | Явно назвать модель static-only; внешняя декларация — допущение, а не runtime boundary proof | Низкая для текста, средняя для provenance/CI; доверие видно в диагностике |

Стоимость дана относительно объёма механизма, **не оценка в днях**. Первая поставка не требует полноценного SAT/SMT: ограничить формулы, нормализовать одинаковые atoms, сохранять только доказанные факты на join. При исчерпании лимита терять точность, не создавать доказательство. Interning из `prop-rep` — полезная реализационная подсказка; скорость на Typespec не измерена.

## Что не брать

- Deep contracts, Shallow shape checks, автоматическое добавление assert и runtime validators: это нарушает цель Typespec. Существующий assert можно анализировать лишь с учётом того, включён ли он при исполнении.
- `no-check` как якобы аналог Optional: он убирает требуемую статическую проверку.
- Полный язык Racket propositions/числовых refinements, solver линейной арифметики и все зависимые selectors сразу: доказательств окупаемости для текущего прототипа нет.
- Наивный перенос истины Racket: в Clojure falsy включают nil и false.
- Обещание произвольного вывода predicates, доказательство purity через имя `foo?`, отрицание `s/valid?` как вычитание всего базового типа. Spec может содержать дополнительный предикат, не представимый в статическом типе.
- Blanket invalidation любого immutable local после каждого вызова; и противоположную крайность — вечные факты на mutable paths.
- Type-driven оптимизацию или unsafe codegen. Typespec проверяет существующий код; runtime может нарушить trusted extern.

## Ограничения вывода

Нет локальных экспериментов/benchmark, не проверен весь Typed Racket inference, тесты и CI не запускались. Статья прочитана выборочно, доказательство soundness не воспроизведено. Механизм кеша, provenance и правила Clojure maps здесь — проектные рекомендации, не заявленная возможность Typed Racket. Статус Typespec взят из текущих карточек, которые другие агенты могут уточнять параллельно. Это исследовательская записка, не подтверждение реализации roadmap.
