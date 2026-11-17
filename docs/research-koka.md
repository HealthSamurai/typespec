# Koka: что взять для статической проверки существующего Clojure

Дата проверки: **2026-11-17 UTC**. Цель — Typespec, без runtime checks, изменения семантики Clojure и исполнения проверяемого кода. Это исследование и предложения, не реализация.

## Summary

Главная польза Koka — не обработчики эффектов, а устройство контракта функции: **аргументы → эффект вызова → результат**, причём результат тоже может хранить будущий эффект. Это прямо уточняет уже существующую **F450**, а не требует новой карточки. Для первого шага Typespec достаточно консервативных summaries; открытые effect rows стоит добавлять вместе с generic HOF, когда простых summaries станет недостаточно.

Особенно полезен реальный библиотечный тип Koka `delayed<e,a>`: создание задержанного вычисления не исполняет `e`, а `force` возвращает `a` с эффектом `e`. Для Clojure аналог нужен не только `delay`, но и lazy seq, возвращаемым функциям, трансдьюсерам. Чистая фабрика не означает чистое потребление её результата. При этом нельзя механически переносить тип Koka `map`: его список обрабатывается строго, а Clojure `map` ленивый.

## Контекст Typespec и отсутствие дублей

Прочитаны README (включая списки текущих возможностей/ограничений), начало reference с полной заявленной грамматикой, каталог features; отдельно F450 и материалы F230, F340, F311. Поиск связанных мест в карточках уточнил связи с F610/F240/F320/F710. Полный аудит исходников Typespec не проводился.

- **F450** уже предлагает summaries, unknown-effect, pure как проверяемое требование, отложенные эффекты, SCC и отдельную политику исключений. Её статус — `proposed`, `not-implemented`. Ни один из этих пунктов не объявляется новой находкой.
- **F230** различает текущие специальные правила `map` и будущие универсальные generics/HOF. Effect-параметры — дополнение к этой работе, не замена type-параметров.
- **F340** — место для ограничений обобщения изменяемых значений и для будущего escape analysis.
- **F311** — rows структурных данных. Можно разделить инфраструктуру переменных/occurs-check, но семантику record rows и effect rows нельзя считать одинаковой.
- **F710/F711**, **F840** — происхождение внешних effect-контрактов и доверие к ним; **F420** — исключения; **F610** — макросы, чьи тела иначе пропускаются.
- В прочитанной грамматике reference нет публичного синтаксиса effect-параметров. Вся нотация ниже — внутренняя модель/псевдотипы, не поддерживаемый синтаксис Typespec.

## Состояние Koka

Через `curl api.github.com` проверены repository metadata и последние 20 commits default branch:

| Поле | Значение на момент проверки |
|---|---|
| Репозиторий | `koka-lang/koka` |
| Default branch | `dev` |
| Stars / forks | 4126 / 231 |
| Open issues count | 308; GitHub включает в этот счётчик и PR |
| Archived | false |
| Последний commit | `b66091b2a5a7ea20c5ed9498ad1e1f03caee9589` |
| Дата commit, author/committer | 2026-11-17 01:47:17 UTC |
| Сообщение | `bump mimalloc allocator to v3.7.0` |
| Repository pushed_at | 2026-11-17 01:47:40 UTC |
| README: latest release | v3.2.9, 2026-09-17; это утверждение README, release API отдельно не проверялся |

В последних 20 коммитах есть исправления LSP, CI, документации, type substitution и races при delayed values. Это свидетельство текущей работы, **не метрика промышленной зрелости или скорости развития effect inference**. Последний commit касается allocator. README прямо называет Koka v3 research language, ещё не готовым к production. Число stars изменяется; по нему нельзя судить о числе пользователей.

## Источники и глубина чтения

1. [README Koka](https://github.com/koka-lang/koka/blob/b66091b2a5a7ea20c5ed9498ad1e1f03caee9589/readme.md): прочитаны introduction, статус, версия, ссылки и начало build section.
2. [Koka book](https://koka-lang.github.io/koka/doc/book.html#why-effects): загружен HTML, прочитаны разделы о minimal core/effect typing и сопутствующие определения `total`, `pure`, `exn`, `div`. Это живая документация, не immutable версия.
3. Daan Leijen, **Koka: Programming with Row-polymorphic Effect Types** (2014), [полный PDF](https://arxiv.org/pdf/1406.2061), [карточка](https://arxiv.org/abs/1406.2061). PDF скачан и преобразован `pdftotext`; прочитаны введение, §2.3–2.7 об effect polymorphism, duplicate effects, heap effects, heap safety и divergence. Это **не чтение только abstract**, но и не проверка всех формальных доказательств статьи. Синтаксис и часть реализации 2014 года исторические.
4. [Type/Unify.hs](https://github.com/koka-lang/koka/blob/b66091b2a5a7ea20c5ed9498ad1e1f03caee9589/src/Type/Unify.hs): прочитаны dispatch унификации эффектов, `unifyEffect`, `extractNormalizeEffect`, `unifyEffectVar`, начало `unifyLabels`.
5. [Type/Infer.hs](https://github.com/koka-lang/koka/blob/b66091b2a5a7ea20c5ed9498ad1e1f03caee9589/src/Type/Infer.hs): целевой поиск точек generalization и divergence; не полный аудит inference engine.
6. [std/core/delayed.kk](https://github.com/koka-lang/koka/blob/b66091b2a5a7ea20c5ed9498ad1e1f03caee9589/lib/std/core/delayed.kk): прочитан файл целиком, включая `delay`, `force/go`, `force`, `once` и unsafe-границу.
7. [std/core/lazy.kk](https://github.com/koka-lang/koka/blob/b66091b2a5a7ea20c5ed9498ad1e1f03caee9589/lib/std/core/lazy.kk): прочитаны runtime-примитивы lazy constructors; это не модель Clojure lazy seq.
8. GitHub API: [metadata](https://api.github.com/repos/koka-lang/koka), [последний commit](https://api.github.com/repos/koka-lang/koka/commits?per_page=1), [20 commits](https://api.github.com/repos/koka-lang/koka/commits?per_page=20). Динамические endpoints; конкретные значения зафиксированы выше.

Компилятор Koka не собирался и не запускался. Примеры Clojure ниже — предлагаемые тесты анализа, не результаты запуска Typespec. Статьи про Perceus/evidence passing из README не исследованы: их названия не используются как доказательство свойств Typespec.

## Findings

### 1. Эффект — часть типа, а не одно поле `pure?`

В книге Koka функция содержит argument types, effect type, result type. `total` означает отсутствие эффектов, включая `exn` и `div`; `pure` допускает исключения и расходимость, но не наблюдаемые side effects. Поэтому `pure` и `total` различаются даже в самом источнике.

Для Typespec стоит сохранять независимые компоненты: наблюдаемые эффекты (`read-state`, `write-state`, I/O, nondeterminism), возможное исключение, неизвестность. Возможную расходимость нельзя просто опустить и затем назвать функцию total. Гарантии книги опираются на типизированный язык Koka и его trusted primitives, а не на произвольный Java interop.

```clojure
(defn quotient [a b] (/ a b))        ; возможное исключение
(defn now [] (System/currentTimeMillis)) ; зависит от внешнего мира
(defn snapshot [a] @a)              ; чтение mutable state
(defn forever [] (recur))           ; без I/O, но не total
```

Это развитие F450, не повод запускать `quotient` или `forever` при check. Отсутствие известного I/O не даёт разрешения на constant evaluation.

### 2. Effect polymorphism позволяет проверять пользовательские HOF

Статья §2.3 показывает `map : (list<a>, a -> e b) -> e list<b>`. Общий effect-параметр сохраняет связь callback с вызывающей функцией. Это лучше, чем считать любой callback неизвестным или объявлять весь `map` impure.

```clojure
(defn invoke-twice [f x] (f x) (f x))
(invoke-twice inc 1)       ; эффекты inc согласно числовому контракту
(invoke-twice println 1)   ; I/O
```

Псевдотип: `forall a b e. (Fn[a,e,b], a) -> e b`. Количество вызовов не нужно для may-effect, но важно не выдавать may-effect за гарантию exactly-once. Свежая инстанциация `e` нужна **на каждом вызове**, иначе вызов с `println` загрязнит независимый вызов с `inc`.

Статья объясняет цену альтернатив: уравнения вроде `e1 ∪ e2 = e3 ∪ e4` не имеют единственного решения и требуют сохранения constraints. Koka выбирает открытые rows и унификацию; это аргумент для изучения алгоритма, а не доказательство, что Typespec обязан скопировать его целиком.

### 3. Конкретный алгоритм открытых rows уже есть в исходниках

`unifyEffect` в текущем commit:

1. Применяет подстановку и извлекает нормализованные labels + tail.
2. Сопоставляет labels (`unifyLabels`) и получает несовпавшие части.
3. Для открытых хвостов добавляет недостающие labels через свежие effect variables.
4. Унифицирует остаточные хвосты с учётом новых подстановок.
5. Отвергает циклическую ситуацию, где один и тот же tail должен равняться своему непустому расширению (`Infinite`).

В статье §2.4 duplicate labels дают principal unification без дополнительных lacks constraints и позволяют правильно типизировать устранение одного слоя эффекта. Например, обработчик исключения сам может бросать: его `exn` не должен исчезнуть вместе с обрабатываемым `exn`.

**Цена переноса:** отдельный kind effect variables, нормализация, freshening, occurs-check, generalization, ошибки и сериализация summaries. Typespec работает со structural subtyping/union/unknown и уже существующим Clojure: статья не доказывает principal types для этой комбинации. Не следует превращать record-row F311 в effect-row простой заменой имени.

Для первого may-effect анализа множества labels и monotone joins проще. Это другой, более грубый алгоритм, и его нельзя рекламировать как Koka inference. Duplicate rows особенно полезны для nested handlers; для текущей задачи без новых handlers их цена пока не оправдана.

### 4. Отложенный эффект должен переживать возврат значения

Реализованный код `delayed.kk` содержит:

```text
computation<e,a> = XComputation(() -> e a) | XDone(a) | XBlocking
delayed<e,a>
delay : (() -> e a) -> delayed<e,a>
force : delayed<e,a> -> e a
```

Это не roadmap. Но библиотека использует `unsafe-total` и `unsafe-no-state-div-cast`, скрывая внутреннюю мутацию кэша и divergence. Следовательно, даже этот удачный пример содержит **trusted boundary**, а не чистое автоматическое доказательство отсутствия всех внутренних эффектов.

Предлагаемая модель Typespec: `Fn[args,e,result]`, `Delay[e,t]`, `LazySeq[e,t]`. Immediate effect и latent effect — разные позиции, не один объединённый флаг.

```clojure
(defn prepare [] (delay (println "later") 42))
(def d (prepare))  ; создание не печатает
(force d)         ; I/O сохраняется в may-summary

(defn messages [xs]
  (map (fn [x] (println x) x) xs))
(def ys (messages [1 2 3]))
(first ys)        ; может реализовать chunk, а не ровно один callback
(dorun ys)        ; потребляет отложенные эффекты
```

Даже если конкретный `d` уже forced, без typestate/alias analysis безопаснее сохранять `e`: may-effect не обещает повторное исполнение. Lazy seq может иметь свой эффект чтения источника; его тоже надо переносить, не только эффект mapping callback.

Особые случаи: `(map f)` возвращает трансдьюсер, `(partial f x)` — функцию, `future` запускает работу асинхронно, а не откладывает её до deref. Общий контракт «всё вызывается позже при force» для них ошибочен. До отдельных правил — unknown, не pure. Функция, возвращающая lazy-результат, может быть чистой как фабрика, но это не доказательство чистого использования результата.

### 5. Локальное состояние скрывается только при доказанном отсутствии утечки

В §2.5 статьи используются `st<h>` и rank-2 `run`: heap-параметр не должен попасть во входы/результат; локальная мутация тогда ненаблюдаема. §2.6 связывает effects с безопасным let-generalization; изменяемую ячейку нельзя независимо инстанциировать под разные типы. Это источник идей для F340, не готовый анализ Clojure atoms.

```clojure
(defn count-local [xs]
  (let [a (atom 0)]
    (doseq [_ xs] (swap! a inc))
    @a))

(defn leaks []
  (let [a (atom 0)]
    (fn [] (swap! a inc))))
```

`leaks` публикует состояние через closure: «atom создан внутри let» недостаточно. Даже в `count-local` эффекты обхода `xs` нельзя скрыть вместе с локальным atom. Java aliases, callbacks, watches/validators, динамические vars, публикация в другие объекты и потоки резко усложняют доказательство. На первом этапе считать локальную мутацию эффектом; принять false negatives по pure вместо несостоятельной гарантии.

### 6. Исключения нельзя вычитать из summary целиком

```clojure
(try (dangerous)
     (catch IllegalArgumentException e
       (println e)
       (throw (ex-info "failed" {}))))
```

Здесь I/O и новое исключение остаются. `catch` конкретного класса не устраняет другие исключения; `finally` добавляет собственные эффекты. Для первой версии F420/F450 безопасно не снимать общий `may-throw` при частичном catch. Koka duplicate labels объясняют важность сохранения эффекта обработчика, но JVM class hierarchy и unchecked exceptions требуют отдельной модели.

## Предложения и приоритеты — только дополнения к существующим карточкам

| Приоритет | Дополнение | Куда | Цена / условие готовности |
|---|---|---|---|
| P0 | Зафиксировать различие pure / may-throw / unknown / total; не обещать total | F450 | Низкая для design; тест unknown-call не может доказать pure |
| P0 | Effect-контракты audited primitives и цепочка происхождения; extern не равен checked body | F450, F710/F711, F840 | Средняя; Java/неизвестный macro дают unknown |
| P1 | Immediate и latent summary; `delay/force`, lazy `map`, `first/dorun`, возвращаемые функции | F450, F230, F610 | Средняя–высокая; потребление не теряет эффект фабрики/источника |
| P1 | Свежий `e` для пользовательского HOF; bounded inference, явные контракты на границах | F230, F450, F220 | Высокая, зависит от generic solver; два вызова не загрязняют друг друга |
| P1 | Общие summaries по SCC, monotone join; при превышении бюджета unknown | F450, F720 | Средняя; рекурсивное отсутствие I/O не доказывает termination |
| P2 | Исследовать open effect rows после прототипа summaries; отдельно сравнить set constraints | F450, F230 | Высокая; тесты freshening, occurs-check, polymorphic composition |
| P2 | Ограниченное снятие локальных state effects только с escape proof | F340, F450 | Очень высокая для полного Clojure; closures/aliases обязательны в negative tests |

Минимальная внутренняя схема первого прототипа:

```text
infer(expr) -> {type, nowEffects, provenance}
Fn.type carries callEffects; Delay/LazySeq.type carries latentEffects.
call: effects(function-expression) + effects(arguments) + instantiated callEffects.
fn/delay: analyse body, store body effects in result type; do not execute body.
consume: add stored latent effects and consumer's own effects.
unknown target / unsupported macro / analysis limit: unknown, never empty.
```

`+` здесь — консервативное объединение may-effects, не алгоритм унификации rows Koka. Для Clojure special forms/macros отдельные transfer rules должны отражать реальный момент вычисления. Ограниченный набор этих правил важнее раннего универсального effect solver.

## Что не брать

- **Algebraic handlers, evidence passing, Perceus, компиляцию в C**: меняют язык/runtime или решают другую задачу. Typespec не должен переписывать существующий Clojure.
- **`unsafe-total` как пользовательский escape hatch без учёта доверия**: иначе агент сможет объявить любой код чистым. Внешний контракт должен быть помечен trusted, а не verified.
- **Автоматическое исполнение pure-функций при check**: F450 уже правильно отделяет это от проверки; Koka не снимает проблему termination, ресурсов и окружения Clojure.
- **Обещание полной soundness для JVM по образцу теорем Koka**: разные языки, interop, динамические vars и пропущенные макросы.
- **Duplicate labels и rank-2 heap polymorphism в первой версии**: польза есть, но для bounded conservative analysis это слишком большой начальный объём.
- **Одну схему для lazy/eager/asynchronous HOF**: особенно нельзя перенести eager Koka `map` на Clojure `map`.

## Ограничения результата

Историческая статья, живая книга и текущие исходники сознательно разделены. Ключевые механизмы effect rows и delayed values подтверждены кодом, но корректность всего компилятора не проверялась. Ни гарантии Koka, ни предложенная схема не составляют доказательство soundness Typespec. Новых карточек, реализации и runtime checks не создано; предлагается точнее разработать существующую F450 вместе с её зависимостями.
