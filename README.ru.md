# Typespec — статический тайпчекер для Clojure, сделанный для кода, который пишут агенты

> 🇬🇧 [English](README.md) · 🇷🇺 Русский
>
> Статус: **дизайн и работающий прототип** (около 2000 строк, см. [Попробовать](#попробовать)). Здесь дизайн-решения. Детали — в [docs/reference.md](docs/reference.md), реализация и этапы — в [PLAN.md](PLAN.md) (оба на английском).

Typespec проверяет код на Clojure до запуска, как `tsc` проверяет TypeScript. Он начинает с того, что в коде уже есть: `s/def` для данных, `s/fdef` для сигнатур функций, type hints, условия `:pre` и Java-классы. В рантайм ничего не добавляется.

## Почему сейчас

Статические типы не прижились в Clojure по одной главной причине: люди не хотели писать и поддерживать аннотации. Этой цены больше нет. Большую часть нового кода пишут LLM-агенты, а **агенты не ленивые**: агент может разметить всю кодовую базу и обновлять разметку при каждой правке.

Агентам не хватает другого — быстрой обратной связи. В динамическом языке их типичные ошибки не видны, пока код не запустится: опечатка в ключе, неожиданный nil, аргументы не в том порядке, мапа не той формы, несуществующая функция библиотеки. Тайпчекер сообщает о них за секунды и в виде, с которым агент может работать.

Отсюда распределение ролей:

- **Агенты пишут типы.** Разметка дешёвая, поэтому можно требовать сигнатуру у каждой публичной функции.
- **Тайпчекер судит.** Он один для всех агентов и моделей, и его нельзя уговорить.
- **Люди читают меньше.** Проверить сигнатуру и дифф типов быстрее, чем читать тела функций.

Раз размечает агент, в дизайне есть ограждения, которые тайпчекеру для людей были бы не нужны:

| Ограждение | От чего защищает |
|---|---|
| Аннотации — инертные данные; `annotate` никогда не меняет поведение | агент ломает рабочий код, «просто добавляя типы» |
| Бюджет доверия в CI: число `any`, `cast`, `ignore`, доверенных guard не растёт | агент заглушает тайпчекер вместо исправления кода |
| Ослабление типа отслеживается (добавлен `nilable`, убран `:req`, сигнатура → `any?`) | агент «чинит» ошибку, ослабляя тип, который сам написал |
| У каждого автофикса уровень риска: `safe`, `review`, `unsafe` | правки без присмотра, меняющие поведение |
| Типы берутся сначала из схем (FHIR Schema, существующие spec), догадки — в последнюю очередь | правдоподобные, но выдуманные типы |
| Один формат JSON, стабильные id ошибок, группировка по первопричине | агент гоняется за пятьюдесятью симптомами одной ошибки |
| Изменения поведения (`:keys!`, новые хинты) идут отдельным PR от разметки | смешанные диффы, которые невозможно ревьюить |

## Как это выглядит

```clojure
(ns app.patients
  (:require [clojure.spec.alpha :as s]
            [clojure.string :as str]))

(s/def ::db map?)
(s/def :patient/id     string?)
(s/def :patient/name   string?)
(s/def :patient/status #{"active" "inactive"})
(s/def ::patient (s/keys :req [:patient/id :patient/status] :opt [:patient/name]))

(s/fdef find-patient
  :args (s/cat :db ::db :id :patient/id)
  :ret (s/nilable ::patient))
(defn find-patient [db id]
  (get db id))

(s/fdef greeting
  :args (s/cat :patient ::patient)
  :ret string?)
(defn greeting [{:patient/keys [name status]}]
  (str (case status "active" "Hi, ")
       (str/upper-case name)))

(defn greet-first [db]
  (greeting (find-patient db 42)))
```

```
$ clojure -M:check check
src/app/patients.clj:21:8  TSP8001  case is not exhaustive: "inactive" is not handled
src/app/patients.clj:22:24  TSP2531  name may be nil: :patient/name is optional in ::patient
src/app/patients.clj:25:13  TSP2531  (find-patient db 42) may be nil: find-patient returns (s/nilable ::patient)
src/app/patients.clj:25:30  TSP2345  42 is not assignable to :patient/id (string?)
```

Это настоящий вывод прототипа. Код — обычный `clojure.spec`: `s/fdef` и есть сигнатура, для Typespec ничего не добавлено. У `greet-first` нет `s/fdef`, поэтому тип результата выведен из тела.

## Попробовать

```
git clone https://github.com/HealthSamurai/typespec && cd typespec
clojure -M:check check --root demo        # маленькое приложение клиники с 23 заложенными ошибками
clojure -M:check check --root demo --json
clojure -M:check sig clinic.billing --root demo
clojure -M:check type-at src/clinic/patients.clj:57:9 --root demo
clojure -M:test                           # сверяет вывод с файлами ожидаемого результата
```

В [демо](demo/README.md) перечислена каждая ошибка: что она делает в рантайме сейчас и что говорит Typespec. Восемь из 23 в рантайме проходят молча: код возвращает неверное значение, и ничего не падает.

**Откуда прототип берёт типы**, в таком порядке:

| Источник | Пример | Написано для Typespec? |
|---|---|---|
| `s/def` | `(s/def :patient/status #{"active" "inactive"})` | нет |
| `s/fdef` | `(s/fdef greeting :args (s/cat :patient ::patient) :ret string?)` | нет |
| type hints | `(defn slug ^String [^String title ^long max] ...)` | нет |
| условия `:pre` | `{:pre [(s/valid? ::invoice invoice) (number? rate)]}` | нет |
| Java-классы с classpath | `(.plusDays date "30")` | нет |
| шаблоны деструктуризации | `[& {:keys! [host port]}]` | нет |
| тело функции | тип результата, если он нигде не объявлен | нет |
| `{:t/sig '[...]}` | короткая запись сигнатуры для кода без `s/fdef` | да |

**Что работает в прототипе** (каждый пункт покрыт `clojure -M:test`): структурные мапы и чтение ключей с подсказками, union литералов и полнота `case`, отслеживание nil и сужение (`if`, `when`, `if-let`, `cond`, `and`/`or`, `some?`), `->` и `some->`, `map`/`filter`/`reduce` по типизированным коллекциям, `assoc`/`update`/`get-in`/`select-keys`/`merge`, `:keys!` и `:missing` из Clojure 1.13, хинты как сигнатуры, проверки хинтов, вызовы Java-методов с перегрузками, предупреждения о reflection с фиксом, несуществующие имена, `#_(t/cast)`/`#_(t/ignore)`/`#_(t/any)` со счётчиками доверия, вывод в JSON, `sig`, `type-at`, `--strict`, `--strict-nil`.

**Что пока не работает:** generics, `reify`/`proxy`/протоколы/мультиметоды, макросы проекта (выдаётся `TSP7031`, тело пропускается), `typespec/types.edn` и externs для библиотек, кеш по ns и демон, `fix` и `annotate`, бюджет доверия в CI. Java-классы читаются через reflection, а не из байткода. Для стандартной библиотеки есть 64 сигнатуры; макросы core без правила пропускаются.

**Первый запуск на настоящем коде.** Три модуля внутренней библиотеки (5800 строк, spec почти нет) проверяются за 1–4 секунды каждый, вместе со стартом JVM. Результат: 1 ошибка и 39 предупреждений. Ошибка настоящая: `BufferedReader` передаётся в параметр с хинтом `^InputStream`, в рантайме это `ClassCastException`. Предупреждения — вызовы через reflection и неописанные макросы проекта. В коде без spec тайпчекеру почти не за что зацепиться, поэтому и находок мало; чтобы дойти до нуля ложных ошибок, понадобилось около десяти правок тайпчекера.

## Ещё примеры

Каждая строка `сейчас` проверена запуском на Clojure 1.13.0-alpha11. Прототип уже выдаёт все строки `TSP` ниже, кроме строки про `reify` (см. `test/examples` и демо); формулировки некоторых сообщений отличаются.

### Type hints — это типы

Хинты, которые уже есть в коде, работают как сигнатура, а тайпчекер говорит, когда хинт неверен или стоит не там.

```clojure
(defn slug ^String [^String title ^long max]          ; хинты и есть сигнатура
  (subs (.toLowerCase title) 0 (min max (.length title))))

(slug 42 "hello")
;;    ^^ TSP2345: 42 is not assignable to String
;;       ^^^^^^^ TSP2345: "hello" is not assignable to long
;; сейчас: ClassCastException при запуске

(defn ^long total [xs] (reduce + xs))
;;    ^^^^^ TSP7021: a hint on the name is not a return type; write (defn total ^long [xs] ...)
;; сейчас: каждый вызывающий не компилируется: "Unable to resolve classname: clojure.core$long@…"

(defn first-len
  {:t/sig '[(s/coll-of int?) :-> int?]}
  [ids]
  (let [^String s (first ids)]
;;      ^^^^^^^ TSP2322: hint String contradicts the type: int?
    (.length s)))
;; сейчас: ClassCastException: Long cannot be cast to String
```

### Работа с Java

Java-типы читаются из байткода, поэтому interop проверяется без деклараций. Тайпчекер знает типы, которых не видит компилятор, и может сам вставить хинты, убирающие reflection.

```clojure
(defn size
  {:t/sig '[java.io.File :-> long]}
  [f]
  (.length f))
;; TSP7023: reflective call, but f is known to be java.io.File
;;          fix (safe): (.length ^java.io.File f)
;; сейчас: предупреждение о reflection и медленный вызов; (size "abc") молча вернёт 3

(defn request-id-length ^long [^java.util.Map headers]
  (.length ^String (.get headers "x-request-id")))
;; TSP2531: (.get headers "x-request-id") may be nil: java.util.Map.get may return null
;; сейчас: NullPointerException, когда заголовка нет

(def by-pair
  (reify java.util.Comparator
    (compare [_ a b] (str a b))))
;;                   ^^^^^^^^^ TSP2322: String is not assignable to int, the return type of Comparator.compare
;; сейчас: компилируется; ClassCastException при первом сравнении
```

### Деструктуризация

Деструктуризация сверяется с объявленным типом: опечатка в ключе — ошибка, а не молчаливый nil.

```clojure
(defn display-name
  {:t/sig '[::patient :-> string?]}
  [{:patient/keys [id nmae]}]
;;                    ^^^^ TSP2339: :patient/nmae is not a key of ::patient. Did you mean :patient/name?
  (str id " " nmae))
;; сейчас: вернёт "p1 ", и никто не заметит

(s/def :job/retries (s/nilable int?))

(defn next-try
  {:t/sig '[(s/keys :opt [:job/retries]) :-> int?]}
  [{:job/keys [retries] :or {retries 0}}]
  (inc retries))
;;     ^^^^^^^ TSP2531: retries may be nil: :job/retries is (s/nilable int?), and :or applies only when the key is absent
;; сейчас: (next-try {:job/retries nil}) бросает NullPointerException
```

### Clojure 1.13: `:keys!`, обязательные именованные аргументы, `:missing`

Новые директивы бросают исключение в рантайме, когда ключа нет. Typespec сообщает о том же в месте вызова, до запуска.

```clojure
(s/def :address/city string?)
(s/def :contact/address (s/keys :req [:address/city]))
(s/def ::contact (s/keys :req [:contact/id] :opt [:contact/address]))

(defn city
  {:t/sig '[::contact :-> string?]}
  [{{:address/keys! [city]} :contact/address}]
;;                          ^^^^^^^^^^^^^^^^ TSP2741: :keys! below requires :contact/address, but it is optional in ::contact
  city)
;; сейчас: (city {:contact/id "c1"}) бросает "Missing required key: :address/city"

(defn- connect [& {:keys! [host port]}] ...)

(connect :host "localhost")
;; TSP2741: required key :port is missing
;; сейчас: IllegalArgumentException: Missing required key: :port

(defn- register [{:user/keys! [email name] :missing m}]
  (if m
    {:error m}                    ; здесь email и name могут быть nil
    (send-welcome email name)))   ; здесь оба снова сужены до не-nil
```

### Мапы и сужение

```clojure
(assoc patient :patient/status "archived")
;;                             ^^^^^^^^^^ TSP2322: "archived" is not assignable to :patient/status (#{"active" "inactive"})
;; сейчас: в мапе теперь статус, которого не ждёт ни один кусок кода

(get-in bundle [:bundle/entry 0 :entry/resourse])
;;                              ^^^^^^^^^^^^^^^ TSP2339: no such key. Did you mean :entry/resource?
;; сейчас: nil

(defn label
  {:t/sig '[(s/nilable ::patient) :-> string?]}
  [p]
  (if p (:patient/id p) "unknown"))     ; ok: внутри ветки p сужен до ::patient
```

### Что получает агент

Диагностики в JSON, с фиксом, который агент применяет не раздумывая, и команды, чтобы спрашивать про типы, а не угадывать. Это настоящий вывод на демо.

```json
{"id": "a34443c4", "code": "TSP7023", "severity": "warning", "file": "src/clinic/export.clj",
 "range": {"start": [39, 3], "end": [39, 17]},
 "message": "reflective call, but file is known to be java.io.File",
 "fixes": [{"title": "Add hint ^java.io.File", "risk": "safe",
            "edits": [{"range": {"start": [39, 12], "end": [39, 12]}, "text": "^java.io.File "}]}]}
```

```
$ clojure -M:check type-at src/clinic/patients.clj:57:9 --root demo
(s/nilable :clinic/patient)

$ clojure -M:check sig clinic.billing --root demo
line-total     [:clinic/line :-> number?]   ; from s/fdef
total          [:clinic/invoice :-> number?]   ; from s/fdef
new-invoice    [:invoice/id :clinic/patient :-> :clinic/invoice]   ; from s/fdef
with-tax       [:clinic/invoice number? :-> number?]   ; from the body
...
```


## Typespec и TypeScript

`+` — Typespec по дизайну впереди, `=` — та же идея, `−` — впереди TypeScript. Колонка Typespec описывает дизайн; прототип покрывает его часть (см. [Попробовать](#попробовать)).

| | TypeScript | Typespec | |
|---|---|---|:-:|
| Где пишутся типы | синтаксис языка, стирается компилятором | цитированные данные в метаданных, `s/def`, EDN; стирать нечего | = |
| Форма объекта | `interface`, `type` | `s/keys` | = |
| Тип поля | объявляется в каждом интерфейсе | объявляется у ключа, один раз на весь проект | + |
| Структурная типизация, открытые объекты | да | да | = |
| Проверка лишних ключей в литералах | да | да | = |
| Точные (закрытые) объекты | нет | `t/exact` | + |
| Union и литеральные типы | `"a" \| "b"` | `#{"a" "b"}`, `s/or` | = |
| Пересечения | `A & B` | `s/merge`, `t/and` | = |
| Защита от null | `strictNullChecks` | `s/nilable`; правила для nil-punning | = |
| Сужение | `typeof`, `in`, `instanceof`, type predicates | `if`/`when`/`cond`/`case`, `some?`, `instance?`, `contains?`, проверяемые guard-функции | = |
| Полнота разбора | приём с `never` | встроена: `case`, `cond`, закрытые мультиметоды | + |
| Generics с ограничениями | `<T extends X>` | `(t/all [(a :< X)] ...)` | = |
| Утилитные типы | `Pick`, `Omit`, `Partial`, `keyof`, `T[K]` | `t/pick`, `t/omit`, `t/partial`, `t/keys-of`, `t/get` | = |
| Conditional, mapped, template literal types | да | нет | − |
| `unknown` и `any` | да | `t/unknown` и `any?` с причиной | = |
| `as const` | да | `#_(t/const)` | = |
| Обходы | `as`, `@ts-ignore`, `@ts-expect-error` | `#_(t/cast)`, `#_(t/ignore "причина")`, считаются бюджетом доверия | + |
| Перегрузки | да | несколько сигнатур в одном `t/sig` | = |
| Вывод типов | локальный, контекстный для колбэков | локальный; контекстный для `map`/`filter`/`reduce`; частично для `comp`, `partial`, трансдьюсеров | − |
| Модули | файл; `isolatedDeclarations` | namespace; в strict публичные сигнатуры обязательны | = |
| Типы для библиотек | `.d.ts`, DefinitelyTyped | externs в EDN, `typespec.exports/` в jar; экосистемы пока нет | − |
| Типы платформы | `lib.d.ts`, написан руками | Java-типы читаются из байткода | + |
| Номинальные типы | нет (бренды по соглашению) | records и Java-классы; `t/brand` (опция) | + |
| Проверка в рантайме | отдельный инструмент (zod, io-ts) | тот же `s/def` проверяет и в рантайме | + |
| Мутации и вариантность | `readonly`, известные дыры в надёжности | данные неизменяемые; atom и каналы инвариантны | + |
| Макросы | в языке нет | нужно описывать (`:lint-as`, hooks); внутри своих DSL слабее | − |
| Динамический код | `eval` редок | переопределение в REPL, `binding`, `resolve` — обычное дело; тип `t/unknown` | − |
| Данные-DSL | типизированные билдеры и JSX — обычное дело | honeysql, hiccup, конфиги Integrant типизируются как обычные данные, без смысла | − |
| Ключи, собранные в рантайме | template literal types | `t/unknown` или `s/map-of` | − |
| Редактор | tsserver | через clojure-lsp, в плане | − |
| Инкрементальная проверка | `--incremental`, project references | кеш интерфейсов по ns, параллельная проверка | = |
| Постепенное внедрение | `allowJs`, флаги строгости | `:strict`, файл baseline | = |
| Поддержка агентов | текстовые диагностики, language server | JSON со стабильными id и первопричинами, фиксы с уровнем риска, `sig`/`type-at`/`order`, бюджет доверия | + |
| Зрелость | 12+ лет, огромная экосистема | дизайн и прототип | − |

Строки с `−` — честные пределы. Часть из них навсегда: тайпчекер видит файлы, а не запущенный образ REPL, макросы нужно описывать, а данные-DSL получают смысл только при интерпретации. Остальное — вопрос работы: externs для библиотек, редактор, вывод для `comp` и трансдьюсеров.

## Дизайн-решения

У каждого решения есть значение по умолчанию. Там, где реальному проекту может понадобиться другое, есть опция.

**1. Без рантайма.** Typespec не добавляет ни проверок, ни обёрток, ни зависимости. Сигнатуры — цитированные данные в метаданных var; обходы — комментарии `#_`, которые Clojure отбрасывает.
*Опция:* `:signatures :sidecar` держит все сигнатуры в `typespec/types.edn` и не трогает исходники.

**2. Один язык типов, и это данные.** Подмножество spec-форм плюс несколько форм `t/`. Одна запись используется в `s/def`, сигнатурах, externs, сообщениях об ошибках и JSON: что тайпчекер напечатал, то можно вставить в код. Всё вне подмножества — ошибка, а не молчаливый `any?`.

**3. Ключ = тип.** Квалифицированный ключ `:patient/id` везде означает одно и то же. Это главная идея spec, и благодаря ей переименование и поиск опечаток работают по всему проекту.
*Опция:* ключ объявляется в `s/def` (настоящий spec, годится и для рантайма) или только как тип в `typespec/types.edn`.

**4. Сигнатуры на границах, вывод внутри.** У публичной функции есть сигнатура: `s/fdef`, type hints или `{:t/sig '[...]}`. Сначала используются существующие аннотации; `t/sig` — лишь короткая запись для кода, где их нет. Тела и приватные функции выводятся локально. Вывода через цепочки вызовов нет. Это модель TypeScript: она делает проверку быстрой, а ошибки локальными.
*Опция:* `:strict false` и файл baseline для постепенного внедрения: старый код не блокируется, новые ошибки блокируются.

**5. Исходник читается, а не загружается.** Декларации берутся из текста файлов. Проверка ns не исполняет его верхний уровень: нет побочных эффектов и нет устаревшего реестра.

**6. Макросы описываются, а не исполняются.** Для макросов core есть встроенные правила. Макросы проекта описываются как в clj-kondo: `:lint-as` или маленький hook. Неописанный макрос — явная ошибка `TSP7031`, его тело получает тип `t/unknown`.
*Опция:* `:macros {:expand true}` раскрывает неизвестные макросы в изолированном рабочем процессе.

**7. Type hints — это типы.** `^String`, `^long` и `^[String]` читаются как типы; функции с хинтами на всём другая сигнатура не нужна. Тайпчекер сообщает о неверных, пропущенных и не туда поставленных хинтах и умеет вставлять доказанные хинты, убирающие reflection. Хинты влияют на компиляцию, поэтому Typespec вставляет их только через явный `fix`.

**8. Java-типы берутся из байткода.** Классы, подтипы, перегрузки, generics и аннотации nullability читаются с classpath. Деклараций для Java писать не нужно.
Значение с хинтом и результат Java-метода — *платформенный тип*, как в Kotlin: nil не проверяется, но место учитывается.
*Опция:* `:strict-nil true` превращает платформенные типы в `T | nil`.

**9. Мапы структурные и открытые.** Тип мапы перечисляет нужные ключи и допускает лишние. Чтение необъявленного ключа — ошибка. Литералы проверяются на лишние ключи против объявленного типа. `t/exact` закрывает мапу.

**10. Деструктуризация проверяется, а не служит источником.** Если тип объявлен, деструктуризация сверяется с ним. `:keys!` и остальное из Clojure 1.13 тайпчекер понимает, но это код, меняющий поведение, поэтому сам Typespec их не ставит и не убирает.

**11. Namespace — это модуль.** Ns проверяется по *сигнатурам* зависимостей, а не по их телам. Это даёт кеш, параллельную проверку и быстрый цикл в редакторе.

**12. У библиотек — externs.** Типы чужих библиотек на Clojure — версионированные EDN-файлы, которые никогда не загружаются как код. Библиотека может поставлять свои внутри jar, как `.d.ts`.

**13. Честные дыры.** Всё недоказанное видно и посчитано: `t/unknown`, `any?` с причиной, cast, externs, платформенные типы. Бюджет доверия не даёт этому числу расти.

**14. Сначала TypeScript.** Где у TypeScript есть ответ, берём его: структурные типы, сужение, union литералов, `unknown` против `any`, `isolatedDeclarations`, коды ошибок. Несколько идей из OCaml и F# (провайдеры типов, бренды, row-полиморфизм) — опциональные расширения, каждое за флагом.

## Рядом с существующими инструментами

| Инструмент | Что это | Почему здесь его мало |
|---|---|---|
| clojure.spec, malli | проверка и генерация в рантайме | проверки срабатывают, когда код уже запущен |
| clj-kondo | быстрый статический линтер, читает исходник | типы поверхностные: арность и простые несовпадения |
| [crabjure](https://github.com/ribelo/crabjure) | линтер в духе clj-kondo на Rust, proof of concept, по своим замерам примерно в 20 раз быстрее clj-kondo | это линтер; его тайпчекер — эксперимент за флагом, типы пишутся в комментариях (`@param`, `@return`) |
| Typed Clojure | настоящая статическая система типов (порт Typed Racket) | загружает проверяемый код, свой синтаксис типов, один мейнтейнер |

Typespec берёт модель чтения у clj-kondo, ядро системы типов у Typed Clojure, запись типов у spec, а продуктовые решения у TypeScript.

## Документы

- [Карточки фич](features/README.md) — предложения из Flow: мотивация, примеры, статус реализации и открытые вопросы.

- [docs/reference.md](docs/reference.md) — язык типов, источники типов, правила системы типов, хинты и Java, externs, обходы, конфиг, CLI и JSON, опциональные расширения, коды ошибок.
- [demo/README.md](demo/README.md) — демо-проект: 23 ошибки, что они делают в рантайме и что сообщает Typespec.
- [PLAN.md](PLAN.md) — архитектура, выбор реализации, что уже есть, этапы, открытые вопросы.
