# Независимое review: solver refs/maps и lifecycle-контракты (Claude Fable 5.2)

Scope: `git diff 01fa96e..90ff9d4` (solver refs/maps, `macro_contracts`, интеграция в `check.clj`, тесты, пример `context-web-pg`), с заходом в rank-1 ядро `9106c2d` там, где новый код на него опирается. Зелёному suite (75 тестов / 386 assertions) не доверял: каждое подозрение воспроизведено маленьким скриптом через `check/check-files` или `solver/solve`. Среда: OpenJDK 25.0.2, Clojure 1.12.4. Сервисы и базы не запускались; единственная runtime-проверка — `system/start-system` из закреплённого `context/system` без БД и HTTP (F12).

Строки и поведение «actual» в findings указаны по состоянию `90ff9d4`, до исправлений. Что исправлено и что осталось — в разделе «Исправления».

## Findings

### F1. High — ложное доказательство: `vector?` принимается как `(s/coll-of string?)` внутри union/вложенности
`src/typespec/solver.clj:105,130-131`, корень в `src/typespec/types.clj:204-205`.

Solver знает, что базовое правило «примитив-коллекция подходит к любому `coll-of`» — не доказательство, и ставит guard `(and (= :prim (:t a)) (= :coll (:t b)))`. Но guard смотрит только на верхний уровень обязательства. Правые union не раскладываются (строка 105), поэтому тот же случай под `s/nilable`, в поле карты под union или в результате функции уходит в `t/sub-result` и возвращается `:proved`.

```clojure
(defn pick  {:t/sig '(t/all [a] [a (s/nilable (s/coll-of string?)) :-> a])} [x names] x)
(defn pick1 {:t/sig '(t/all [a] [a (s/coll-of string?) :-> a])} [x names] x)
(defn f {:t/sig '[vector? :-> int?]} [v] (pick 1 v))   ; actual: чисто
(defn g {:t/sig '[vector? :-> int?]} [v] (pick1 1 v))  ; actual: TSP210-01
```
Ожидалось: оба вызова не доказаны (TSP210-01). Фактически: добавление `s/nilable` к цели превращает «не проверено» в «доказано». То же на уровне solver: `{:xs vector?} <: (s/nilable {:xs (s/coll-of string?)})` → `:proved`, а без nilable → `:inconclusive`.

Fix: строгий режим для всех конкретных проверок solver — правило `prim → coll-of/map-of` даёт inconclusive на любой глубине (dynamic-флаг в `types.clj`, включается в `solver/evidence`); guard верхнего уровня становится лишним.

### F2. High — ложная ошибка на типичном «свёрточном» вызове с литеральным seed
`src/typespec/check.clj:455-467`.

Контекст callback берётся из частичного решения, где кандидат — литеральный тип (`0`, `""`). Параметр callback замораживается как `0`, результат callback (`int?`) расширяет ту же переменную, и финальная проверка `a <: 0` падает.

```clojure
(defn fold {:t/sig '(t/all [a b] [(t/fn [b a :-> b]) b (s/coll-of a) :-> b])} [f init xs] (reduce f init xs))
(defn sum {:t/sig '[(s/coll-of int?) :-> int?]} [xs] (fold (fn [acc x] (+ acc x)) 0 xs))
;; actual: TSP2345 fold: int? is not assignable to 0
(defn iter {:t/sig '(t/all [a] [a (t/fn [a :-> a]) :-> a])} [x f] (f x))
(defn h {:t/sig '[:-> int?]} [] (iter 1 (fn [x] (inc x))))   ; actual: TSP2345 int? is not assignable to 1
```
Ожидалось: чисто (с `int?`-параметром вместо литерала так и есть). С пустым seed `[]` аналогично: `int? is not assignable to t/never`.

Fix: при построении контекста callback расширять литералы до примитивов (это ужесточает требование к callback, поэтому безопасно); контекст с `t/never` внутри (пустая коллекция) считать недостаточным — тело проверяется без контекста, вызов остаётся «не проверен», а не ошибкой.

### F3. Medium — ложный TSP2345 для параметра `(s/nilable a)`
`src/typespec/solver.clj:105`.

Правый union с переменной не раскладывается, нижняя граница через него не собирается, а mismatch выносится по неполному кандидату.

```clojure
(s/def ::nick (s/nilable string?)) (s/def ::user (s/keys :req-un [::nick]))
(defn or-else {:t/sig '(t/all [a] [(s/nilable a) a :-> a])} [x d] (if (nil? x) d x))
(defn f {:t/sig '[::user :-> string?]} [u] (or-else (:nick u) "anon"))
;; actual: TSP2345 or-else: string? is not assignable to (s/nilable "anon")
```
Ожидалось: чисто (`a = string? | "anon"`). С типизированным `d string?` — чисто, то есть ошибка зависит от литерала.

Fix: в `obligations` для `L <: (закрытые… | открытая-альтернатива)` — если `L` доказуемо входит в закрытую часть, обязательство снято; иначе при единственной альтернативе с переменной сводить к `L <: альтернатива`. При нескольких открытых альтернативах mismatch понижать до inconclusive.

### F4. Medium — кандидат при «грязных» нижних границах утекает как тип
`src/typespec/solver.clj:152`, `src/typespec/check.clj:412-426,455`.

`good (remove uncertain? low)` отбрасывает нижние границы `unknown`/`any` и считает остаток решением. Статус честно inconclusive, но подстановка уходит (а) в тип результата, (б) в контекст callback. Флаг `:generic-failure` живёт только на верхнем уровне типа и теряется при вложении.

```clojure
(defn pick {:t/sig '(t/all [a] [a a :-> a])} [x y] x)
(defn unk [x] x)
(defn f {:t/sig '[:-> ::item]} [] {:title (pick 1 (unk "s"))})
;; actual: TSP2322 value of :title: 1 is not assignable to ::title  — ложная ошибка, a = 1 | unknown
(defn app2 {:t/sig '(t/all [a b] [a a (t/fn [a :-> b]) :-> b])} [x y f] (f x))
(defn g {:t/sig '[int? :-> int?]} [n] n)
(defn h {:t/sig '[:-> int?]} [] (app2 "s" (unk 1) (fn [x] (g x))))
;; actual: TSP2345 "s" is not assignable to int?  — callback типизирован неполным кандидатом
```
Обратная сторона того же механизма — потерянная диагностика: при полностью известном кандидате флаг всё равно глушит проверку возврата.
```clojure
(defn two {:t/sig '(t/all [a b] [a b :-> a])} [x y] x)
(defn f {:t/sig '[:-> string?]} [] (two 1 (unk 2)))   ; actual: только TSP210-01; `1` как string? не ловится
(defn g2 {:t/sig '[string? :-> string?]} [s] s)
(defn f2 {:t/sig '[:-> string?]} [] (let [x (two 1 (unk 2))] (g2 x)))   ; то же
```
Fix: solver возвращает множество «неопределённых» переменных (есть сомнительная нижняя граница, транзитивно); их кандидаты не подставляются в результат и в контекст callback. `:generic-failure` ставить только при mismatch или когда результат реально содержит unknown; известный результат проверяется дальше.

### F5. Medium — macro contract перехватывает не-макросы и формы core
`src/typespec/check.clj:1324-1326`, `src/typespec/macro_contracts.clj:39`.

Для неквалифицированной головы ключ контракта конструируется как `<текущий-ns>/<имя>` без проверки, что такой макрос есть. Контракт проверяется раньше правил special forms и раньше обычного вызова. Валидация конфигурации отвергает только `clojure.core/*`.

```clojure
;; typespec.edn: {:macro-contracts {m/run-it {:shape :binding-body :bindings [int? int?] :ret any?}}}
(defn run-it {:t/sig '[(s/coll-of int?) int? :-> int?]} [v n] n)
(defn f {:t/sig '[int? int? :-> int?]} [a b] (run-it [a b] "oops"))
;; actual: только TSP210-01; без контракта — TSP2345 "oops" is not assignable to int?
;; ключ m/let: тело каждого (let [a x] ...) в ns m проверяется с типами контракта
;; ключ m/when-let: TSP7032 вместо реального TSP2339 про опечатку ключа
```
Ожидалось: контракт на имя, которое в проверяемых исходниках объявлено функцией, — ошибка конфигурации TSP9004; неквалифицированная голова без `:refer` не должна совпадать с контрактом, если текущий ns не объявляет такой макрос.

Fix: (1) `compile-contracts` отвергает ключ, для которого в env есть не-макро var; (2) в `infer-list` fallback `<ns>/<имя>` только когда `(:macros env)` содержит этот символ.

### F6. Medium — экспоненциальная нормализация именованных карт
`src/typespec/solver.clj:53-82`.

`normalize` раскрывает ссылки деревом; бюджет ограничивает только глубину (12), а не размер. Общие подтипы раскрываются заново в каждом поле, и так дважды на ограничение (до и после подстановки).

Repro (solver, `::tN` = карта из W полей `::tN-1`): глубина 5 × ширина 10 — 0.9 c; 5×20 — 19 c; 5×30 — больше 60 c (прервано). Через checker: 18 полей × 5 уровней, один вызов `(id x)` — 12.8 c. Для FHIR-подобных схем это реальный размер.

Fix: общий счётчик узлов на вызов `solve`; при исчерпании — `unknown` с причиной `:size` → inconclusive, как и для глубины.

### F7. Medium — rigid без границы в конкретный тип даёт только warning
`src/typespec/types.clj:192-193`.

```clojure
(defn bad {:t/sig '(t/all [a] [a :-> string?])} [x] x)
;; actual: TSP210-01 warning (unbounded-rigid), exit 0
(defn caller {:t/sig '[:-> string?]} [] (bad 42))   ; чисто — доказано по неверной декларации
```
Для произвольного `a` утверждение `a <: string?` ложно, это не «недостаток информации». Обратное направление (`1` вместо `a`) уже ошибка.

Fix: rigid с границей unknown не является подтипом ничего, кроме себя и top → mismatch (TSP2322/TSP2345).

### F8. Low/Medium — тело `#(...)`-callback не получает контекст, ошибки теряются
`src/typespec/check.clj:435-436`.

```clojure
(defn f {:t/sig '[::item :-> any?]} [x] (app x #(:titel %)))        ; actual: только TSP210-01
(defn g {:t/sig '[::item :-> any?]} [x] (app x (fn [i] (:titel i))))  ; TSP2339 + подсказка
```
`inline?` узнаёт только `(fn ...)`. Обычный `(map #(:titel %) xs)` опечатку ловит. `docs/solver-usage.md` обещает проверку тела callback.

Fix: считать `#()` контекстным callback: арность по старшему `%N`, `%&` — без контекста.

### F9. Low — параметр `any?` делает generic-вызов недоказуемым
`src/typespec/solver.clj:126`.

`(defn with-any {:t/sig '(t/all [a] [a any? :-> a])} ...)`, вызов `(with-any 1 2)` → TSP210-01 (в `--strict` — ошибка). `uncertain?` применяется и к цели, хотя цель `any?`/`t/unknown` — top, в неё подходит всё.

Fix: цель `any`/`unknown` без `:solver-reason` — `:proved`.

### F10. Low — `[_ _]` отвергается как «неразличимые символы»
`src/typespec/macro_contracts.clj:62`. `(system/defstart [_ _] nil)` — валидный Clojure, даёт TSP7032; пример обходит это именами `_ctx _state`. Fix: не учитывать `_` в проверке уникальности.

### F11. Low — несовпадение арности callback даёт расплывчатый warning
`src/typespec/solver.clj:109-110`. `(app x (fn [a b] "s"))` → `TSP210-01 (unresolved, unresolved-variable)`. Обе стороны — одна фиксированная арность, это точный mismatch. Fix: `:mismatch :arity`.

### F12. Low — доверенная граница примера неточна: config в `defstart` бывает nil
`examples/context-web-pg/typespec.edn:6`. Контракт объявляет `map?`. Проверено запуском закреплённого `context/system` (без БД/HTTP): модуль с манифестом без `:config` получает `config = nil`; именно так устроен `todo.app`. Тело, использующее config как карту, было бы «доказано» по ложной посылке. Fix: `(t/or map? nil?)`.

### F13. Low (базовый checker, вне diff) — противоречащий kind принимается
`src/typespec/types.clj:204`. `set?` как `(s/coll-of string? :kind vector?)` и `vector?` как `:kind set?` — чисто. Fix: примитив с известным видом не подходит к другому `:kind`.

## Что проверено и дефектом не является

- Обязательные/optional ключи: optional в источнике не становится required (и через `id`, и через `(s/coll-of a)`), отсутствующий ключ — mismatch, лишние поля разрешены.
- Вариантность: параметры fn контравариантны, результат и поля карт ковариантны.
- Rigid: возврат конкретного значения, другой переменной, результата callback не того типа — ошибки; bounded rigid работает по границе.
- Циклы, отсутствующие ссылки, глубина > 12 — inconclusive, не proved; unresolved ivar не утекают в `:types`.
- Контракты: quote/syntax-quote/`comment` не проверяются как вызовы; локальное имя затеняет макрос; макросы не загружаются и не исполняются; неверная конфигурация и форма вызова — TSP9004/TSP7032; hint на binder, противоречащий контракту, — ошибка.
- Пример: `clojure -M:check` — 0/0; `src negative` — exit 1, два TSP2345. Без `:macro-contracts` тот же negative даёт TSP2345 и warning про unknown ctx — литерал `42` ловится и так, реальная польза контракта — в типе `ctx`.

## Известные ограничения (не дефекты)

- Рекурсивные именованные типы через generic всегда «не проверено» (зафиксировано тестом `bounded-named-reference-evidence`).
- Solver не поддерживает Java-классы, `s/map-of`, `s/merge`: `(id file)` — TSP210-01. Карта с полем `any?` через generic тоже не доказывается.
- Открытые карты: отсутствующий optional-ключ считается совместимым, хотя значение может его содержать с другим типом (как в базовом checker).
- `:refer :all` и `:use` не разрешаются: контракт к такому вызову не применяется, тело идёт с unknown-контекстом.
- Многоарные и variadic callbacks, callback-значение полиморфной функции — «не проверено».
- Несовпадение формы под переменной (`42` в `(s/coll-of a)`, vector в `:kind set?` с переменной) — «не проверено», а не ошибка.
- Контракт не описывает сгенерированные vars (`start`/`stop`) и значение самой macro-формы; `:levels {TSP9004 :off}` позволяет заглушить ошибку конфигурации.

## Исправления

Все тринадцать пунктов исправлены в том же коммите, что и этот документ; на каждый есть regression в `test/typespec/review_fixes_test.clj` (13 тестов, 85 assertions). На коде `90ff9d4` этот файл даёт 48 падений во всех 13 тестах, после исправлений — 0.

| # | Что сделано | Где |
|---|---|---|
| F1 | В проверках solver «голая» коллекция (`vector?`, `map?`) не доказывает `coll-of`/`map-of` на любой глубине: `t/*strict-shapes*`. Обычная (gradual) проверка не изменилась | `types.clj`, `solver.clj` evidence |
| F2 | Seed переменной, которую возвращает callback, расширяется (`0` → `int?`); пустой seed контекста не даёт. Литеральные union, которые callback не возвращает, остаются точными | `check.clj` callback-substitution |
| F3 | Цель-union с переменной раскладывается: закрытая часть доказала — готово; иначе единственная открытая альтернатива получает нижнюю границу; несколько открытых — «не проверено», не ошибка. Литерал рядом со своим примитивом поглощается (`"anon" \| string?` → `string?`) | `solver.clj` obligations, absorb-literals |
| F4 | `solve` возвращает `:uncertain`; такие кандидаты не идут ни в тип результата, ни в контекст callback. `:generic-failure` только при mismatch или неизвестном результате; известный результат проверяется дальше | `solver.clj`, `check.clj` generic-result |
| F5 | Ключ контракта на функцию или форму со встроенным правилом — TSP9004; неквалифицированная голова совпадает с контрактом, только если текущий ns объявляет такой макрос | `macro_contracts.clj`, `check.clj` infer-list |
| F6 | Общий лимит узлов на один `solve` (`:max-nodes`, по умолчанию 50000) → inconclusive с причиной `size`. Случай 5×30: было > 60 c, стало ~0.1 c | `solver.clj` normalize |
| F7 | Rigid без границы не подходит ни к чему, кроме себя и top: TSP2322/TSP2345 вместо warning | `types.clj` sub1? |
| F8 | `#(...)` — контекстный callback: арность по старшему `%N`, тело проверяется с типами параметров; `%&` — без контекста | `check.clj` infer-fn-lit |
| F9 | Объявленная цель `any?`/`t/unknown` — proved; unknown от неразрешённой ссылки по-прежнему inconclusive | `solver.clj` evidence |
| F10 | `_` может повторяться в binder-векторе | `macro_contracts.clj` |
| F11 | Две фиксированные арности разной длины — mismatch `arity` | `solver.clj` obligations |
| F12 | В примере config у `defstart` — `(t/or map? nil?)` | `examples/context-web-pg/typespec.edn` |
| F13 | Примитив с известным видом не подходит к другому `:kind` | `types.clj` sub1? |

Попутно: rigid-переменная считается известным типом при выборе контекста callback (раньше её unknown-граница отключала контекст, и `(app x (fn [v] v))` внутри generic-тела был «не проверен»); `:generic-failure` не участвует в сравнении типов, поэтому `a | a` снова схлопывается в `a`.

Проверки после исправлений:

- `clojure -M:test` — 88 тестов / 471 assertions, 0 failures, 0 errors; golden/CLI — 14 из 14 ok, эталоны не менялись.
- `examples/context-web-pg`: `clojure -M:check` — 0 errors / 0 warnings, exit 0; `clojure -M:check src negative` — exit 1, те же два TSP2345.
- Runtime-тесты примера (PostgreSQL/HTTP) не перезапускались: менялся только `typespec.edn`, который читает один checker.

Что осталось после исправлений:

- Лимит 50000 узлов задан в коде; из `typespec.edn` он не настраивается. Очень широкие схемы через generic дадут «не проверено (size)», а не доказательство.
- `{:a int?}` в `(s/nilable (s/keys ... any?-поле))` через generic по-прежнему «не проверено»: правый union без переменных не раскладывается.
- Цель с двумя и более альтернативами, содержащими переменные (`(t/or a (s/coll-of a))`), — всегда «не проверено».
- Расширение seed до примитива делает контекст callback менее точным: для `(iter :a (fn [x] ...))` параметр — `keyword?`, не `:a`.
- После mismatch в одной ветви тип-кандидат этой ветви всё ещё может попасть в union с другим типом и дать вторичную диагностику.

## Непроверенные риски

- Тип `state` в `defstop` (`(t/or map? nil?)`): upstream передаёт `(get system <module-key>)`, содержимое зависит от модулей; не проверял.
- `http/*`, `pg/*` externs — доверенные, с upstream-кодом не сверял.
- Взаимодействие `t/ignore`/`t/cast` с телами macro contract проверено только на одном примере.
- `--strict-nil` вместе с generic-вызовами и platform-типами — только один случай (`nil <: a`, `a <: ^String`).
- Производительность базового `t/sub?` на широких схемах (у него свой предел глубины, но не размера) не измерял.
