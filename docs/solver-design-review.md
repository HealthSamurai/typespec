# Review: минимальный rank-1 solver

Проверенная база: `7494c1ec146507cc2bd32604d5bb08842f693df6`. Только дизайн, source не менялся, тесты ниже — спецификация, не результаты запуска. Согласовано с `solver-contract.md`. Изучены `types.clj`, `check.clj`, `env.clj`, `externs.clj`, F230/F220. В F230 исторические разделы устарели: сравнение функций уже структурное, глубина sub? уже не даёт true; опираться на исходники и обновление карточки.

## 1. Контракт и граница MVP

Поддержать явное `(t/all [a b] [a [a :-> b] :-> b])`, также binder `(a :< number?)` согласно reference. Только внешний all над одной фиксированной арностью; callback мономорфный, также одна фиксированная арность. Типы: существующие primitives/literals, immutable `:coll`, вложенный фиксированный `:fn`, nil/never/unknown/any с прежней политикой доверия. В bounds только закрытый тип без binders. Named refs допустимы после ограниченного раскрытия, если их содержимое попадает в этот фрагмент.

Не обобщать let автоматически. Схему именованной функции можно сохранить через `(let [f same] ...)`: это перенос существующей схемы, не generalization. Каждый вызов f инстанцируется заново. Полиморфную функцию как callback в MVP явно отклонять как unsupported, даже если возможна rank-1 специализация: это отдельное расширение, не смешивать с требованием higher-rank.

## 2. Представление и чистое ядро

Предлагаемый новый `typespec.solver` не пишет диагностики, не читает AST и не меняет env/`*rets*`:

- `{:t :all :binders [{:id binder-id :name 'a :upper type}] :body fn-type}`;
- bound occurrence `{:t :bound :id binder-id}`;
- call metavariable `{:t :meta :id fresh-id}`;
- body skolem `{:t :rigid :id fresh-id :upper closed-type}`.

Идентификатор, а не имя, задаёт равенство. Вызов получает собственный supply/state, skolemization тела — отдельный. Схема в env неизменна. Сравнение двух схем для конфликта деклараций — alpha-normalization, не сравнение случайных IDs через `=`.

Минимальные чистые операции: `instantiate`, `skolemize`, `subst`, `constrain`, `solve`. Результат: `{:status :proved|:mismatch|:inconclusive :subst ... :type ... :reasons ... :constraints ...}`. Ограничение хранит обе стороны и origin (позиция, номер аргумента, путь `[:param 0]`/`:ret`, bound). Подстановка идёт по явному перечню полей типов, а не через произвольный postwalk: нельзя заменять имена/данные `:lit`, metadata или ключи map. Проверять отсутствие свободных meta/bound в возвращаемом типе; rigid внешнего проверяемого тела может оставаться локально, но не экспортируется в env.

## 3. Ограничения и решение

1. На вызов открыть all свежими meta. Добавить `actual <: formal` и, если известен ожидаемый результат, `result <: expected`. Объявленные bounds становятся upper constraints.
2. Разложение: `coll S <: coll T` даёт `S <: T` при совместимом kind; `fn S <: fn T` даёт `T.params[i] <: S.params[i]` и `S.ret <: T.ret`. Проверять арность до разложения. Для `L <: α` записать lower, для `α <: U` upper; `α <: β` — направленное ребро, не равенство.
3. Сначала ограничения обычных аргументов независимо от порядка записи callback. Поддержать DAG зависимостей переменных: распространять lower вперёд, upper назад до ограниченного fixed point. `α <: α` убрать. Иные циклы и occurs check (`α` внутри собственного кандидата) в MVP дают inconclusive, не успешное решение.
4. Ground lower literals расширять до примитивов. Если lower есть, кандидат — их нормализованный union; при одном lower он сам. Это допустимый, но не обязательно полный алгоритм. Любой кандидат проверяется против КАЖДОГО upper и исходного ограничения после полной подстановки. Union здесь — способ выразить join, не лицензия выбирать удобную ветку union формального типа.
5. Если lower нет: можно выбрать единственный наиболее узкий upper, который доказанно subtype всех остальных, только когда upper пришёл из реального контекста/аргумента. Один декларативный bound не является свидетельством типа результата. Нет свидетельств или несравнимые upper — inconclusive. Пустая коллекция с lower never не превращается произвольно в int: разрешить never, если он реально выведен из пустого источника; отсутствие lower не равно lower never.
6. Снова проверить все исходные constraints через `sub-result`, после удаления всех meta. Mismatch — ошибка; unknown, recursion/depth budget и unsupported — inconclusive. Не превращать inconclusive в passing overload. `any?` остаётся явной границей доверия: сохранять bypass/учёт trust, не заявлять доказанную параметричность для такого пути. Unknown нельзя тихо заменить конкретным решением; безопасный выход unknown всё равно не означает доказанную связь типов.

Для MVP не разбирать disjunction с unresolved meta в формальном union и не искать backtracking-решение перегрузок. Закрытые сравнения union уже умеет `sub-result`. Эти ограничения лучше оставить явными, чем обещать главный/наиболее общий тип.

## 4. Callback и rigid тело

`call-fn` сейчас сразу infer-ит все аргументы, поэтому inline fn теряет контекст. Generic-путь должен отложить inline `(fn ...)` и `#(...)`. Собрать обычные аргументы, частично решить только нужные входные meta, подставить их в ожидаемый тип callback, затем проверить lambda ОДИН раз. Не фиксировать ещё не ограниченные выходные meta: тело lambda добавит для них lower. Если вход callback всё ещё не определён, вернуть explicit incomplete, а не угадывать его по операциям тела.

Ожидаемый тип — полный fn, не только arg-types: проверять арность, параметры, вывод результата и отношение к expected ret. Для `(fn [x] ...)` существующий `infer-fn` полезен для связывания параметров, но его нынешнего интерфейса недостаточно. Ввести обёртку `check-fn-against` или расширить интерфейс явно; сохранить старый путь вызова special rules. Callback-symbol с мономорфной сигнатурой проверять структурно с контравариантностью, не подменять проверку единичным удобным вызовом. Lambda, уже выведенная без контекста в let, не получает контекст задним числом: ей нужна аннотация, либо incomplete.

Для проверки defn заменить binders rigid и проверить тело один раз вне связи с вызовами. Равный rigid совместим с собой; rigid <: T можно доказать через declared upper; T <: rigid нельзя доказывать через upper (кроме never и существующего явного any-bypass). `a <: number?` позволяет использовать a как число, но результат inc типа number/int нельзя объявить тем же a. Мономорфное тело не должно решать rigid как meta. Вызов generic identity внутри generic тела допустим: свежая meta может решиться внешним rigid; наружу в env это решение не сохраняется.

## 5. Точные места интеграции

| Место на указанном HEAD | Минимальное изменение |
|---|---|
| `types/parse-type` :symbol и :seq (361+) | Lexical binder scope до class resolution; `:t/all`, проверки duplicate/free binder, closed bound, только outer all. Неподдержанное не считать успешно разобранным unknown. |
| `types/parse-sig` (348+) | Общий вход signature принимает all и прежние vector/list формы. Не допустить взаимной рекурсии parse-type/parse-sig на all без потребления формы. |
| `types/show` (259+), `sub1?`, `sub-result` (241+) | Печать all/rigid без случайных IDs; rigid правила до generic fallback. Meta в обычном subtype — inconclusive/internal invariant, не mismatch по неизвестному тегу. Не делать subtype схем через открытие одной и той же mutable переменной. |
| `env/collect-file` (121+), `env/build` (162+) | Сохранить all целиком; :complete? сегодня истинно при любом declared — для новой схемы нужна валидность и закрытость, не сам факт наличия. Не устанавливать malformed all как успешный контракт. Сравнение деклараций учитывать alpha-equivalence. |
| `externs/load-contracts` (26+) | Symbols сейчас идут прямо в parse-sig: дать им ту же грамматику all, что t/sig. Оставить inert EDN, политику core override и атомарное отклонение malformed entry. |
| `check/var-type` (481+) | Схемы возвращать неизменными ДО inference/cache shortcuts. `*rets*` не хранит инстанциацию. Audit прямых чтений :arities: у all арности лежат в body. |
| `check/infer-list` (1215+, local branch 1227+) | Сейчас callable-local определяется `(seq (:arities ft))`: local all иначе молча даст unknown. Общий callable predicate для fn/all. |
| `check/call-fn` (402+) | Generic branch до eager infer; staging callback; передача expected result только по явному check-expression пути. Старую monomorphic overload/nil политику не переносить автоматически в solver. |
| `check/infer-fn` (562+), `apply-fn-node` (606+) | Expected callback contract, арность и ret; #() тоже. Сейчас direct fn возвращает union ret всех арностей: это не общий механизм проверки. Обойти его новой общей проверкой в поддержанном случае. |
| `check/apply-type` (578+) | То же чистое invoke/solve ядро для уже известных argument types; свежесть на каждый вызов, не отдельный алгоритм generics. AST/contextual inference сюда не помещать. |
| `check/check-defn` (1163+), `check-return!` (1145+) | Skolemize all до поиска :arities; параметры и expected return — rigid. Прокинуть expected return к последнему выражению через check-expression; infer остаётся без контекста по умолчанию. |

Диагностики/record!/trust нельзя выпускать при пробном inference дважды. С одной generic арностью проба вообще не нужна; pure solver возвращает данные, checker один раз публикует диагностику. Для неполного контекста inline fn допустим отдельный один диагностический проход с unknown и явным incomplete, не два последовательных infer.

## 6. Восемь конкретных acceptance-тестов

Сниппеты предполагают обычный parsed fixture namespace с alias s. Проверять не только отсутствие диагностики, но итоговые recorded types, статус solver и неизменность исходной схемы. Новая диагностика F230 пока proposed: не фиксировать несуществующие коды как работающие.

1. **Независимость вызовов и let alias.** `(defn same {:t/sig '(t/all [a] [a :-> a])} [x] x)`, затем `(let [f same] [(f 1) (f "s") (f 2)])`. Типы отдельных вызовов int/string/int, без ошибок. Повторить в обратном порядке и в новом check-run. Проверить, что env schema не изменилась, fresh meta IDs различны и в результатах их нет. Ловит global substitution, var-type cache и local :arities shortcut.
2. **Generic extern.** Контракт EDN `{ext/same (t/all [a] [a :-> a])}`, два вызова с int/string. Те же результаты, нет TSP9003. Дополнительные malformed entries `(t/all [a a] [a :-> a])` и `(t/all [a] [b :-> a])` отклонены и не установлены. Ловит parse-sig bypass и неявный unknown свободной переменной.
3. **Тело-константа.** `(defn bad {:t/sig '(t/all [a] [a :-> a])} [x] 42)`. Ошибка универсального результата даже БЕЗ вызовов; `(bad 1)` не должен сделать тело допустимым. `(defn same ... [x] x)` — положительный контроль.
4. **Bound не равен типу.** `(defn bad-n {:t/sig '(t/all [(a :< number?)] [a :-> a])} [x] (inc x))`. Ошибка возврата concrete number в rigid a; `(defn good-n {:t/sig '(t/all [(a :< number?)] [a :-> number?])} [x] x)` допустима. `(good-n "s")` отвергается, `(good-n 1)` принимается. Ловит неправильное направление rigid subtyping и игнорирование upper.
5. **Вход callback от более позднего аргумента.** `(defn run-one {:t/sig '(t/all [a b] [[a :-> b] a :-> b])} [f x] (f x))`; `(run-one (fn [n] (inc n)) 1)` даёт числовой результат и без unknown/incomplete. Повторить с `#(inc %)`; `(run-one (fn [n] (inc n)) "s")` даёт одну ошибку внутри callback, не две. Ловит eager infer и зависимость от порядка аргументов.
6. **Неверный выход callback и expected result.** `(defn need-int {:t/sig '[:-> int?]} [] (run-one (fn [n] "oops") 1))`. Lower b=string и upper b=int конфликтуют; нельзя принять, сменив a, проигнорировав return context или вернув unknown без incomplete. Контроль: `(fn [n] n)` вместо строки проходит.
7. **Контравариантность named callback.** Extern `{ext/wide [number? :-> string?], ext/narrow [int? :-> string?]}`; generic `run-one` из №5. В wrapper с x:int `(run-one ext/wide x)` проходит и возвращает string; в wrapper с x:number `(run-one ext/narrow x)` отвергается. Все проверки через declared types, не числовой literal, чтобы действительно проверить variance.
8. **Нельзя доказать неизвестное или занять старое решение.** Extern `{ext/make (t/all [a] [:-> a]), ext/opaque [:-> t/unknown]}`. После `(same 1)` выражение `(ext/make)` без expected типа даёт inconclusive, не int/never/any. `(same (ext/opaque))` не получает :proved с concrete int, результат безопасно unknown/incomplete; в wrapper, обещающем int, обязательно incomplete/error. Повторить после `(same "s")`: результат не меняется. Ловит отсутствие lower, unknown-to-proof и утечку cache.

При реализации добавить unit assertions для typed subst (вложенный fn/coll), occurs/depth budget и повторной валидации каждого исходного constraint. Это детали указанных acceptance-сценариев и ядра, не утверждение о выполненных тестах. Подключение нового test namespace проверить в `test/typespec/demo_test.clj`: alias `clojure -M:test` запускает этот runner, а не автоматически любой новый файл.

## 7. Что намеренно не входит

Higher-rank/nested all, impredicativity, автоматическая let-generalization, polymorphic recursion, recursive/dependent bounds, row polymorphism и generic maps, mutable variance, generic overload/rest/kwargs, поиск решений disjunction, неограниченные циклы constraints, вывод через уже сохранённый неаннотированный callback, универсальные comp/partial/transducer contracts. Неизвестное/лимит/unsupported — неполная проверка, не успех. Старые специальные правила core остаются; их постепенный перевод на solver — отдельная работа после восьми тестов.
