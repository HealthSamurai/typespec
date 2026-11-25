# Аудит 13 контрактов clojure.string

Проверено по Clojure **1.12.4**, локально на Java **25.0.2**. Ниже сохранён первоначальный аудит: Current означает состояние до реализации. Предложенные контракты внедрены; единственный источник сигнатур теперь `resources/typespec/types/clojure/string.edn`, а `src/typespec/core_types.clj` загружает EDN через classpath. символьные литералы теперь распознаются как Character. Это ограниченный контракт, а не обещание описать все случайно работающие значения JVM.

## Результат реализации и проверок

- Новый `string_contracts_test.clj`: 5 тестов / 105 assertions, включая runtime-пробы настоящего Clojure, положительные и отрицательные проверки checker, Character roundtrip, обе формы конструктора Object.
- Общий `clojure -M:test`: 93 теста / 576 assertions плюс golden/CLI — без ошибок. В двух demo-эталонах изменён только ожидаемый тип одного предупреждения: String → CharSequence.
- Web+PG пример: статически 0 ошибок / 0 предупреждений, unit 3/22 PASS. HTTP/Postgres integration в этом этапе повторно не запускался.
- Остаются ограничения: JVM arrays не входят в bounded join-контракт; quoted list может давать incomplete; нет специализации callback по groupCount regex; диапазон split limit не проверяется. Unknown-параметр callback не выдаётся за доказанный тип.
- Найденный сопутствующий дефект исправлен: конкретные `(Object.)` и `(new Object)` больше не превращаются в any. Объявленный Object/top глобально не менялся.



## Источники и воспроизведение

GitHub API: [tag ref](https://api.github.com/repos/clojure/clojure/git/ref/tags/clojure-1.12.4) → annotated tag `8f943defd009573d6ef448c2621aa30dfe658c2a` → [tag object](https://api.github.com/repos/clojure/clojure/git/tags/8f943defd009573d6ef448c2621aa30dfe658c2a) → commit **`b4ea0f824b2eea039dfc06b796ed601e35cbeab6`**. Tag date: 2025-12-10T18:25:37Z; unsigned. API получен через curl, исходник тега побайтно совпал с исходником commit.

Pinned первоисточники:
- [string.clj](https://github.com/clojure/clojure/blob/b4ea0f824b2eea039dfc06b796ed601e35cbeab6/src/clj/clojure/string.clj): reverse L48, replace-by L62, replace L75, join L180, capitalize L196, upper-case L206, lower-case L213, split L220, split-lines L231, trim L237, blank? L288; starts/ends/includes — конец файла.
- [RT.java: seqFrom](https://github.com/clojure/clojure/blob/b4ea0f824b2eea039dfc06b796ed601e35cbeab6/src/jvm/clojure/lang/RT.java#L550-L578): действительная область seq.
- [core.clj: re-groups](https://github.com/clojure/clojure/blob/b4ea0f824b2eea039dfc06b796ed601e35cbeab6/src/clj/clojure/core.clj): строка без capturing groups, vector при наличии групп; unmatched group → nil.
- Локальные `src/typespec/core_types.clj:58–70`, `types.clj:50–105,377–387,429–480`, `parse.clj` и `check.clj` (regex → Pattern).

Эксперимент и полный вывод сохранены только в scratch агента: `../.hyper/tmp/cemk/audit.clj`, `../.hyper/tmp/cemk/results.txt` относительно typespec. Команда: `JAVA_HOME=/opt/homebrew/opt/openjdk@25 clojure -M ../.hyper/tmp/cemk/audit.clj` из typespec. Это runtime-пробы и проверки разбора типов, **не** добавленные регрессионные тесты checker.

## Таблица

Обозначения: S = `string?`, CS = `java.lang.CharSequence`, P = `java.util.regex.Pattern`, C = `java.lang.Character`, V = `(s/coll-of string? :kind vector?)`. Сокращения только для документа, в externs нужны полные формы. `?` после имени типа здесь означает nilable. Все результаты S/boolean/V не nil.

| Функция | Actual 1.12.4 | Current | Proposed | Runtime-пробы / будущие regression tests | Ограничения |
|---|---|---|---|---|---|
| blank? | CS или nil → boolean; false также возвращает true из-за `if` | S? → boolean | `(s/nilable java.lang.CharSequence)` → boolean | nil=true; StringBuilder работает; 42 throws; false=true | false намеренно вне поддержанного контракта; текущий parser `false?` превращает в весь boolean, не literal false |
| join | `(seq coll)`; любой separator через str; элементы через str | any → S; any any → S | Q → S; `t/unknown Q` → S (Q ниже) | nil→""; string, StringBuilder, vector, map, set, ArrayList, int-array работают; 42 throws; map-separator работает | Нельзя подменять seqable на any или только coll; массивы требуют отдельного решения |
| split | CS P [limit] → V; limit конвертируется в JVM int | S any [int] → V | CS P [int?] → V | builder работает; string/nil pattern throws; nil input throws; -1 сохраняет trailing empty; 2147483648 throws; 1.5 принимается как 1 | int? ограниченнее фактической coercion; не гарантирует диапазон signed int |
| trim | CS → S, Character/isWhitespace | S → S | CS → S | builder " Ab\\n"→"Ab"; nil/42/false throws | Не Java String.trim; Unicode и mutable CS — runtime-семантика |
| lower-case | CS → S по интерфейсному контракту; фактически `.toString` допускает и 42/false | S → S | CS → S | builder работает; nil throws; 42→"42", false→"false" | Не расширять до any: произвольные Object — случайная широта реализации; default locale |
| upper-case | То же | S → S | CS → S | builder работает; nil throws; false→"FALSE" | Та же сознательная граница; default locale |
| starts-with? | CS S → boolean; receiver через toString | S S → boolean | CS S → boolean | builder receiver работает; builder needle throws; оба nil положения throws; 42 receiver работает | Второй аргумент именно String; произвольный receiver исключён |
| ends-with? | CS S → boolean; receiver через toString | S S → boolean | CS S → boolean | Те же типовые случаи | То же |
| includes? | CS CS → boolean; receiver через toString | S S → boolean | CS CS → boolean | builder и слева, и справа работают; оба nil положения throws | Не копировать String-only needle из starts/ends |
| replace | Ненулевой первый аргумент + одна из связанных пар C/C, CS/CS, P/CS, P/callback → S | S any any → S | Четыре overloads, см. ниже | Первый аргумент преобразуется через `.toString`. Допустимы keyword и число. `nil` в первом аргументе вызывает NPE. | Связь match/replacement обязательна; независимые union аргументов неверны |
| capitalize | CS → S; receiver через toString | S → S | CS → S | builder работает; nil throws; 42/false тоже работают | Сознательно не обещаем arbitrary Object; locale |
| reverse | CS → S, StringBuilder.reverse | S → S | CS → S | builder работает; nil/42/false throws | Не обещать reverse графем: JVM UTF-16/surrogate semantics |
| split-lines | CS → V, split по `#"\\r?\\n"` | S → V | CS → V | builder " Ab\\n"→[" Ab"]; nil/42/false throws | Не все Unicode line separators; trailing empty исчезают |

## join: честная граница без any

Практический выразимый тип Q:

```clojure
(t/or nil? java.lang.CharSequence clojure.lang.Seqable
      java.lang.Iterable java.util.Map)
```

Это покрывает обычные Clojure collections, Java Iterable/Map и CS, но **не все массивы JVM**. RT.seqFrom принимает также любой массив, включая primitive arrays. Нельзя утверждать, что один `clojure.lang.Seqable` покрывает всю область `seq`. Не добавлять произвольный Object: `class-type java.lang.Object` уже превращается в any. `seqable?` сейчас не разбирается как тип. Для полного покрытия нужен отдельный `seqable`/array capability либо точная проверка известных class types массивов; неизвестный класс должен дать unsupported/inconclusive, не успех.

Separator и элементы не требуют string?: runtime применяет str к любому значению, включая nil. Для separator честнее `t/unknown` (без права вызывать на нём что угодно), а не any-escape. Для элементов параметр вообще не нужен: Q проверяет способность быть последовательностью, не содержимое. Бесконечный seq может не завершиться; это не решается типом. Проверить интеграционно в checker, что unknown как **ожидаемый** верхний тип не создаёт ложной ошибки; если создаёт, это ограничение checker, а не основание принимать произвольный coll.

## replace: связанные overloads и callback

Предлагаемые формы (CS/P/C должны быть раскрыты в полные имена):

```clojure
([CS C C :-> string?]
 [CS CS CS :-> string?]
 [CS P CS :-> string?]
 [CS P [(t/or string?
               (s/coll-of (s/nilable string?) :kind vector?))
         :-> string?] :-> string?])
```

Callback получает String, если regex не содержит capturing groups; иначе vector `[full-match group1 ... groupN]`, где optional unmatched groups — nil, full-match всегда String. Например `(replace "b" #"(a)?b" pr-str)` → `"[\"b\" nil]"`. Return callback **строго String**, не произвольный CS: StringBuilder даёт ClassCastException, nil — NullPointerException в Matcher/quoteReplacement. Callback text экранируется, `$` и `\\` литеральные; P/CS replacement использует Java regex substitution (`$1` и т.п.), может бросить исключение для неверных ссылок.

Общая callback-сигнатура безопасна, но груба: vector не пуст, его первая позиция не nil; `s/tuple` сейчас схлопывается в homogeneous vector и не сохраняет эту связь. Обычный `(fn [s] (upper-case s))` допустим для regex без групп, но не для неизвестного P. Для точного принятия нужен special rule для **regex literal**: определить groupCount, контекстно задать callback input; для динамического Pattern оставить union или явный unsupported. Не угадывать группы подсчётом скобок. Более узкий callback нельзя молча считать принимающим весь union.

Недопустимый callback вроде 42 иногда проходит runtime, если совпадений нет; это не безопасный контракт и не причина разрешать any. Class Character выразим, но char literals в source AST отсутствуют как полноценный тип (fallback, lit-prim относит прочие литералы к symbol); `char?` type parser отвергает. Поэтому char/char ветка описывает цель, а не уже работающую проверку исходников: до устранения char gap нужен явный unsupported/diagnostic, не обещание покрытия.

## Что уже выразимо, а что требует правила

- Полные Java class names разбираются и используют isAssignableFrom; String/StringBuilder являются CS. CS, Pattern, Character, nilable CS и callback union проверены через `t/parse-type` локально. Regex AST уже даёт Pattern: для split отдельный regex special rule не нужен.
- Overload-list поддерживается parse-sig. Внедрение replace требует regression tests выбора overloads и проверки callback body; одних parse-type проб недостаточно.
- Не использовать `s/and` для несуществующих refinement guarantees: parser берёт только первый operand. Не использовать `false?` как singleton: он сейчас boolean. Не объявлять seqable?/char? как будто они уже поддержаны.
- Диапазон split limit, наличие capturing groups, точные позиции callback groups и массивы join не выражаются обычной текущей сигнатурой без потерь. Либо честное ограничение, либо отдельное маленькое правило с тестами; не вторая таблица сигнатур и не новый standard-pack.
- Глобальное расширение типов до any не исправляет ни одну из найденных дыр. Сначала CS/Pattern/связанные overloads; затем явно перечисленные ограничения.
