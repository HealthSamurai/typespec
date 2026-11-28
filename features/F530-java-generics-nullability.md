---
id: F530
title: Java generics и nullability
status: designed
decision: accepted
implementation: partial
area: java
origin: Clojure
---

# F530 · Java generics и nullability

## Зачем

Clojure-код постоянно пересекает границу JVM: читает Java-коллекции, вызывает библиотеки, реализует интерфейсы. Знать только erased-класс `Object` недостаточно: теряется тип элемента, а наличие `^String` не гарантирует, что значение не равно `nil`. Цель — сохранить параметры Java-типов и отделить доказанный non-null от доверия к внешнему контракту. Это позволяет обнаруживать неправильную запись в коллекцию и разыменование возможного `null` до исполнения.

Это карточка механизма generic signatures + nullability, а не повтор алгоритма выбора перегрузки F520. Она задаёт информацию, которую кандидаты перегрузок должны получать и возвращать; политика `strict-nil` согласуется с F320.

## Что действительно работает сейчас

Для наследуемого публичного Java-метода чекер может восстановить номинальный
тип результата, если он объявлен как параметр класса или интерфейса.
Подстановка проходит по `getGenericSuperclass` и `getGenericInterfaces`.
Например, `StorageOptions$Builder` подставляется вместо `B` в
`ServiceOptions$Builder.setCredentials`. Следующий `.build()` проверяется
на `StorageOptions$Builder`.

Для Java-метода, возвращающего `Iterator<T>` или `Enumeration<T>`,
`iterator-seq`, `enumeration-seq`, `.next()` и `.nextElement()`
используют известный класс элемента.
Подстановка через наследование и верхняя граница `? extends T` учитываются.
Например, `JarFile.entries()` даёт `JarEntry`, а `ZipFile.entries()`
даёт `ZipEntry`. Элемент из Java сохраняет platform-nullability.
Для `Iterable<T>`, `Collection<T>` и известных интерфейсов коллекций
тот же тип элемента используется в `for`, `doseq` и колбэках.
Например, `FileSystem.getRootDirectories()` даёт элементы `Path`,
а `supportedFileAttributeViews()` — строки.
Вызов `.iterator()` на таком `Iterable` переносит тип элемента в
`.next()` и `iterator-seq`.
Для `List<T>` известный тип элемента также используется как результат
`.get(int)`, например у списка строк из `Files.readAllLines()`.
`listIterator()` и `listIterator(int)` сохраняют его для `.next()`,
`.previous()` и `iterator-seq`.
Java `Stream<T>` тоже передаёт тип элемента через `.iterator()`.
Например, итератор от `Files.lines()` даёт строки.
`Iterable.forEach` использует известный тип элемента в параметре
переданной функции.
`Stream.filter` использует его при проверке функции-предиката
и сохраняет в возвращаемом потоке.
`Collection.stream()` и `.parallelStream()` переносят тип элемента
из коллекции в поток.
Для возвращаемого `Map<K,V>` и известных интерфейсов карт сохраняются
классы ключа и значения. `.get(Object)` использует класс значения и
учитывает возможный `nil`. Это работает, например, с `System.getenv()`
через функцию из другого namespace и совместимый hint. `.values()`
передаёт тип значения в Java-итератор и Clojure-колбэки коллекции,
а `.keySet()` передаёт тип ключа.

- `java/methods-named` сохраняет стёртые параметры перегрузок и использует
  `getGenericReturnType` для подстановки результата. При совпадающих
  сигнатурах параметров предпочитаются обычные методы перед bridge и
  synthetic методами, затем более конкретные тип результата и владелец.
- При raw-наследовании, неизвестной переменной или недоступной metadata
  используется стёртый тип результата. Подстановка сохраняет класс
  `List` для результата `List<T>`. Вложенный `T`, generic-параметры
  вызова и ограничения параметров метода остаются за рамками реализации.
- В `types/parse-type` ветка `t/class` разрешает только имя класса. Аргументы после имени **игнорируются**: `(t/class java.util.List String)` пока не сохраняет `String`. Само принятие такой формы не означает проверку generics.
- `check/java-ret` обрабатывает небольшой список `java/nullable-return?`: например, `Map.get`, `Map.remove`, `Queue.poll` и перегрузку `System.getenv(String)`. Перегрузка `System.getenv()` возвращает карту с обычной platform-nullability. Для остальных подходящих результатов используется platform type; `types/platform` не делает примитивы nullable. Это не чтение JSpecify или иных аннотаций.
- `check/eff` при `:strict-nil` превращает platform type в объединение с `nil`; `method-call` выдаёт `TSP2531` при nullable receiver. Метаданные `:null-return` сохраняют причину для известных nullable-методов, в том числе через hint.
- `java-arg-ok?` для ссылочных параметров удаляет `nil` перед проверкой совместимости. Полноценной проверки non-null **аргументов** Java-методов нет.

Локальный Java-fixture проверяет наследование через классы и интерфейсы,
raw fallback, bridge-метод, отсутствующие методы и сохранение nil-политики.
На get-ig приёмка воспроизводит исправленную цепочку на закреплённом GCS
classpath. [Тесты](../test/typespec/java_generic_return_test.clj) · [Прогон get-ig](../docs/fhir-registry-result.md#перепроверка-после-f530)

## Примеры и ожидаемое поведение

Обычные блоки ниже — спецификация и иллюстрации, не golden-тесты. `t/class` используется внутри quoted `:t/sig`, а не как исполняемый вызов.

### 1. Current: явная проверка результата известного nullable-метода

```clojure
(defn home-size []
  (let [home (System/getenv "HOME")]
    (if (nil? home)
      0
      (.length ^String home))))
```

Сейчас `System.getenv` входит в nullable-таблицу; ветка после `nil?` исключает `nil`, поэтому разыменование не должно давать `TSP2531`. Ожидание касается именно nullability, не обещает отсутствия всех остальных предупреждений. Planned: тот же результат независимо от режима platform types — известная nullable-сигнатура не становится non-null в обычном режиме.

### 2. Current: hint не отменяет известный null

```clojure
(defn label-size [^java.util.Map labels]
  (.length ^String (.get labels "title")))
```

В обычном режиме сейчас ожидается `TSP2531` на результате `.get`, с причиной `java.util.Map.get may return null`. Такой же случай уже зафиксирован в `test/examples/src/ex/more.clj` и `test/examples/expected.txt`. Hint помогает определить вызываемый класс, но не доказывает наличие записи. При `strict-nil` появляется также вопрос nullable receiver `labels`: hint сам по себе не гарантирует его non-null.

### 3. Planned: перенос V через Map.get, с учётом отсутствующего ключа

```clojure
(defn find-items
  {:t/sig '[(t/class java.util.Map String
                     (t/class java.util.List java.util.UUID))
            :-> (s/nilable (t/class java.util.List java.util.UUID))]}
  [m]
  (.get m "missing"))
```

Целевой тип результата — nullable `List<UUID>`: подстановка сохраняет вложенный параметр, а модель `Map.get` добавляет возможность отсутствия ключа. Current: параметры `t/class` теряются, результат `.get` строится из erased-типа; такой вывод пока не поддержан. Reference §4.3 показывает platform-результат для `.get`; здесь уточняется приоритет известного nullable-контракта над общим fallback platform. Это проектное уточнение, не утверждение, что reference уже исправлен.

Важно: параметр Java `Map.get` — `Object`, а не `K`. Нельзя автоматически запрещать ключ другого типа лишь потому, что receiver имеет `Map<String,V>`; runtime такой поиск допускает. Типизированный контракт ключа потребовал бы отдельно заявленной более строгой политики.

### 4. Planned: ошибка записи и инвариантность Java List

```clojure
(defn append-wrong
  {:t/sig '[(t/class java.util.List String) :-> boolean?]}
  [xs]
  (.add xs 42))

(defn as-objects
  {:t/sig '[(t/class java.util.List String)
            :-> (t/class java.util.List Object)]}
  [xs]
  xs)
```

Planned: первый вызов отклоняется после подстановки `E := String`; второй — потому что mutable `List<String>` не является `List<Object>`. Иначе через расширенный alias можно было бы добавить число в список строк. Current: обе проверки generic-параметров отсутствуют; нельзя использовать отсутствие диагностики как доказательство безопасности. Ковариантность immutable Clojure-коллекций из reference не переносится автоматически на Java List.

### 5. Граница: platform receiver — допущение, не доказательство

```clojure
(defn unsafe-size [^String s]
  (.length s))

(defn guarded-size [^String s]
  (if (nil? s) 0 (.length s)))
```

Current: в обычном режиме platform hint допускает первый вызов; с `strict-nil` ожидается `TSP2531`. Второй вариант исключает nil перед вызовом и подходит обоим режимам. Planned сохраняет это различие для **неаннотированного** ссылочного типа. Явный доверенный non-null контракт не должен становиться nullable лишь из-за включения strict-nil. Проверка не делает первый вариант безопасным при фактическом `nil` в runtime.

## Целевой алгоритм: минимальное ядро и исследовательские расширения

Ниже — план реализации и обязательств проверки, **не доказанный sound алгоритм всей Java type system**. Начальный проверяемый поднабор: конкретные аргументы класса, подстановка по наследованию и явная nullability. Wildcards, generic methods и recursive bounds требуют отдельной формализации и тестирования до заявления поддержки.

1. **Собрать входы.** AST вызова и позиции receiver/аргументов; их выведенные типы; classpath и версии классов; generic и annotated signatures кандидатов; конфигурацию доверенных аннотаций, overrides и `strict-nil`. Не исполнять методы или инициализаторы проекта. Сохранить идентичность метода по declaring class + имени + erased descriptor, иначе мостовые методы и перегрузки смешиваются.
2. **Прочитать декларацию без преждевременного стирания.** Кроме erased-сигнатуры получить параметры класса/метода, generic superclasses/interfaces, bounds, вложенные аргументы и позиции аннотаций. `java.lang.reflect.Type` описывает форму типа; `AnnotatedType` — аннотации конкретных вложенных позиций. Reflection не видит все аннотации с невидимым runtime retention: для обещанной поддержки нужен анализ classfile либо явно отмеченный пробел. Чтение defaults package/class также должно быть явным.
3. **Построить IR.** Расширить нынешний `{:t :class :name ...}` аргументами; добавить переменные с идентичностью владельца, а не одним именем `T`. Для каждого узла хранить состояние nullability (`non-null`, `nullable`, `unspecified/platform`) и происхождение. Nullability контейнера и элемента независимы. Bounds, wildcards и captures требуют отдельных узлов, их нельзя представлять как `any?` с видимостью успешной проверки. Это проектная структура, не уже существующий формат.
4. **Согласовать источники.** Для одной позиции сначала применить явно настроенный override с записанным происхождением; затем поддержанную явную аннотацию, затем применимый default, затем модель известного метода, наконец platform fallback. Explicit Nullable сильнее package default NonNull. Конфликт независимых явных аннотаций или override с контрактом должен быть виден, а не молча выбран в пользу non-null. Это предлагаемая политика: её нужно утвердить, особенно для JDK-моделей. Примитивы не получают nullable; противоречивая аннотация требует диагностики.
5. **Подставить параметры receiver.** Найти declaring class кандидата через generic-граф наследования и построить substitution, например `Child<String> → Parent<List<String>>`. Подставить её в параметры и результат, включая вложенные позиции и nullability. Raw receiver не позволяет восстановить потерянный аргумент: результат пометить непроверенным. Не придумывать параметр по первому использованию. Рекурсивный обход требует обнаружения циклов и лимита сложности с явным результатом «не проверено».
6. **Проверить конкретный кандидат.** F520 отвечает за применимость и выбор перегрузки; F530 поставляет instantiated-сигнатуру и ограничения. Java-параметризованные mutable-типы проверяются инвариантно; ссылочный nullable-аргумент не подходит доказанному non-null параметру. Platform в обычном режиме остаётся зарегистрированным допущением, в strict-nil требует исключить nil. Boxing, widening и varargs нельзя заменять общей проверкой «оба числа»: это отдельная ответственность interop. Не стирать generic-информацию перед выбором.
7. **Ограничить сложный вывод.** Для generic method variables нужны ограничения от аргументов, bounds и, если используется, ожидаемого результата. Wildcard capture должен создавать свежую переменную для данного использования: `? extends Number` разрешает чтение с верхней границей, но не запись произвольного `Number`; `? super Integer` разрешает соответствующую запись, но не чтение как `Integer`. Nullability проверяется независимо. Capture conversion, intersection/F-bounds и выбор решений пока требуют исследования; до реализации возвращать непроверенный результат, а не принимать вызов по erasure как доказанный.
8. **Сформировать результат и факты.** После успешной подстановки вернуть тип результата с отдельной nullability и provenance. Known nullable return остаётся nullable и без strict-nil. Ветвление по сохранённой локальной переменной может удалить nil; проверка одного `.get` не доказывает non-null следующего `.get` из изменяемой коллекции. Потерянный тип или неподдержанная конструкция должны снижать покрытие проверки.
9. **Выдать диагностики.** Указать позицию, сигнатуру кандидата, путь вложенного аргумента (например, `Map.value/List.element`), ожидаемый/фактический тип и источник nullability. Для обычной несовместимости и nil receiver переиспользовать существующие `TSP2345` / `TSP2322` и `TSP2531`, когда их смысл подходит. Для неподдержанного generic-контракта предлагается **proposed TSP530-01**, для конфликта nullability-источников — **proposed TSP530-02**. Эти коды ещё не реализованы. Ошибка вывода не должна сопровождаться заявлением «вызов проверен».

## Интеграция с прототипом

- `src/typespec/java.clj`: расширить данные `methods-named`, не ломая erased-данные для выбора перегрузок; заменить/дополнить nullable-таблицу моделями с provenance. Сейчас таблица различает owner/name, не descriptor; это слишком грубо для общих overrides.
- `src/typespec/types.clj`: сохранять `t/class` args при parsing, printing, subtyping и substitution. Одного изменения parser недостаточно: нынешнее class subtyping сравнивает классы, не параметры.
- `src/typespec/check.clj`: заменить erasure-only путь `java-call` / `java-ret` на работу с instantiated-кандидатами; убрать безусловное удаление nil там, где параметр требует non-null. `method-call`, `eff` и существующее narrowing использовать как места подключения, проверив сохранение вложенных типов.
- `src/typespec/env.clj`: hints остаются platform-источником, а не свидетельством non-null или generic-параметров. `^java.util.List` сам не задаёт `E`.
- Существующий `test/typespec/demo_test.clj` сравнивает диагностики с fixtures, включая strict-nil; новые fixture-классы и точные утверждения о выводимом типе нужны отдельно. Эти примеры не подключены к автоматическому card-run.

## Ограничения и нерешённые вопросы

- Нужен утверждённый перечень поддерживаемых annotation dialects и версий. Нельзя объявить всю поддержку JSpecify/JetBrains/jakarta/Android только по совпадению простого имени `Nullable`. Внешние спецификации для этой правки не проверялись; соответствие им пока не заявляется.
- Defaults, отмена defaults, аннотации type-use и аннотации на declaration не взаимозаменяемы. Nullability переменной типа требует отдельной семантики; простая операция «приписать nil ко всему T» недостаточна.
- Raw types, unchecked casts, heap pollution и внешняя мутация нарушают generic-контракт во время исполнения. Статический вывод условен относительно доверия к Java-декларациям, он не заменяет проверку содержимого на границе недоверенных данных.
- Наследование аннотаций, override-проверки методов, bridge methods, generic arrays, constructors и SAM требуют согласования с другими interop-фичами; здесь не обещается их готовность.
- Reference иллюстрирует Java externs, но стабильный формат адресации перегрузок и precedence overrides в текущем checker не реализованы этой карточкой. Не следует копировать иллюстрацию extern как работающую конфигурацию.

## Конкретная матрица тестов

Все planned-строки — требования к будущим тестам, не перечень уже пройденных проверок. Для generic-случаев проверять не только отсутствие/наличие ошибок, но и сохранённые аргументы выведенного типа.

| Случай | Ожидаемый результат | Состояние |
|---|---|---|
| `^String`, прямой `.length`, обычный режим / strict-nil | Допущение platform / `TSP2531` | Current, также strict-nil fixtures |
| Локальный результат `getenv`, `nil?`, затем `.length` | Nullable до ветки, non-null внутри ветки | Current по пути checker; нужен отдельный regression |
| `^String (.get labels "title")` | `TSP2531`, причина `Map.get` сохраняется | Current, examples fixture |
| Parse/show `List<String>` против `List<Long>` | Разные IR и отображение, без стирания | Planned |
| Вложенный `Map<String,List<UUID>>.get` | Nullable `List<UUID>`, UUID сохранён | Planned |
| `Map<String,V>.get` с числовым ключом | Не запрещать по одному лишь K: параметр Object | Planned |
| `List<String>.add` со строкой / числом | Принимается / несовместимость String | Planned |
| `List<String>` присваивается `List<Object>` | Отказ по инвариантности | Planned |
| `Child<String> extends Parent<List<T>>` | Метод Parent возвращает `List<String>` | Planned, fixture bytecode |
| Nullable receiver / nullable element | Ошибка разыменования контейнера / независимый nullable элемент | Planned |
| Неаннотированный / явно NonNull результат, оба режима | Platform зависит от режима; NonNull не становится nilable | Planned |
| Nullable аргумент передан в NonNull параметр | Ошибка на аргументе, не только на receiver | Planned |
| Package default + явный Nullable на вложенном аргументе | Явный Nullable сохраняется только в своей позиции | Planned, fixture annotations |
| Override против annotation; две противоречивые annotations | Видимый конфликт с provenance, proposed TSP530-02 | Planned |
| Runtime-invisible type-use annotation | Читается из classfile либо честный unsupported | Planned |
| Raw List, отсутствующая generic signature | Непроверенный параметр, не фиктивный `String` | Planned |
| `? extends Number` / `? super Integer` | Раздельные ограничения чтения и записи либо unsupported | Research / planned |
| Generic method с recursive bound, конфликт constraints | Корректное решение/отказ либо proposed TSP530-01; без зависания | Research / planned |
| Два `.get` с мутацией между ними | Narrowing первого результата не переносится на второй | Planned |

Критерий готовности: положительные, отрицательные и пограничные fixtures для поддержанного поднабора; диагностики с позициями и provenance; явно непроверенные случаи вне него. Наличие только nullable-таблицы или parser-формы `t/class` критерию не соответствует.

## Проверенные локальные источники

- [Reference §1, §3, §4](../docs/reference.md): целевой язык типов, platform/strict-nil, bytecode и аннотации; это дизайн, не перечень реализованного.
- [java.clj](../src/typespec/java.clj): erased reflection и nullable-таблица.
- [types.clj](../src/typespec/types.clj): представление класса, platform, subtyping и parsing `t/class`.
- [check.clj](../src/typespec/check.clj): `eff`, `java-arg-ok?`, `java-ret`, `java-call`, `method-call` и сохранение `:null-return`.
- [env.clj](../src/typespec/env.clj): типы из hints.
- [more.clj](../test/examples/src/ex/more.clj), [expected.txt](../test/examples/expected.txt), [demo_test.clj](../test/typespec/demo_test.clj): существующий nullable-случай и устройство fixture-тестов.
