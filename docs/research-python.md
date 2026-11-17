# Pyright, mypy и typeshed: что перенести в Typespec

Проверено **2026-11-17, около 20:22 UTC**. Цель: статическая проверка существующего Clojure, без добавленных runtime checks. Это исследование Typespec, не Aidbox.

## Summary

Наиболее полезны не Python-синтаксис типов и не классы, а инфраструктура доверия: отдельные декларации библиотек, явное происхождение неполных типов, постепенное включение проверок по namespace, контекстный вывод callback и инвалидация по изменившимся интерфейсам. Первым стоит сделать воспроизводимый каталог externs и различение неизвестности/явного отказа от проверки; затем универсальные HOF и кеш интерфейсов. Полный mypy daemon — слишком дорогая первая цель.

Важная поправка: **Pyright `Unknown` — специальная разновидность `Any`, а не безопасный top type**. Его полезное свойство — возможность сообщить о пробеле в знаниях. Семантику «сначала сузь, потом используй» из Typespec нельзя обосновывать поведением Pyright: это собственное, более строгое решение Typespec.

## Состояние проектов и границы проверки

Метаданные получены непосредственно `curl https://api.github.com/repos/{owner}/{repo}` и `/commits?per_page=1`; stars — снимок, не показатель корректности.

| Проект | Stars | Последний commit default branch, UTC | SHA |
|---|---:|---|---|
| microsoft/pyright | 15 749 | 2026-11-17 17:20:30 | `317abe4ead0fa0cc781d25afa810e3463acb59c4` |
| python/mypy | 20 725 | 2026-11-17 17:00:31 | `7ba681974e1eae6350c0bc0842a0de832a4e5ec5` |
| python/typeshed | 5 157 | 2026-11-17 19:35:49 | `92a2ee3d72bde65012062e6e1f938125b6de69d2` |

Все три не archived. У Pyright последний commit обновляет GitHub Actions, у mypy относится к mypyc/weak references, у typeshed исправляет croniter. Поэтому свежий HEAD показывает обслуживание репозитория, **не** скорость развития нужного нам алгоритма; месячный темп работы и bus factor не измерялись. mypyc — компилятор, не предмет переноса.

Прочитаны README Typespec (включая раздел Try it), reference и релевантные разделы карточек: F130, F210, F220, F230, F231, F711, F720, F730, F810, F830, F840, F910. Каталог features просмотрен для исключения дубликатов. Это не новый аудит исходников Typespec: статус ниже взят из README и карточек. По README уже есть локальный вывод, специализированная поддержка map/filter/reduce, JSON/CLI; generics, externs, namespace cache, daemon и CI trust-budget ещё не работают полностью. Наличие примера в reference не означает реализацию.

В Python-проектах прочитаны документация и указанные ниже участки исходников. Никакие Python/Clojure эксперименты в этой работе не запускались; примеры далее — **предлагаемые acceptance cases**, не результаты тестов. Нет выводов, основанных только на abstract, поисковом сниппете или рекламном пересказе. Источники скачаны с main/master в момент проверки; для воспроизводимости при дальнейшем использовании следует закрепить ссылки на приведённые SHA. Исследованы отдельные механизмы, не весь type evaluator.

## 1. Stubs/typeshed: внешние контракты, но не доказательство реализации

**Что есть в Python.** `.pyi` отделяет типовой интерфейс от runtime-кода. mypy предпочитает найденный stub реализации `.py`; Pyright описывает разрешение через пользовательский `stubPath`, установленные stub packages, inline types и typeshed. Это не простое правило «любой stub сильнее всего»: имеет значение порядок поиска пакетов. Typeshed содержит stdlib и сторонние stubs; stdlib обычно поставляется вместе с checker, сторонние — отдельными `types-*` пакетами. Их версии связывают версию библиотеки и дату выпуска. Даже обновление только stubs может изменить результат проверки.

**Перенос.** Это ровно область **F711 + F130 + F710**, не новая карточка:

1. Читать EDN деклараций без `require`, `eval`, выполнения макросов и пользовательских tagged readers.
2. Фиксировать в lock/manifest координаты библиотеки, диапазон её версий, версию набора деклараций, hash содержимого и версию схемы.
3. Хранить provenance каждой var: встроенный core-контракт, inline/spec, project sidecar, внешний пакет, явный override. Конфликт не скрывать порядком файлов.
4. Проверять арности, существование допустимых конструкторов типов и конфликты до запуска inference. Отсутствующую запись оставлять неизвестной, не превращать в «полный namespace».
5. Собственное тело продолжать проверять против аннотации. Чужой extern отмечать как trusted, а не body-verified.

```clojure
;; Обычный существующий код, никаких runtime checks:
(clojure.string/upper-case 42)
```

При контракте `[string? :-> string?]` из extern должен появиться аргументный type error, с адресом декларации в provenance. После удаления декларации нельзя просто получить зелёный CI: должно вырасти число неизвестных границ.

**Ограничения и цена.** Загрузчик EDN сравнительно недорог; долгосрочная цена — поддержка каталога, совместимость версий и review контрактов. Сигнатуру можно ошибочно написать слишком узкой или широкой. Python `stubtest` импортирует библиотеку и сравнивает runtime introspection со stub; он не доказывает корректность возвращаемого типа. **Не переносить его как обязательный шаг Typespec**: загрузка Clojure namespace может выполнять произвольный код. Статически проверяемые тела и fixtures — подходящий обязательный путь; отдельно запускаемый интеграционный тест не должен становиться частью `check`.

## 2. Unknown/Any: отделить семантику от происхождения

**Наблюдение.** mypy `Any` позволяет произвольные операции и распространяется через обращения и вызовы. Неаннотированные параметры и потерянные generic parameters могут давать implicit Any; `ignore_missing_imports` также способен распространить Any из зависимостей. `object` в mypy не равен Any: для специальных операций нужны дополнительные сведения о типе.

Pyright прямо называет Unknown специальной формой Any и даёт `reportUnknownParameterType`, `reportUnknownArgumentType`, `reportUnknownVariableType` и другие настройки. Можно обнаружить частичную неизвестность, например контейнер с неизвестным элементом. Это учёт blind spots, не гарантия запрета всех небезопасных операций.

**Предложение для F210/F810/F840.** Не вводить третью сущность только ради Python. Оставить `t/unknown` безопасным top, `any?` явным escape hatch, `t/never` пустым типом. Независимо от типа хранить причину потери знания: missing-extern, unsupported-macro, unresolved-java, inference-limit, explicit-any, inherited-any. Иначе одна и та же диагностика не объясняет, что исправлять.

```clojure
;; Если legacy/read-code возвращает t/unknown:
(let [x (legacy/read-code)]
  (inc x))                         ; нельзя доказать number?

(let [x (legacy/read-code)]
  (if (number? x) (inc x) nil))     ; существующая ветка даёт narrowing
```

Второй пример не означает предложение автоматически вставить guard: Typespec только анализирует уже написанный код. Если программа не проверяет значение, checker не должен менять её поведение.

**Цена.** Метаданные происхождения — средняя сложность; распространение через unions, ключи maps, callbacks и кеш требует системной поддержки. Не выводить единственный процент «typed»: отдельно public contracts, checked bodies, unknown leaves, explicit Any и trusted externs. Полный интерфейс может целиком состоять из Any и оставаться слабым.

## 3. Gradual adoption: разные области проверки, один смысл типов

**Что существует.** mypy рекомендует постепенное включение по модулям. Полностью неаннотированное тело обычно не проверяется; `check_untyped_defs` добавляет проверку тела, но не волшебные точные типы входов. `disallow_untyped_defs`, `disallow_untyped_calls`, `disallow_any_generics`, `warn_unused_ignores` решают разные задачи. Pyright имеет off/basic/standard/strict, настройку strict для выбранных путей и отдельные diagnostic rules.

**Для F730/F830/F840.** Включать строгие namespaces «островами», но явно учитывать границы:

- Для выбранного namespace требовать public signatures и отсутствие неизвестных экспортируемых типов.
- Проверять его собственные тела всегда, включая функции без аннотаций; unknown-аргументы честно ограничивают точность.
- Непроверенную зависимость подключать через extern либо сообщать unknown boundary. Не считать отсутствие её диагностики доказательством безопасности.
- Подавление привязывать к коду диагностики и конкретному месту; unused suppression — сигнал для удаления.
- Baseline сравнивать только при одинаковых checker/config/dependency inputs; иначе снижение числа ошибок можно получить исключением файлов из проверки.

Стоимость невелика для конфигурации, средняя для стабильных diagnostic IDs и snapshot migration. Не брать global `ignore_missing_imports` как рекомендуемый старт: он способен дать ложное впечатление покрытия. Текущие счётчики trust в Typespec не равны уже работающему CI gate (F840 прямо разделяет current/planned).

## 4. Callbacks: двунаправленный вывод раньше межпроцедурной магии

**В Python.** Pyright использует expected types для bidirectional inference, в том числе для lambda. Для неаннотированных функций документирован ограниченный по глубине call-site return inference: тип результата может выводиться из конкретных аргументов вызова. Это не доказательство универсальной сигнатуры функции. Mypy callback protocols через `__call__` выражают более сложные callable-интерфейсы, чем короткий `Callable`, включая overloads, variadic и keyword-only параметры; имена keyword-параметров могут влиять на совместимость.

**Перенос в F220/F230/F231/F410.** Сначала реализовать общий алгоритм для HOF, а не очередной special case имени `map`:

1. При вызове инстанцировать параметры generic-контракта свежими переменными.
2. Из аргументов-коллекций собрать ограничения на тип элемента.
3. Передать ожидаемый тип функции в callback и его destructuring.
4. Проверить тело lambda; результат добавить в ограничения.
5. Решить ограничения и проверить bounds/obligations. Нерешённое не заменять молча Any.

```clojure
;; ids известно как vector of string:
(map (fn [id] (inc id)) ids)       ; ошибка внутри callback, id = string?
(map (fn [id] (str id "!")) ids)   ; результат — последовательность строк
```

Для собственного `map-like`, имеющего корректную generic-сигнатуру, ожидать то же поведение. Это отделяет универсальную поддержку от текущих core-special-cases. Generic функцию проверять с жёсткими параметрами, а вызов — со свежими переменными; один удачный вызов не доказывает `(all [a] ...)`.

**Цена/границы.** Средняя–высокая: overloads, variadic arities, `apply`, `comp`, transducers и recursive inference резко усложняют constraint solving. Python callback protocols не надо буквально превращать в Clojure protocols: обычному `fn` достаточно структурной callable-сигнатуры. Не переносить Python keyword-argument naming в позиционные Clojure calls. Ограниченный call-site inference можно рассмотреть позже, с budget и объяснимым fallback, без автоматического экспорта полученного контракта.

## 5. Incrementality: интерфейсы сначала, fine-grained daemon потом

**Исходники mypy.** `mypy/server/deps.py` хранит карту trigger → targets. Trigger представляет изменившееся внешне видимое свойство, например сигнатуру функции; target — функция, метод или top-level модуля. `astdiff.py` сравнивает снимки symbol tables. `update.py` описывает цикл: повторно разобрать/проверить изменённый модуль, вычислить изменившиеся свойства, пометить зависимые targets, повторять до отсутствия новых triggers. Важна не просто дата изменения файла: правка тела, не отражённая в интерфейсе, не обязана инвалидировать клиентов. В реализации есть цена mutable AST: strip/merge и сохранение identity. Старые dependency edges могут сохраняться, вызывая лишнюю работу, но не пропущенную проверку.

**Исходники Pyright.** В прочитанном `program.ts::markFilesDirty` изменённый source file помечается dirty, `_markFileDirtyRecursive` отмечает зависимые файлы; изменение builtins вызывает mark-all, при наличии dirty set пересоздаётся evaluator. Есть отдельная обработка filesystem aliases. Из этого участка нельзя заключать, что Pyright использует ровно mypy signature-trigger algorithm или что проверка всегда полностью перечитывает весь проект.

**Для F720/F910, не новой карточки.** Первый этап — namespace cache:

- Hash исходника + checker/config + extern/spec manifests + target JVM/Clojure/classpath fingerprint.
- Нормализованный exported interface и его hash отдельно от source hash.
- При правке тела перепроверить namespace; если экспорт не изменился, не распространять invalidation на клиентов.
- Зависимости учитывать не только через `require`: qualified spec keys, macro contracts, schema providers, Java/classpath и inferred exports тоже входят в граф.
- В циклах namespaces обрабатывать SCC; при ограничении итераций выдавать честную неполноту, а не старый успешный результат.
- Смена core externs/checker version инвалидирует всё зависимое; отключённый source span/annotation не может пережить смену hash.

Пример: `a/f` остаётся `[string? :-> string?]`, но тело теперь возвращает число — ошибка должна появиться в `a`, даже если интерфейс клиентов не поменялся. Если результат был inferred и стал числом, меняется interface hash и проверяются пользователи `a/f`.

**Цена.** Namespace-level кеш — средняя сложность. Function-level граф и интерактивный daemon — высокая: неполные буферы, удалённые definitions, cycles, fresh diagnostics и cancellation. Лучше immutable summary Typespec, чем копирование AST surgery mypy. Нужны differential tests: cold check и incremental check дают одинаковые диагностики после правок тела, сигнатуры, extern, spec key, макроконтракта и конфигурации. Производительность здесь не измерялась; численные обещания ускорения были бы необоснованны.

## Приоритеты без дублирования карточек

| Приоритет | Доработать существующие карточки | Конкретный результат | Цена |
|---|---|---|---|
| P0 | F711/F130/F710 | Версионированный inert extern loader, conflict/provenance diagnostics, первые core/library fixtures | Средняя; сопровождение высокое |
| P0 | F210/F730/F810/F840 | Причины unknown, явный Any, границы namespace, раздельные coverage/trust показатели | Средняя |
| P1 | F220/F230/F231/F410 | Expected-type callback inference для пользовательской generic HOF, не только core map | Высокая |
| P1 | F720/F910 | Interface hash + корректная namespace invalidation, cold/incremental differential tests | Средняя–высокая |
| P1 | F830/F840 | Targeted suppression, unused check, baseline при закреплённой конфигурации | Средняя |
| P2 | F720/F910 | Fine-grained targets, daemon и бюджет call-site inference после измерений | Высокая |

Это предложения по уточнению/реализации уже имеющихся возможностей. Новые карточки и изменения кода не создавались.

## Что не брать

- Python `Unknown` как образец safe top; у него другая семантика.
- Any как бесшумную замену отсутствующего extern, неподдержанного macro или исчерпанного inference budget.
- Автоматическое копирование `.pyi` shadowing на свои тела: внешняя аннотация не должна отменять проверку реализации.
- Runtime imports/introspection `stubtest` внутри штатного статического checker.
- Python-specific классы, MRO, keyword-only matching и AST mutation как обязательную основу Clojure HOF.
- «100% type completeness» как эквивалент отсутствия Any, доказательства тела или корректности externs.
- Daemon и глубокий call-site inference прежде контрактов и корректной инвалидации.

## Первичные источники и что именно проверено

Все ссылки доступны без платных сервисов/логинов. Документация upstream, не сторонние abstracts. Исходники прочитаны выборочно в указанных алгоритмических участках.

1. [Pyright: type inference](https://github.com/microsoft/pyright/blob/main/docs/type-inference.md) — Unknown, expected types, call-site return inference и ограничения глубины.
2. [Pyright: configuration](https://github.com/microsoft/pyright/blob/main/docs/configuration.md) — режимы и diagnostics unknown.
3. [Pyright: import resolution](https://github.com/microsoft/pyright/blob/main/docs/import-resolution.md) — stubs, порядок поиска.
4. [Pyright: typed libraries](https://github.com/microsoft/pyright/blob/main/docs/typed-libraries.md) — `--verifytypes`, completeness, JSON, `--ignoreexternal`.
5. [Pyright program.ts](https://github.com/microsoft/pyright/blob/main/packages/pyright-internal/src/analyzer/program.ts) — `markFilesDirty` и связанная рекурсивная инвалидация.
6. [mypy: dynamic typing](https://github.com/python/mypy/blob/master/docs/source/dynamic_typing.rst) — Any, распространение, unannotated parameters и generics.
7. [mypy: existing code](https://github.com/python/mypy/blob/master/docs/source/existing_code.rst) — gradual adoption и строгие настройки.
8. [mypy: protocols](https://github.com/python/mypy/blob/master/docs/source/protocols.rst) — раздел Callback protocols.
9. [mypy: stubs](https://github.com/python/mypy/blob/master/docs/source/stubs.rst) — отдельные интерфейсы и приоритет `.pyi`.
10. [mypy: stubtest](https://github.com/python/mypy/blob/master/docs/source/stubtest.rst) — runtime import/introspection и недоказуемость return contracts этим инструментом.
11. [mypy: daemon](https://github.com/python/mypy/blob/master/docs/source/mypy_daemon.rst) — документированное наличие incremental daemon; скорость в этом исследовании не проверялась.
12. [mypy/server/deps.py](https://github.com/python/mypy/blob/master/mypy/server/deps.py), [astdiff.py](https://github.com/python/mypy/blob/master/mypy/server/astdiff.py), [update.py](https://github.com/python/mypy/blob/master/mypy/server/update.py) — технические описания и фрагменты реализации trigger/target, symbol snapshots, propagation loop. Комментарии о конкретных semantic-analysis passes не следует считать независимой проверкой каждой ветви текущей реализации.
13. [typeshed README](https://github.com/python/typeshed/blob/main/README.md) — назначение, stdlib vs third-party, version pinning и риск stub drift.
14. GitHub REST metadata: [Pyright](https://api.github.com/repos/microsoft/pyright), [mypy](https://api.github.com/repos/python/mypy), [typeshed](https://api.github.com/repos/python/typeshed); для HEAD использован endpoint `/commits?per_page=1` каждого репозитория.

Дополнительно скачана Pyright `type-concepts-advanced.md`, но её полное содержание не разобрано и отдельные выводы на ней не основаны. Benchmark, запуск checker, полный аудит solver и количественный анализ истории commits остаются вне этой работы.
