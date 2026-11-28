# Проверка Ring Core 1.15.3

Обычный `typespec check` проверяет все 28 Clojure-файлов (2 258 строк)
из опубликованного `ring/ring-core` 1.15.3. Версия совпадает с зависимостью
в приёмке `context-http-check`. Зависимости нужны для classpath, область
проверки ограничена исходниками Ring Core из JAR.
[Артефакт на Clojars](https://clojars.org/ring/ring-core/versions/1.15.3)

Результат на Clojure 1.12.4 и JDK 27 — **1 ошибка, 65 предупреждений**.
Trust-счётчики равны нулю. Прогоны до и после поддержки списка ключей
из `s/tuple` в `select-keys` дали одинаковые диагностики. Проверка
низкоуровневого `extend` добавила одно предупреждение об отсутствующем
`s/fdef` для реализации на байтовых массивах. Чтение типа статического поля
`File/separatorChar` убрало `TSP210-01` в `src/ring/util/response.clj:175:23`.
На JVM значение поля `char` передаётся в `clojure.string/replace` как `Character`.
Контракт `re-find` добавил `TSP210-01` в `util/mime_type.clj:107:52`,
`util/parsing.clj:30:25`, `util/parsing.clj:30:36` и `websocket.clj:72:38`.
Проверка теперь доходит до аргументов `Pattern` и `CharSequence`.
Тип группы в `mime_type.clj:107` убрал `TSP210-01` на `lower-case` в `108:21`.
Контракт `re-matches` добавил четыре `TSP210-01` в
`middleware/keyword_params.clj:12:19`, `:12:36`, `:13:43` и `:13:65`.
F721 вывел тип `Pattern` для двух Var с regex-литералами и убрал
предупреждения в `:12:19` и `:13:43`. Тип параметра `s` остаётся неизвестным.
Контракт `re-seq` добавил два `TSP210-01` в
`middleware/cookies.clj:30:32` и `:30:42` для `re-cookie` и `header`.
Вывод из инициализаторов `re-pattern` убрал предупреждения на `re-cookie`
в `:30:32` и `re-charset` в `util/parsing.clj:30:25`.

- `TSP2345` в `src/ring/util/response.clj:295:31`. В ветке `resource-data :jar`
  JVM-метод `URL.openConnection()` объявляет `URLConnection`. Чекер не связывает
  диспетчеризацию по протоколу `jar` с типом возвращённого соединения. Для
  стандартного `jar:` URL проверка в JVM подтвердила `JarURLConnection`, и
  `resource-data` успешно прочитал ресурс из закреплённого JAR.
- 50 предупреждений относятся к неизвестным типам (`TSP210-01`), семь —
  к отсутствующим `s/fdef` мультиметодов (`TSP430-03`), восемь — к Java interop.

## Воспроизведение

Из корня Typespec, с Clojure CLI и Python 3. Команда `check` завершается
с кодом 1 из-за указанной диагностики.

```sh
ring_root=$(mktemp -d)
ring_deps='{:deps {ring/ring-core {:mvn/version "1.15.3"}}}'
ring_classpath=$(clojure -Sdeps "$ring_deps" -Spath)

python3 - "$ring_root" "$ring_classpath" <<'PY'
import hashlib
import os
import sys
from pathlib import Path
from zipfile import ZipFile

root = Path(sys.argv[1]) / "src"
(jar,) = [Path(entry) for entry in sys.argv[2].split(os.pathsep)
          if Path(entry).name == "ring-core-1.15.3.jar"]
assert hashlib.sha256(jar.read_bytes()).hexdigest() == (
    "4d8d97098898e33e8bb78d3f0f28b0b1c8cede6d88ba3a4ec35ee58e974007bc"
)
with ZipFile(jar) as archive:
    sources = [name for name in archive.namelist()
               if name.startswith("ring/") and name.endswith((".clj", ".cljc", ".cljs"))]
    assert len(sources) == 28
    for name in sources:
        dest = root / name
        dest.parent.mkdir(parents=True, exist_ok=True)
        dest.write_bytes(archive.read(name))
PY

clojure -Sdeps "$ring_deps" -M:check check --root "$ring_root"
```

## Проверка `body-string` по Spec

Добавьте [проверочный `s/fdef`](../examples/ring-core-check/body_contract.clj)
в распакованные исходники и повторите обычный `check`.

```sh
cp examples/ring-core-check/body_contract.clj "$ring_root/src/ring/"
clojure -Sdeps "$ring_deps" -M:check check --root "$ring_root"
```

С контрактом прогон охватывает 29 файлов. `class` в `(comp class :body)`
сужает поле до `string?` в исходном методе `String`. До доработки F440
эта ветка давала `TSP2322`. Проверка variadic-сигнатуры `str` также
подтверждает результат `(apply str (:body request))` в методе `ISeq`.
Каталог `clojure.core` выводит `string?` для `slurp` в методах `File`
и `InputStream`. С контрактом получены **1 ошибка, 65 предупреждений**.
Диагностики совпадают с прогоном без добавленного `s/fdef`.

## Проверка `SizableResponseBody`

Добавьте [контракт `body-size-in-bytes`](../examples/ring-core-check/content_length_contract.clj)
рядом с контрактом `body-string` и повторите `check`.

```sh
cp examples/ring-core-check/content_length_contract.clj "$ring_root/src/ring/"
clojure -Sdeps "$ring_deps" -M:check check --root "$ring_root"
```

С обоими контрактами проверка охватывает 30 файлов и даёт **1 ошибку,
60 предупреждений**. Проверены пять реализаций `body-size-in-bytes`, включая
`extend` на `(Class/forName "[B")`. В отдельной копии исходников подмена
`(alength bs)` строкой дала `TSP2322` в `content_length.clj:17:22`.

## Массивы в `ring.middleware.session.cookie`

Конструктор `(byte-array data)` передаёт класс `[B` в вызов `Cipher.doFinal`.
Тип виден на исходниках через `type-at`.

```sh
clojure -Sdeps "$ring_deps" -M:check type-at src/ring/middleware/session/cookie.clj:55:31 --root "$ring_root"
```
