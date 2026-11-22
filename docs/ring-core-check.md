# Проверка Ring Core 1.15.3

Обычный `typespec check` проверяет все 28 Clojure-файлов (2 258 строк)
из опубликованного `ring/ring-core` 1.15.3. Версия совпадает с зависимостью
в приёмке `context-http-check`. Зависимости нужны для classpath, область
проверки ограничена исходниками Ring Core из JAR.
[Артефакт на Clojars](https://clojars.org/ring/ring-core/versions/1.15.3)

Результат на Clojure 1.12.4 и JDK 27 — **1 ошибка, 60 предупреждений**.
Trust-счётчики равны нулю. Прогоны до и после поддержки списка ключей
из `s/tuple` в `select-keys` дали одинаковые диагностики.

- `TSP2345` в `src/ring/util/response.clj:295:31`. В ветке `resource-data :jar`
  JVM-метод `URL.openConnection()` объявляет `URLConnection`. Чекер не связывает
  диспетчеризацию по протоколу `jar` с типом возвращённого соединения. Для
  стандартного `jar:` URL проверка в JVM подтвердила `JarURLConnection`, и
  `resource-data` успешно прочитал ресурс из закреплённого JAR.
- 46 предупреждений относятся к неизвестным типам (`TSP210-01`), шесть —
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
