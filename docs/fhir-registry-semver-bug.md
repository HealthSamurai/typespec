# Подтверждённый логический дефект prerelease comparator

Источник: fhir-clj/fhir-package-registry pin ea79b6a18d9ae759593c3824f502bc1200209e22, src/fhir/registry/semver.clj, compare-pre-release (:24–63).

При разметке прочитан pure namespace: только clojure.string dependency, определения функций и comment. Он отдельно загружен через clojure-repl plugin; основной fhir.registry не загружен, DB/GCS/HTTP не запускались.

Результат реального REPL:
```clojure
(fhir.registry.semver/semver-comparator "1.0.0-alpha.1" "1.0.0-alpha.2")
;; => 0, ожидается отрицательное значение
(fhir.registry.semver/semver-comparator "1.0.0-alpha" "1.0.0-alpha.1")
;; => 0, ожидается отрицательное значение
```

По SemVer prerelease identifiers сравниваются слева направо; при равенстве первого нужно перейти к следующему, а короткая последовательность при равных предшествующих identifiers ниже длинной. Здесь loop всегда заканчивается на первом identifier: ветви compare возвращают0 сразу, recur отсутствует. Поэтому разные версии с общим первым prerelease identifier ошибочно объявляются равными. Это может влиять на sort/latest для prerelease; production проявление и реальные пакеты не проверялись.

Это подтверждённый дефект логики приложения, найденный чтением при разметке и REPL-пробой, **не находка статического Typespec**. Обычная сигнатура string×string→int его не обнаружит. Исходник upstream и checker не изменялись. Следующий шаг — property/example tests lexicographic сравнения и отдельное исправление upstream после согласования.
