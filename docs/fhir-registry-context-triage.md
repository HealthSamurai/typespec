# Registry context: nested def и unbound Var

На pin ea79b6a18d9ae759593c3824f502bc1200209e22 checker сообщает TSP2305 в src/fhir/registry/diff.clj:148:14: `fhir.registry has no var context`.

Исходник определяет context через nested def внутри -main (registry.clj:846) и start-dev (:855). Эти формы не внутри comment. diff.clj:148 читает Var на верхнем уровне, :150 передаёт его в register-ns-endpoints. Поэтому важно отличать существование Var от его инициализации.

## Независимая проверка через clojure-repl plugin

В приватном Unix-socket nREPL, без загрузки fhir.registry, без DB/GCS/HTTP:

```clojure
(create-ns 'triage.nested-def)
(binding [*ns* (the-ns 'triage.nested-def)]
  (clojure.core/refer 'clojure.core)
  (eval '(defn init [] (def context :ready)))
  (let [v (ns-resolve *ns* 'context)]
    {:var-exists (var? v)
     :bound-before-init (bound? v)
     :value-class (str (class (var-get v)))}))
;; => {:var-exists true, :bound-before-init false,
;;     :value-class "class clojure.lang.Var$Unbound"}
```

Checker-only двухфайловый repro:

```clojure
(ns probe.a) (defn init [] (def context :ready))
;; другой файл
(ns probe.b (:require [probe.a])) (def x probe.a/context)
```

Даёт TSP2305 `probe.a has no var context`. При этом probe namespaces не исполнялись checker, fhir.registry в REPL остаётся незагруженным.

## Классификация

**Сообщение об отсутствии Var — ложное срабатывание модели объявления Typespec.** Но реальная безопасность чтения context зависит от порядка инициализации: до вызова -main/start-dev Var unbound. Не следует просто присвоить ему тип значения nested def и объявить все чтения безопасными. При исправлении нужно отличать объявленный Var от доказанной инициализации; как минимум сохранить unknown/неполноту для такого чтения. Сам get-ig не запускался; поведение register-ns-endpoints на unbound значении не проверялось и runtime-дефект сервиса здесь не заявляется.
