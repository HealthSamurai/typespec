# Приёмочные сценарии подключения solver

План тестов; не утверждение о текущей реализации. Синтаксис t/all берётся из reference, один fixed-arity контракт в первом срезе.

## Независимые вызовы и проверка тела

```clojure
(defn identity-value
  {:t/sig '(t/all [a] [a :-> a])}
  [x] x)

(defn text-value {:t/sig '[string? :-> string?]} [s]
  (identity-value s))
(defn number-value {:t/sig '[int? :-> int?]} [n]
  (identity-value n))

(defn bad-identity
  {:t/sig '(t/all [a] [a :-> a])}
  [x] 42) ; ошибка тела: конкретное число не произвольное a
```

Проверить порядок двух вызовов и отсутствие утечки instantiation, наличие ошибки именно в bad-identity, сохранение all в sig.

## Callback на пользовательской HOF

```clojure
(defn apply-one
  {:t/sig '(t/all [a b] [a (t/fn [a :-> b]) :-> b])}
  [x f] (f x))

(defn length-of {:t/sig '[string? :-> int?]} [s]
  (apply-one s (fn [text] (count text))))

(defn bad-length {:t/sig '[string? :-> int?]} [s]
  (apply-one s (fn [text] "wrong"))) ; несовместимый результат
```

Обычный аргумент даёт a; callback проверяется с expected a; его результат даёт b. Нельзя сначала принять unknown-параметр callback за any или выпустить диагностики из пробного прохода.

## Ограничение, extern и неполная проверка

```clojure
(defn numeric-id
  {:t/sig '(t/all [(a :< number?)] [a :-> a])}
  [x] x)
;; numeric-id со строкой — mismatch, unknown — incomplete.

;; EDN declaration:
;; {vendor/id (t/all [a] [a :-> a])}
```

Загрузчик должен принять callable all, сохранить происхождение и проверять собственное тело при его наличии. Неизвестная ссылка, duplicate binders, nested all, generic rest/kwargs и нерешённый параметр результата получают явный ограниченный результат, не ложный успех.

## Команды

Unit kernel и интеграционные tests запускаются отдельно перед добавлением к clojure -M:test. Golden не обновляются для скрытия регрессий. Source и extern paths должны проходить одну грамматику.
