# Typespec — reference

Details behind the design in [README.md](../README.md). Implementation and milestones: [PLAN.md](../PLAN.md). Status: this is the design; the prototype implements part of it (the list is in the README, section "Try it").

## 1. Type language

Types are **data in spec style**: a subset of spec forms plus `t/` forms. They are never evaluated by Typespec.

```clojure
string?  keyword?  boolean?  uuid?  inst?            ; basic types
map?  vector?  set?  seq?  fn?                        ; untyped containers
int?     double?   number?                           ; int? = long-like values, not arbitrary numbers
nil?     any?      t/unknown  t/never                ; any? = explicit opt-out, unknown = "narrow first"
#{"active" "inactive"}  #{:get :post}                ; literal unions
(s/nilable T)   (s/or :a A :b B)                     ; union
(s/keys :req [:patient/id] :opt [:patient/name])     ; map with qualified keys (structural)
(s/keys :req-un [:patient/id])                       ; unqualified key :id, type from :patient/id
(s/coll-of T :kind vector?)  (s/map-of K V)  (s/tuple A B)
(s/merge A B)                                        ; input must satisfy both = intersection of requirements
(s/cat :a A :b B)  (s/* T)  (s/? T)                  ; argument lists, as in s/fdef
String  long  java.time.Instant                      ; Java classes and primitives
(t/class java.util.List String)                      ; Java generics
(t/array String)                                     ; Java arrays
(t/platform String)                                  ; from a hint or Java: nil is not checked (section 4)
(t/fn [A B :-> R])                                   ; function
(t/all [a (r :< T)] [...])                           ; generics with bounds
(t/exact (s/keys ...))                               ; no extra keys
(t/and A B)  (t/pick T :k ...)  (t/omit T :k ...)  (t/partial T)  (t/keys-of T)  (t/get T :k)
(t/keys-str {"id" string?})                          ; string keys (JSON before keywordization)
(t/keys {:req [...]} :rest (s/map-of keyword? any?)) ; known keys + a typed tail
(t/atom T)  (t/chan T)                               ; mutable types are invariant
(t/opaque javax.sql.DataSource)                      ; a value you may pass around but not look into
(t/brand string? :patient/id)                        ; optional, see README "Optional extensions"
```

This is the **whole** grammar. Tool output (errors, `sig`, `type-at`, JSON) uses only these forms, so it can be pasted back. In prose below, `T | nil` is shorthand for `(s/nilable T)`.

**Where each form may appear.** Plain spec forms can go in `s/def` and `s/fdef` (real spec, works without Typespec). `t/` forms are type-only, so they appear only where nothing is evaluated: in a quoted `{:t/sig '...}`, in `typespec/types.edn`, and in externs.

**Map type operators.** `t/pick` and `t/omit` select or drop declared fields and preserve their types and requiredness. `t/partial` makes top-level fields optional and preserves the type of each present value. `t/keys-of` returns the union of declared required and optional keys, or `t/never` for an empty shape. `t/get` gives the type of reading a declared field, adding `nil` for an optional key. The input of each operator must resolve to a single declared `s/keys` shape. The resulting maps remain open. These operators are supported in `:t/sig` and external EDN declarations.

Rules the type checker makes explicit:

- `s/and` yields the type of its first component. Other predicates are **refinements**, meaning documentation (`pos?`, regexes), and are not checked statically.
- A form outside the grammar produces `TSP9001 not a type`, not a silent `any?`.
- Recursion is allowed: `(s/def :fhir/extension (s/keys :opt-un [:fhir/extension ...]))` is a recursive type.

---

## 2. Where types come from

| What | Source | Where it is written |
|---|---|---|
| value of key `:ns/k` | `(s/def :ns/k T)`, or an entry in `typespec/types.edn` | alongside domain code (`specs.clj`), or type-only in EDN |
| entity shape | `(s/def ::patient (s/keys ...))` | same place |
| public function signature | `s/fdef`, or type hints on every parameter and the result, or `{:t/sig '[...]}` in the `defn` attr-map | next to the function |
| parameter types without a signature | `:pre` conditions: `(string? x)`, `(s/valid? ::spec x)` | in the function |
| return type without a signature | inferred from the body | — |
| Java classes and methods | bytecode on the classpath | — |
| private function signature | not required; inferred locally | — |
| local values | inference | — |
| third-party libraries | externs | `typespec/externs/` |

### 2.1 `t/sig` — signatures as data

```clojure
(defn find-patient
  {:t/sig '[::ctx :patient/id :-> (s/nilable ::patient)]}
  [ctx id]
  ...)
```

This is a regular `defn` attr-map. The type is **quoted**, so the compiler simply stores it in var metadata without executing anything. Typespec reads it from source. Multiple arities and overloads are written as follows:

```clojure
{:t/sig '([:fhir/id :-> (s/nilable :fhir/resource)]
          [:fhir/type :fhir/id :-> (s/nilable :fhir/resource)])}
```

`s/fdef` is also supported for code that already uses it: the type checker reads the `s/fdef` form from source. If a function has both `t/sig` and `s/fdef`, they must match; otherwise, it produces `TSP2790`.

Keys from other namespaces are written with an alias; `(:require [app.patient :as-alias patient])` gives the alias without loading the namespace.

Why not `^{:t ...}` on parameters? Metadata on a map-shaped parameter is lost during `fn` expansion, while unquoted metadata in an attr-map is evaluated. One quoted signature per function means one place, with no exceptions.

### 2.2 `typespec/types.edn` — type-only declarations

```clojure
;; typespec/types.edn — data, never loaded by Clojure
{:patient/id      (t/brand string? :patient/id)       ; refines (s/def :patient/id string?)
 :app/handler     (t/fn [:ring/request :-> :ring/response])
 :legacy/row      (s/map-of keyword? any?)            ; a key that has no s/def at all
 app.legacy/run   [:legacy/row :-> any?]}             ; a symbol = signature of a var (sidecar)
```

Keywords declare key types, symbols declare var signatures. A sidecar signature is for code you cannot or do not want to touch; if the var also has `t/sig`, the two must match (`TSP2790`). With `:signatures :sidecar` in the config, `annotate` writes every signature here and leaves source files untouched.

A key may be declared in `s/def`, in `types.edn`, or both. If both exist, the EDN type must be a subtype of the spec (`TSP2791` otherwise). `annotate` writes new key types here by default: adding an `s/def` is not behavior-neutral, because spec validates every registered key it meets in a map.

### 2.3 Destructuring — a check, not a source

When a parameter's type is declared, destructuring is **checked** against it:

```clojure
(defn display-name
  {:t/sig '[::patient :-> string?]}
  [{:patient/keys [name birthDate]}]
;;                      ~~~~~~~~~ TSP2339: :patient/birthDate is not a key of ::patient.
;;                                Did you mean :patient/birth_date?
  name)
```

When no type is declared (a private function), destructuring establishes a **local lower bound**: “this function needs these keys.” It is checked at call sites **in the same ns** and does not propagate further:

```clojure
(defn- label [{:patient/keys [id name]}] (str id " " name))
;; inferred: [(s/keys :opt [:patient/id :patient/name]) :-> string?]
```

In strict mode, a qualified key with no declared type produces `TSP2304: no type for :patient/birthDate`. This catches typos even without a declared type.

An unqualified key (`{:keys [id]}`) gets its type only from a declared `s/keys :req-un`. Without a declaration, its type is `t/unknown`.

### 2.4 `:keys!` and other Clojure 1.13 directives

`:keys!`, `:req!`, `:select`, `:excess`, `:missing`, and `:all` are **code**: they change behavior (`req!` throws an exception). Typespec never adds or removes them automatically. If they are already in the code, Typespec understands them:

| Directive | Type |
|---|---|
| `:keys! [a]` | the key is required in the input. Any nested `!` makes the **entire path** to it required and non-nil (`req!` on nil throws). The value has its spec type; nil remains if the spec allows it |
| `:keys [a]` | value is `T \| nil`; a parameter using only `:keys` has type `(s/nilable map?)` |
| `:or {a d}` | `T \| typeof(d)`, but an explicit `{:a nil}` yields `nil`; `:or` on a `!` key is a Clojure compilation error, which Typespec also reports |
| `[a & :b :c]` | `:b :c` participate in checks without creating bindings; they are also required in `:keys!` |
| `:select s` | `(t/pick (merge defaults Arg) ks)`, recursively through nested forms; `\| nil` if Arg is nilable; record → plain map |
| `:excess e` | recursive remainder: unmentioned keys plus nested `{:k nested-excess}`, `\| nil` |
| `:missing m` | `!` keys become optional for the caller; bindings are `T \| nil`; narrowed after `(when-not m ...)` |
| `:all a` | `(merge defaults Arg)`, with the argument taking precedence |
| `(selector f)` | one directive → its type; multiple directives → `{:select .. :excess ..}` without nil values |
| `[& {:keys! [a]}]` | required keyword arguments |

The table is validated with a **differential test**: a generator builds random forms and values, then compares the predicted type with the result of the actual `destructure`. The Clojure version is pinned in the config: the 1.13 syntax is still in alpha, `:defaults` has already been removed, and `:req!` was added later.


---

## 3. Type system

- **Maps are structural.** `(s/keys :req [:a])` accepts a map with any additional keys. **Reading** a key absent from both the type and the declared key types is an error (`TSP2339`). Reading from `nil` yields `T | nil`.
- A **fresh literal** `{...}` in a call is checked for extra keys **only against the declared type**. An extra qualified key without a declared type is an error; a key that looks like a typo produces an error with Did you mean.
- **`t/exact`** forbids extra keys everywhere.
- **Map operations.** `get-in` uses the default when a literal path may be absent and preserves `nil` stored at the final key. `assoc-in` and `update-in` track keyword paths and keep sibling fields. [Supported paths](../features/F312-literal-map-paths.md)
- **Fixed vector positions.** `s/tuple` checks length and element order. Vector literals supply positional types for destructuring and constant indices. [Supported operations](../features/F331-fixed-vector-positions.md)
- **`apply` with a fixed tail.** Arguments before the final sequence and its known positions form one checked call. [Supported tails](../features/F411-apply-fixed-tail.md)
- **Generics** with explicit binders and constraints:
  ```clojure
  {:t/sig '(t/all [a (r :< :fhir/resource)] [(s/coll-of r) (t/fn [r :-> a]) :-> (s/coll-of a)])}
  ```
  Immutable collections are covariant; `atom`, `volatile`, arrays, and channels are invariant.
- **Function composition.** `comp` checks each unary link from right to left and preserves the rightmost input arities. `partial` checks the captured arguments and retains the remaining positional arities. [Supported signatures and examples](../features/F233-function-composition.md)
- **Transducers in `into`.** An inline `comp` passes element types through its `map` and `filter` stages from left to right. [Supported forms](../features/F235-transducer-composition.md)
- **Literals.** `let` preserves existing literal types. `#_(t/const)` preserves the literal element types of nested map, vector and set literals. `#_(t/satisfies T)` checks the inferred type against `T` and keeps the inferred type. [Supported contracts and examples](../features/F280-literal-preservation.md)
- **Narrowing** after `if`/`when`/`cond`/`case`/`when-let`, `some?`/`nil?`/type predicates/`instance?`/`=` with a literal/`contains?` on a map. For a function with one positional parameter, the checker can infer independent truthy and falsy facts from its body. An explicit `{:t/guard '{:type ::patient :mode :checked}}` requires a truthy proof. Add `:two-sided true` to require a falsy proof. The trusted form `{:t/guard '::patient}` supplies a truthy fact and is counted once per declaration in the trust budget. `filter`, `filterv` and `take-while` narrow elements using the truthy fact. `remove` uses the falsy fact.
- **Exhaustiveness**: `case` over literal unions, `cond`/`condp` using `=` on a single variable, and `defmulti` with closed dispatch (`{:t/closed true}`). A `cond` without `:else` yields `T | nil`; this is not an error.
- **nil-punning**: `(first nil)` → `nil`, `(seq x)` narrows to a non-empty collection, `(:k x)` on `t/unknown` produces a “narrow first” error.
- **String keys** and **map tails**: `t/keys-str` and `t/keys ... :rest` from section 1.

### Clojure specifics

| Construct | Rule |
|---|---|
| `defrecord R [a b]` | nominal class + fields + open extra keys; `assoc` preserves `R`, `dissoc` of a field yields a map |
| `defprotocol` | `{:t/sig ...}` on methods; every `extend-*` is checked, including those in other namespaces |
| `defmulti` | the dispatch function is typed; if it is a keyword (`:resourceType`), the argument in `defmethod` is narrowed by value |
| `atom` | `(t/atom T)`; `swap!` requires `[T ... :-> T]`; narrowing of `@a` does not survive a second deref |
| `core.async` | `(t/chan T)`; `<!` → `T \| nil`; `go`/`go-loop` have built-in rules |
| `binding`, `eval`, `resolve` | `t/unknown` |

### Macros

Typespec does not run project code, so a macro has to be known to it in one of four ways:

| Way | When |
|---|---|
| built-in rule | `clojure.core` and core.async macros (`let`, `fn`, `for`, `doseq`, `->`, `cond->`, `go`, `with-open`, …); errors are reported in terms of the original form |
| `:lint-as` | the macro has the shape of a known one: `{my.app/defhandler clojure.core/defn}` |
| hook | a small function (run in SCI, not on the project classpath) that rewrites the form into plain Clojure |
| `:t/macro` in externs | the macro behaves like a typed function |

Existing `clj-kondo` configuration (`:lint-as`, hooks, `clj-kondo.exports/` in jars) is reused as is. An unknown macro gives `TSP7031`; its body is not checked and its value is `t/unknown`. With `:macros {:expand true}` unknown macros are expanded in a separate worker process without network access, and the expansion is checked; macro code is then trusted code.

`cond->` and `cond->>` check each reachable step and combine the accumulator types from executed and skipped steps. [Examples](../features/F611-conditional-threading.md)

`time`, `lazy-seq` and `future` check their bodies and report diagnostics at the original expressions. [Wrapper types](../features/F612-core-wrapper-bodies.md)

---

## 4. Type hints and Java types

Clojure on the JVM already has a type system — Java's. Typespec uses it fully instead of hiding it.

### 4.1 Hints are types

```clojure
(defn slug ^String [^String title ^long max]          ; hints = signature, no t/sig needed
  (subs (.toLowerCase title) 0 (min max (.length title))))

(defn parse-ts ^java.time.Instant [^CharSequence s] (java.time.Instant/parse s))

(map ^[String] String/.trim lines)                     ; Clojure 1.12 param-tags: exact overload
```

- A hinted parameter or return has that type. Hints count as a signature: a function with hints on all parameters and the return value needs no `t/sig`.
- Class names are part of the type language: `{:t/sig '[String long :-> (s/nilable java.time.Instant)]}`, `(s/coll-of java.util.UUID)`, `(t/class java.util.List String)` for Java generics.
- `^String` allows nil (the compiler allows it) — the type is the **platform type** `(t/platform String)`, as in Kotlin's `String!`: method calls are allowed, nil flows are counted in coverage. With `:strict-nil` it becomes `String | nil`, and `(.length s)` requires narrowing. Primitives `^long`/`^double` are never nil.
- `t/sig` and hints must agree: `^long` in the code and `string?` in the signature give `TSP2322`.

### 4.2 The type checker checks hints

| Check | Example | Code |
|---|---|---|
| hint contradicts the inferred type — a `ClassCastException` caught before running | `(let [^String s (count xs)] ...)` | `TSP2322` |
| return hint on the name instead of the arg vector (breaks callers) | `(defn ^long f [x] ...)` | `TSP7021` |
| reflection: the class is unknown | `(.getName x)` with `x : t/unknown` | `TSP7020` |
| reflection: the class is known, but the compiler does not see it | `x` is proven `File`, no hint | `TSP7023` + fix |
| ambiguous overload for the given types | `(.write w x)` with `x : Object` | `TSP7022` |
| boxed math on a hot path | `(+ a b)` with `a : Long` boxed | `TSP7024` (opt-in) |
| primitive hints on more than 4 arguments | `(fn [^long a ^long b ^long c ^long d ^long e])` | `TSP7025` |
| useless hint: the type is already known to the compiler | `^String (str x)` | `TSP7026` (hint) |

**Hint fixes.** Because the type checker knows types the compiler does not see, `fix` can insert hints that remove reflection. Hints are inserted only by an explicit `fix`, never by `check` or `annotate`. A fix is `safe` when exactly one overload matches the proven type and it is the same one reflection would pick. Otherwise it is `review`. This is the main everyday win: `*warn-on-reflection*` warnings turn into automatic, proven fixes.

### 4.3 Java types from bytecode

- **Classes, interfaces, inheritance**: subtyping follows Java (`ArrayList <: List <: Collection`), and Clojure collections are Java collections too (`IPersistentVector <: java.util.List`).
- **Methods, constructors, static fields, varargs**: arity and argument types are checked; overload resolution follows the Clojure compiler's rules.
- **Inherited generic returns.** When a concrete receiver binds a superclass or interface type variable, Typespec uses its known nominal class as the method result. Raw receivers and unresolved variables retain the erased return. [F530 scope](../features/F530-java-generics-nullability.md)
- **Nullability**: JSpecify, JetBrains, jakarta, Android `@Nullable`/`@NonNull`, and `@NullMarked` packages. Without annotations, results are platform types (section 4.1).
- **`instance?`, `cond->` and `case` on classes** narrow types; `(instance? Long x)` narrows to `Long`.
- **`reify`, `proxy`, `deftype`, `defrecord` implementing interfaces**: method signatures are checked against the interface.
- **Functional interfaces** (Clojure 1.12): a Clojure fn passed where `java.util.function.Function` is expected is checked as `(t/fn [A :-> B])`.
- **Arrays**: `^bytes`, `^"[Ljava.lang.String;"`, `(t/array String)`; `aget`/`aset` are typed.
- **Checked exceptions** are not tracked: Clojure does not have them.

Java declarations need no externs: the bytecode on the classpath is the `.d.ts`. Externs are only for nullability overrides, e.g. `{java.util.Map/get [K :-> (s/nilable V)]}`.

---

## 5. Third-party libraries — externs

```clojure
;; typespec/externs/next.jdbc@1.3.939.edn — data only, not code
{next.jdbc/get-datasource ([string? :-> :next.jdbc/datasource]
                           [map? :-> :next.jdbc/datasource])
 next.jdbc/execute-one!   [:next.jdbc/datasource :next.jdbc/sql-params :-> (s/nilable :next.jdbc/row)]
 :types {:next.jdbc/datasource (t/opaque javax.sql.DataSource)
         :next.jdbc/sql-params (s/cat :sql string? :params (s/* any?))
         :next.jdbc/row        (s/map-of keyword? any?)}}
```

- Externs are **EDN**, not Clojure. They are not loaded as code, executed, or included at runtime.
- The version is part of the filename; a mismatch with `deps.edn` produces a warning.
- Lookup order: local externs → `typespec.exports/` bundled in the library jar (written by `typespec emit`, the analog of `.d.ts`) → `s/fdef` from library source → shared `typespec-externs` repository → `t/unknown`.
- `typespec extern next.jdbc` produces an agent-generated draft based on arglists, docstrings, source, and tests. `--verify` runs the library's tests with `instrument` and `:ret` checking. This is **sample-based** validation, not proof.

---

## 6. Escape hatches and trust

Every escape hatch is a `#_` directive placed before the expression. Clojure discards it; Typespec's parser sees it. Nothing is added at runtime, and no library is required.

```clojure
#_(t/ignore "AIDBOX-123: legacy") (legacy-call x)   ; silence errors in the next form
#_(t/ignore TSP2345 "Library accepts this input") (legacy-call x)
#_(t/cast ::patient) row                            ; trust me: row is ::patient
#_(t/any "dynamic plugin api") (load-plugin id)     ; opt out, with a reason
```

A reason string is required for `ignore` and `any`. An `ignore` with a code suppresses only that diagnostic within the following form. An unused `ignore` produces `TSP2578`. Use `:levels {TSP2345 :off}` to disable one diagnostic across the configured paths. File selection is controlled by `:paths` or the CLI paths. There is no source-level directive to skip an entire file.

`#_(t/const) [:active :pending]` preserves literal element types.
`#_(t/satisfies ::patient) patient` checks compatibility and preserves the
type of `patient`. These directives add no trust entries.

**Trust budget.** CI fails if a PR increases the number of `any`, `cast`, `ignore`, trusted guards or platform types, or weakens a type. Weakening means adding `nilable`, removing a `:req`, changing `:keys!` → `:keys`, or replacing a signature with `any?`.

---

## 7. Configuration

```clojure
;; typespec.edn
{:paths        ["src"]
 :clojure      "1.13.0-alpha11"     ; destructuring semantics
 :types        "typespec/types.edn"
 :signatures   :inline              ; :sidecar = annotate writes signatures to types.edn, source untouched
 :externs      ["typespec/externs"]
 :strict       true                 ; public fns need signatures, a key without a type is an error, no implicit unknown
 :strict-nil   false                ; true: platform types (^String, Java results) become T | nil
 :macros       {:lint-as {my.app/defhandler clojure.core/defn}   ; clj-kondo style
                :hooks   "typespec/hooks"                        ; SCI functions that rewrite a form
                :expand  false}                                  ; true: expand unknown macros in a sandboxed worker
 :levels       {TSP7020 :off, TSP7031 :warning}                  ; per-code severity
 :baseline     ".typespec/baseline.edn"} ; existing errors do not block; new ones do
```

---

## 8. CLI and JSON for agents

```bash
typespec check src --json            # errors
typespec sig app.patients            # inferred and declared ns signatures
typespec type-at src/app/p.clj:14:9  # expression type (hover)
typespec annotate app.patients       # draft t/sig and types.edn — data only, no behavior changes
typespec fix --risk safe             # safe autofixes only
typespec coverage --trust            # proven types, gaps, budget
typespec order                       # annotation order: bottom-up through the ns graph
```

```json
{"version": 1, "id": "e3f1…", "code": "TSP2339", "severity": "error",
 "file": "src/app/patients.clj", "range": {"base": 1, "start": [14, 39], "end": [14, 48]},
 "file_sha": "9ac2…",
 "message": ":patient/birthDate is not a key of ::patient",
 "expected": "(s/keys :req [:patient/id] :opt [:patient/birth_date])",
 "actual": ":patient/birthDate",
 "cause_of": ["a91b…", "c02d…"],
 "related": [{"file": "src/app/specs.clj", "line": 7, "note": "::patient defined here"}],
 "fixes": [{"title": "Rename to :patient/birth_date", "risk": "safe",
            "edits": [{"range": {"start": [14, 39], "end": [14, 48]}, "text": "birth_date"}]}]}
```

- `expected`/`actual` are printed **in the type language from section 1** and can be pasted into `t/sig`.
- `cause_of` groups cascading errors by root cause. `id` is stable across runs (needed for the baseline).
- `risk` is `safe`, `review`, or `unsafe`. A fix using `any?`, a cast, or type weakening is never `safe`.

**Sources for agents, in order of trust:**

1. FHIR Schema (generating key types for Aidbox resources);
2. existing spec and malli definitions;
3. test traces (recorded actual values → draft);
4. code and docstrings.

**Agent loop:** `order` → `annotate` → `check --json` → edit types or code → repeat. Bug fixes and `:keys` → `:keys!` changes go in a separate PR.

---

## 9. Optional extensions (ideas from OCaml / F#)

The foundation is **TypeScript**, and we stick to its ideas as much as possible. The ideas below are **optional** extensions. Each one is enabled by a flag in `typespec.edn` and is mapped to its closest TS counterpart, so we stay within the TS model.

| Idea | From | Closest in TS | Flag |
|---|---|---|---|
| Types from external schemas | F# type providers | `.d.ts` codegen (openapi-typescript) | `:providers` |
| Value tags | F# units of measure, OCaml phantom types | branded types `string & {__brand: "PatientId"}` | `:brands` |
| Open maps with a row tail | OCaml row polymorphism | generic `T extends {id: string}` → `T` | `:rows` |
| Counterexample for a missing case | OCaml exhaustiveness | `never` check in `switch` | `:exhaustive-examples` |
| Error-tolerant analysis for IDEs | OCaml Merlin | tsserver | — (part of LSP) |

**Type providers.** Key types are generated at check time from FHIR Schema, a DB schema, OpenAPI or sample JSON. The output lives in the cache and is not committed:

```clojure
{:providers [{:kind :fhir-schema :package "hl7.fhir.r4.core@4.0.1" :ns :fhir}
             {:kind :openapi :file "api/openapi.yaml" :ns :api}]}
```

**Brands.** Tags exist only during checking and are erased, as in TS:

```clojure
;; typespec/types.edn
{:patient/id   (t/brand string? :patient/id)
 :encounter/id (t/brand string? :encounter/id)}

;; code
(find-patient ctx (:encounter/id e))
;; TSP2345: :encounter/id is not assignable to :patient/id
```

Units of measure are a special case: `(t/brand double? :unit/mg)`.

**Row polymorphism.** A function can take a map with the keys it needs and return "the rest" without `any?`. This is what generics with a constraint and `T & {...}` give you in TS:

```clojure
{:t/sig '(t/all [(m :< (s/keys :req [:db/id]))] [m :-> (t/and m (s/keys :req [:db/version]))])}
```

**Counterexample in exhaustiveness errors.** The error shows a concrete missing case: `TSP8001: case is not exhaustive, e.g. "inactive"`, and for nested maps, `{:status "active" :kind nil}`.

**Not adopted:** full Hindley–Milner inference without signatures (contradicts "signatures at boundaries" and works poorly with maps and macros); functors and GADTs (complexity without payoff for Clojure applications).

---

## 10. Diagnostic codes

Numbers match TypeScript only where the meaning matches.

| Code | Meaning |
|---|---|
| `TSP2304` | no type for a qualified key |
| `TSP2305` | the namespace has no such var |
| `TSP2322` | value is not assignable to the declared type (also: hint contradicts the type) |
| `TSP2339` | key does not exist on the type |
| `TSP2345` | argument is not assignable to the parameter |
| `TSP2353` | unknown key in a map literal |
| `TSP2531` | value may be nil |
| `TSP2554` | wrong number of arguments |
| `TSP2578` | unused `t/ignore` |
| `TSP2678` | a `case` clause that can never match |
| `TSP2741` | required key is missing |
| `TSP2790` | two declarations of one signature disagree (`t/sig`, `s/fdef`, sidecar, hints) |
| `TSP250-01` | truthy result does not prove the declared guard type |
| `TSP250-02` | falsy result does not exclude the declared guard type |
| `TSP250-03` | invalid guard declaration or unsupported proof |
| `TSP270-01` | invalid map type operator arity or nonliteral key |
| `TSP270-02` | key is absent from the declared map shape |
| `TSP270-03` | unsupported map shape, conflicting fields, unresolved or cyclic map reference, or resolution limit |
| `TSP280-01` | unsupported or unproven `t/satisfies` contract |
| `TSP280-02` | malformed or conflicting literal directive |
| `TSP330-01` | vector length does not match a tuple contract |
| `TSP330-02` | constant `nth` index may be out of bounds without a default |
| `TSP410-01` | the length or positions of an `apply` tail could not be verified |
| `TSP2791` | `types.edn` entry is not a subtype of the `s/def` |
| `TSP7010` | strict mode: a public function has no signature |
| `TSP7020`–`TSP7026` | hints and interop (section 4.2) |
| `TSP7031` | unknown macro: describe it with `:lint-as`, a hook or `:t/macro` |
| `TSP721-01` | the root of a declared Var may be unbound at this read |
| `TSP8001` | `case`/`cond`/multimethod is not exhaustive |
| `TSP9001` | form is not a type |
| `TSP9002` | warning: a spec the checker cannot read (a custom predicate), treated as `t/unknown` |
