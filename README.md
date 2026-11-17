# Typespec 1.0 — a static type checker for Clojure, designed from scratch

> 🇬🇧 English · 🇷🇺 [Русский](README.ru.md)
>
> Status: **design document**, no code yet. Incorporates 4 independent reviews and experiments in a Typed Clojure fork ([niquola/typedclojure@ts-features](https://github.com/niquola/typedclojure/tree/ts-features)).

## 0. Principles

1. **Zero runtime impact.** Typespec does not change a single byte of compiled code. Annotations are data that the compiler skips.
2. **One type language.** The same notation is used in `s/def`, signatures, externs, error messages, and JSON. Whatever the type checker prints can be pasted into code.
3. **Key = type.** A qualified key such as `:patient/id` means the same thing everywhere: whatever `(s/def :patient/id ...)` declares. This is spec's core idea, and Typespec sticks to it.
4. **Signatures at boundaries, inference within.** Public functions have declared signatures; their bodies are inferred locally. No inference “across three calls”—this is how TS works, and why it is fast.
5. **Read source, don't load it.** Declarations come from file contents. Project code is not executed (except project macros; see section 7).
6. **Honesty.** Everything the type checker has not proven is visible: `Unknown`, `Any` with a reason, `cast`, externs. CI counts these gaps.
7. **Agents write, the type checker judges.** Annotations are designed to be added at scale by LLM agents; humans review diffs and errors.

---

## 1. Type language

These are **spec-style data**: a subset of spec forms plus a few `t/` forms. These forms are not evaluated; only the type checker reads them.

```clojure
string?  keyword?  boolean?  uuid?  inst?            ; basic types
int?     double?   number?                           ; int? = long-like values, not arbitrary numbers
nil?     any?      t/unknown  t/never                ; any? = explicit opt-out, unknown = “narrow first”
#{"active" "inactive"}  #{:get :post}                 ; literal unions
(s/nilable T)   (s/or :a A :b B)                     ; union
(s/keys :req [:patient/id] :opt [:patient/name])     ; map with qualified keys (structural)
(s/keys :req-un [:patient/id])                       ; unqualified key :id, type from :patient/id
(s/coll-of T :kind vector?)  (s/map-of K V)  (s/tuple A B)
(s/merge A B)                                        ; input must satisfy both = intersection of requirements
(t/fn [A B :-> R])                                   ; function
(t/exact (s/keys ...))                               ; no extra keys
(t/and A B)  (t/pick T :k ...)  (t/omit T :k ...)  (t/partial T)  (t/keys-of T)  (t/get T :k)
(t/class java.time.Instant)                          ; Java class
(t/atom T)  (t/chan T)                               ; mutable types are invariant
```

Type variables are declared **explicitly** in signatures (section 3). They are not needed in `s/def`.

Rules the type checker makes explicit:

- `s/and` yields the type of its first component. Other predicates are treated as **refinements**, meaning documentation (`pos?`, regexes), and are not checked statically.
- A form outside the subset produces `TSP9001 not a type`, not a silent `Any`.
- Recursion is allowed: `(s/def :fhir/extension (s/keys :opt-un [:fhir/extension ...]))` is a recursive type, not `Any`.

---

## 2. Where types come from

| What | Source | Where it is written |
|---|---|---|
| value of key `:ns/k` | `(s/def :ns/k T)` | alongside domain code (`specs.clj`) |
| entity shape | `(s/def ::patient (s/keys ...))` | same place |
| public function signature | `t/sig` at the start of `defn` or `s/fdef` | next to the function |
| private function signature | not required; inferred locally | — |
| local values | inference | — |
| third-party libraries | externs | `typespec/externs/` |

### 2.1 `t/sig` — signatures as data

```clojure
(defn find-patient
  {:t/sig '[Ctx :patient/id :-> (s/nilable ::patient)]}
  [ctx id]
  ...)
```

This is a regular `defn` attr-map. The type is **quoted**, so the compiler simply stores it in var metadata without executing anything. Typespec reads it from source. Multiple arities and overloads are written as follows:

```clojure
{:t/sig '([:fhir/id :-> (s/nilable :fhir/resource)]
          [:fhir/type :fhir/id :-> (s/nilable :fhir/resource)])}
```

`s/fdef` is also supported for code that already uses it: the type checker reads the `s/fdef` form from source. If a function has both `t/sig` and `s/fdef`, they must match; otherwise, it produces `TSP2790`.

Why not `^{:t ...}` on parameters? Metadata on a map-shaped parameter is lost during `fn` expansion, while unquoted metadata in an attr-map is evaluated. One quoted signature per function means one place, with no exceptions.

### 2.2 Destructuring — a check, not a source

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

In strict mode, a qualified key without an `s/def` produces `TSP2304: no spec for :patient/birthDate`. This catches typos even without a declared type.

An unqualified key (`{:keys [id]}`) gets its type only from a declared `s/keys :req-un`. Without a declaration, its type is `t/unknown`.

### 2.3 `:keys!` and other Clojure 1.13 directives

`:keys!`, `:req!`, `:select`, `:excess`, `:missing`, and `:all` are **code**: they change behavior (`req!` throws an exception). Typespec never adds or removes them automatically. If they are already in the code, Typespec understands them:

| Directive | Type |
|---|---|
| `:keys! [a]` | the key is required in the input. Any nested `!` makes the **entire path** to it required and non-nil (`req!` on nil throws). The value has its spec type; nil remains if the spec allows it |
| `:keys [a]` | value is `T \| nil`; a parameter using only `:keys` has type `Map \| nil` |
| `:or {a d}` | `T \| typeof(d)`, but an explicit `{:a nil}` yields `nil`; `:or` on a `!` key is a Clojure compilation error, which Typespec also reports |
| `[a & :b :c]` | `:b :c` participate in checks without creating bindings; they are also required in `:keys!` |
| `:select s` | `(t/pick (merge defaults Arg) ks)`, recursively through nested forms; `\| nil` if Arg is nilable; record → plain map |
| `:excess e` | recursive remainder: unmentioned keys plus nested `{:k nested-excess}`, `\| nil` |
| `:missing m` | `!` keys become optional for the caller; bindings are `T \| nil`; narrowed after `(when-not m ...)` |
| `:all a` | `(merge defaults Arg)`, with the argument taking precedence |
| `(selector f)` | one directive → its type; multiple directives → `{:select .. :excess ..}` without nil values |
| `[& {:keys! [a]}]` | required keyword arguments |

The table is validated with a **differential test**: a generator builds random forms and values, then compares the predicted type with the result of the actual `destructure`. The Clojure version is pinned in the config: the 1.13 syntax is still in alpha, `:defaults` has already been removed, and `:req!` was added later.

Implementation: precise rules for the functions the form expands into (`req!`, `get`, `select-keys`, `merge`, `dissoc`, `some-vals`), plus a syntax-level rule for clear messages. If the rule and expansion disagree, that is a type checker bug, caught by the test.

---

## 3. Type system

- **Maps are structural.** `(s/keys :req [:a])` accepts a map with any additional keys. **Reading** a key absent from both the type and the spec registry is an error (TS2339). Reading from `nil` yields `T | nil`.
- A **fresh literal** `{...}` in a call is checked for extra keys **only against the declared type**. An extra qualified key without an `s/def` is an error; a key that looks like a typo produces an error with Did you mean.
- **`t/exact`** forbids extra keys everywhere.
- **Map operations** are typed precisely: `assoc`, `dissoc`, `update`, `merge`, `select-keys`, `get-in`/`assoc-in`/`update-in` with literal paths, and keywords used as functions in `map`/`filter`.
- **Generics** with explicit binders and constraints:
  ```clojure
  {:t/sig '(t/all [a (r :< :fhir/resource)] [(s/coll-of r) (t/fn [r :-> a]) :-> (s/coll-of a)])}
  ```
  Immutable collections are covariant; `atom`, `volatile`, arrays, and channels are invariant.
- **Literals**: `1` in `let` widens to `int?`; in a literal-union position, it remains a literal; `(t/const {...})` is analogous to `as const`.
- **Narrowing** after `if`/`when`/`cond`/`case`/`when-let`, `some?`/`nil?`/type predicates/`instance?`/`=` with a literal/`contains?` on a map. Custom guard functions can be checked (narrowing is inferred from the body) or trusted via `t/guard!` (included in the report).
- **Exhaustiveness**: `case` over literal unions, `cond`/`condp` using `=` on a single variable, and `defmulti` with closed dispatch (`{:t/closed true}`). A `cond` without `:else` yields `T | nil`; this is not an error.
- **nil-punning**: `(first nil)` → `nil`, `(seq x)` narrows to a non-empty collection, `(:k x)` on `t/unknown` produces a “narrow first” error.
- **String keys** (JSON before keywordization): `(t/keys-str {"id" string?})`.
- **Map tail**: `(t/keys {:req [...]} :rest (s/map-of keyword? any?))`.

### Clojure specifics

| Construct | Rule |
|---|---|
| `defrecord R [a b]` | nominal class + fields + open extra keys; `assoc` preserves `R`, `dissoc` of a field yields a map |
| `defprotocol` | `{:t/sig ...}` on methods; every `extend-*` is checked, including those in other namespaces |
| `defmulti` | the dispatch function is typed; if it is a keyword (`:resourceType`), the argument in `defmethod` is narrowed by value |
| `atom` | `(t/atom T)`; `swap!` requires `[T ... :-> T]`; narrowing of `@a` does not survive a second deref |
| `core.async` | `(t/chan T)`; `<!` → `T \| nil`; `go`/`go-loop` have built-in rules |
| `binding`, `eval`, `resolve` | `t/unknown` |

---

## 4. Java and JVM hints

- `:tag` controls **compilation**; it is not a Typespec type. Hints are optional: the type checker neither requires nor inserts them.
- The type checker interprets `^String x` as `(t/class String)` **including nil**: the compiler does not prohibit nil, and plenty of code relies on this. Only primitives (`^long`) and signatures establish non-nil types.
- Java method results default to **platform types**, as in Kotlin: nil is not checked, but the site is included in coverage. `:strict-interop true` makes the result `T | nil`. Bytecode annotations `@Nullable`/`@NonNull` are respected.
- Reflection produces a separate warning, `TSP7020`. The “add `^Class`” autofix is classified as `review`: it can change Java overload resolution.

---

## 5. Modules and performance

This is what makes TS fast, and Typespec copies it directly.

1. **Public var signatures form the ns interface.** Ns `B` is checked against the **signatures** of `A`, not its bodies. Editing a function body in `A` does not trigger rechecking `B`.
2. **Strict mode requires signatures for public functions** (like `isolatedDeclarations` in TS 5.5). This allows an ns interface to be computed from a single file's text, without checking bodies, and all namespaces to be checked **in parallel**.
3. **Interface cache**: `.typespec/cache/<ns>.edn`, keyed by a hash of the source and dependency interfaces.
4. **Daemon** (`typespec watch`, LSP) keeps a warm JVM and the ns graph in memory. The target is <300 ms per save for a medium-sized file; this is a **CI benchmark**, not a promise.
5. **Interface generation** for Clojure libraries: `typespec emit` places `.typespec/<lib>.edn` in the jar, analogous to `.d.ts`.

---

## 6. Third-party libraries — externs

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
- Lookup order: local externs → interface bundled in the library jar (`emit`) → `s/fdef` from library source → shared `typespec-externs` repository → `t/unknown`.
- `typespec extern next.jdbc` produces an agent-generated draft based on arglists, docstrings, source, and tests. `--verify` runs the library's tests with `instrument` and `:ret` checking. This is **sample-based** validation, not proof.

---

## 7. How Typespec reads code

1. **Reader**: tools.reader with positions and comments, without execution. This supplies `s/def`, `t/sig`, `s/fdef`, and `ns`/`require`.
2. **Core macros** (`let`, `fn`, `for`, `doseq`, `->`, `cond->`, `go`, `with-open` …) are expanded using the type checker's **built-in rules**. Errors are reported in terms of the original form.
3. **Project and library macros** are expanded in a separate **worker process** with no network access and read-only access to the project. Only namespaces defining macros and their dependencies are loaded. Trust boundary: “macros are trusted code.”
4. A macro can be described without expansion: `{:t/macro '[...]}` in externs. In that case, the type checker does not use the worker process.
5. A deleted `s/def` disappears from the model immediately, because there is no in-memory registry: only source text.

---

## 8. Suppression and trust

```clojure
(t/ignore "AIDBOX-123: legacy" (legacy-call x))     ; macro → simply (legacy-call x)
(t/cast ::patient row)                              ; → row
(t/any "dynamic plugin api")                        ; type any? with a reason
```

`t/ignore`, `t/cast`, and `t/any` are identity macros from a tiny runtime library (about 20 lines, no dependencies). An unused `ignore` produces a warning (like `@ts-expect-error`).

**Trust budget**: CI fails if a PR increases the number of `any`, `cast`, `ignore`, `guard!`, or platform types, or weakens a type. Weakening includes adding `nilable`, removing a `:req`, changing `:keys!` → `:keys`, or replacing a signature with `any?`.

---

## 9. Configuration

```clojure
;; typespec.edn
{:paths        ["src"]
 :clojure      "1.13.0-alpha11"     ; destructuring semantics
 :externs      ["typespec/externs"]
 :strict       true                 ; public fn signatures, missing key spec = error, no implicit unknown
 :strict-interop false              ; Java method results are platform types
 :baseline     ".typespec/baseline.edn"  ; existing errors do not block; new ones do
 :ignore-ns    [aidbox.legacy.*]}
```

---

## 10. For agents

```bash
typespec check src --json            # errors
typespec sig app.patients            # inferred and declared ns signatures
typespec type-at src/app/p.clj:14:9  # expression type (hover)
typespec annotate app.patients       # draft t/sig and s/def — data only, no behavior changes
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

1. FHIR Schema (generating `s/def` for Aidbox resources);
2. existing spec and malli definitions;
3. test traces (recorded actual values → draft);
4. code and docstrings.

**Agent loop:** `order` → `annotate` → `check --json` → edit types or code → repeat. Bug fixes and `:keys` → `:keys!` changes go in a separate PR.

---

## 11. What already exists and how to build it

| Layer | Foundation | Status |
|---|---|---|
| Core: narrowing, generics, unions, body inference | Typed Clojure (a port of Typed Racket) | available |
| TS-style errors, Did you mean, Why | `ts-features` fork | available |
| Pick/Omit/Partial/Required, exhaustive `case` | fork | available |
| spec forms → types | fork `74c55cc`, reads the registry | partial; needs to read source |
| `req!`, nested destructuring | fork `c0f507b` | partial |
| reader-first declarations, `t/sig`, EDN externs | — | new |
| ns interfaces, cache, parallelism, daemon, LSP | — | new; the bulk of the work |
| JSON, fix, coverage, baseline | — | new |

**Plan (estimate):**

1. **M1, 4–6 weeks.** Reader-first declarations, `t/sig`, the type language from section 1, JSON errors. Goal: `app2` and context-clj type-check without `t/ann`.
2. **M2, 4–6 weeks.** Ns interfaces, cache, parallel checking, `watch`. Plus externs: next.jdbc, ring, http-kit, aidbox-sdk.
3. **M3, 6–8 weeks.** Clojure 1.13 destructuring with differential testing, records, protocols, multimethods, recursive FHIR types from FHIR Schema.
4. **M4.** LSP, `annotate`, and the agent loop on a real Aidbox module; trust budget in CI.
---

## 12. Optional: ideas from OCaml / F#

The foundation is **TypeScript**, and we stick to its ideas as much as possible. The ideas below are **optional** extensions. Each one is enabled by a flag in `typespec.edn` and is mapped to its closest TS counterpart, so we stay within the TS model.

| Idea | From | Closest in TS | Flag |
|---|---|---|---|
| Types from external schemas | F# type providers | `.d.ts` codegen (openapi-typescript) | `:providers` |
| Value tags | F# units of measure, OCaml phantom types | branded types `string & {__brand: "PatientId"}` | `:brands` |
| Open maps with a row tail | OCaml row polymorphism | generic `T extends {id: string}` → `T` | `:rows` |
| Counterexample for a missing case | OCaml exhaustiveness | `never` check in `switch` | `:exhaustive-examples` |
| Error-tolerant analysis for IDEs | OCaml Merlin | tsserver | — (part of LSP) |

**12.1 Type providers.** `s/def`s are generated at check time from FHIR Schema, a DB schema, OpenAPI or sample JSON. The output lives in the cache and is not committed:

```clojure
{:providers [{:kind :fhir-schema :package "hl7.fhir.r4.core@4.0.1" :ns :fhir}
             {:kind :openapi :file "api/openapi.yaml" :ns :api}]}
```

**12.2 Brands.** Tags exist only during checking and are erased, as in TS:

```clojure
(s/def :patient/id   (t/brand string? :patient/id))
(s/def :encounter/id (t/brand string? :encounter/id))
(find-patient ctx (:encounter/id e))
;; TSP2345: :encounter/id is not assignable to :patient/id
```

Units of measure are a special case: `(t/brand double? :unit/mg)`.

**12.3 Row polymorphism.** A function can take a map with the keys it needs and return "the rest" without `Any`. This is what generics with a constraint and `T & {...}` give you in TS:

```clojure
{:t/sig '(t/all [(m :< (s/keys :req [:db/id]))] [m :-> (t/and m (s/keys :req [:db/version]))])}
```

**12.4 Counterexample in exhaustiveness errors.** The error shows a concrete missing case: `TSP8001: case is not exhaustive, e.g. "inactive"`, and for nested maps, `{:status "active" :kind nil}`.

**Not adopted:** full Hindley–Milner inference without signatures (contradicts "signatures at boundaries" and works poorly with maps and macros); functors and GADTs (complexity without payoff for Clojure applications).
