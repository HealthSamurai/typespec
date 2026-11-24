# Typespec — a static type checker for Clojure, built for agentic code

> 🇬🇧 English · 🇷🇺 [Русский](README.ru.md)
>
> Status: **a design and a working prototype** (about 2,000 lines, see [Try it](#try-it)). This page holds the design decisions. Details are in [docs/reference.md](docs/reference.md), implementation and milestones in [PLAN.md](PLAN.md).

Typespec checks Clojure code before it runs, the way `tsc` checks TypeScript. It starts from what the code already has: `s/def` for data, `s/fdef` for function signatures, type hints, `:pre` conditions and Java classes. Nothing is added at runtime.

## Why now

Static types never took off in Clojure for one main reason: people did not want to write and maintain annotations. That cost is gone. Most new code is written by LLM agents, and **agents are not lazy**: an agent can annotate a whole codebase and keep the annotations current on every change.

What agents lack is fast feedback. In a dynamic language their typical mistakes stay hidden until the code runs: a misspelled key, a nil nobody expected, arguments in the wrong order, a map of the wrong shape, a library function that does not exist. A type checker reports these in seconds, in a form an agent can act on.

So the deal is:

- **Agents write the types.** Annotation is cheap, so we can ask for a signature on every public function.
- **The type checker judges.** It is the same for every agent and every model, and it cannot be talked into anything.
- **People read less.** Reviewing a signature and a diff of types is faster than reviewing bodies.

Because the annotator is an agent, the design adds guardrails a human-oriented checker would not need:

| Guardrail | What it prevents |
|---|---|
| Annotations are inert data; `annotate` never changes behavior | an agent breaking working code while "just adding types" |
| Trust budget in CI: no growth of `any`, `cast`, `ignore`, trusted guards | an agent silencing the checker instead of fixing the code |
| Weakening is detected (`nilable` added, `:req` removed, signature → `any?`) | an agent "fixing" an error by loosening a type it wrote itself |
| Every autofix has a risk level: `safe`, `review`, `unsafe` | unattended edits that change behavior |
| Types come from schemas first (FHIR Schema, existing specs), guesses last | plausible but invented types |
| One JSON format, stable error ids, errors grouped by root cause | an agent chasing fifty symptoms of one mistake |
| Behavior changes (`:keys!`, new hints) go in a separate PR from annotations | mixed diffs nobody can review |

## What it looks like

```clojure
(ns app.patients
  (:require [clojure.spec.alpha :as s]
            [clojure.string :as str]))

(s/def ::db map?)
(s/def :patient/id     string?)
(s/def :patient/name   string?)
(s/def :patient/status #{"active" "inactive"})
(s/def ::patient (s/keys :req [:patient/id :patient/status] :opt [:patient/name]))

(s/fdef find-patient
  :args (s/cat :db ::db :id :patient/id)
  :ret (s/nilable ::patient))
(defn find-patient [db id]
  (get db id))

(s/fdef greeting
  :args (s/cat :patient ::patient)
  :ret string?)
(defn greeting [{:patient/keys [name status]}]
  (str (case status "active" "Hi, ")
       (str/upper-case name)))

(defn greet-first [db]
  (greeting (find-patient db 42)))
```

```
$ clojure -M:check check
src/app/patients.clj:21:8  TSP8001  case is not exhaustive: "inactive" is not handled
src/app/patients.clj:22:24  TSP2531  name may be nil: :patient/name is optional in ::patient
src/app/patients.clj:25:13  TSP2531  (find-patient db 42) may be nil: find-patient returns (s/nilable ::patient)
src/app/patients.clj:25:30  TSP2345  42 is not assignable to :patient/id (string?)
```

This is the real output of the prototype. The code is plain `clojure.spec`: `s/fdef` is the signature, and nothing was added for Typespec. `greet-first` has no `s/fdef`, so its return type is inferred from the body.

## Try it

Download `typespec.jar` from the [main preview](https://github.com/HealthSamurai/typespec/releases/tag/main-preview)
and run it with Java 25 or newer.

```sh
java -jar typespec.jar check --root path/to/project
```

For Java types and declaration resources supplied by a Clojure CLI project's
dependencies, run from that project with its classpath.

```sh
java -cp "/path/to/typespec.jar:$(clojure -Spath)" typespec.main check
```

Each tested push to `main` updates the preview. To build the same standalone
JAR locally, run `clojure -T:build uber` and use `target/typespec.jar`.

```
git clone https://github.com/HealthSamurai/typespec && cd typespec
clojure -M:check check --root demo        # a small clinic app with 23 planted mistakes
clojure -M:check check --root demo --json
clojure -M:check sig clinic.billing --root demo
clojure -M:check type-at src/clinic/patients.clj:57:9 --root demo
clojure -M:test                           # compares the output with the expected files
```

The [demo](demo/README.md) lists every mistake, what it does at run time today, and what Typespec says. Eight of the 23 fail silently at run time: the code returns a wrong value and nothing is thrown.

**Where the prototype takes types from**, in this order:

| Source | Example | Written for Typespec? |
|---|---|---|
| `s/def` | `(s/def :patient/status #{"active" "inactive"})` | no |
| `s/fdef` | `(s/fdef greeting :args (s/cat :patient ::patient) :ret string?)` | no |
| type hints | `(defn slug ^String [^String title ^long max] ...)` | no |
| `:pre` conditions | `{:pre [(s/valid? ::invoice invoice) (number? rate)]}` | no |
| Java classes on the classpath | `(.plusDays date "30")` | no |
| destructuring patterns | `[& {:keys! [host port]}]` | no |
| the function body | the return type, when nothing declares it | no |
| `{:t/sig '[...]}` | a shorter way to write a signature, for code without `s/fdef` | yes |

**What works in the prototype** (each item is covered by `clojure -M:test`): structural maps and key lookup with suggestions, literal unions and exhaustive `case`, nil tracking and narrowing (`if`, `when`, `if-let`, `cond`, `and`/`or`, `some?`), `->` and `some->`, `map`/`filter`/`reduce` over typed collections, `assoc`/`update`/`get-in`/`select-keys`/`merge`, Clojure 1.13 `:keys!` and `:missing`, hints as signatures, hint checks, Java method calls with overloads, reflection warnings with a fix, unresolved names, `#_(t/cast)`/`#_(t/ignore)`/`#_(t/any)` with trust counters, JSON output, `sig`, `type-at`, `--strict`, `--strict-nil`.

**Also implemented:** bounded rank-1 generics (`t/all`), contextual callbacks, named refs and structural map constraints; local EDN extern declarations; opt-in binding-body macro contracts without macro expansion. See [solver usage](docs/solver-usage.md) and the verified [context-clj HTTP + PostgreSQL example](examples/context-web-pg/README.md).

**Type declarations:** the 64 built-in signatures now live in `resources/typespec/types/clojure/core.edn` (51) and `string.edn` (13). `core_types.clj` loads them strictly through the classpath; there is no second signature table. Users can supply the same EDN format through `:externs ["types/vendor.edn"]`. Explicit user declarations from JAR resources are planned, not implemented. See [declaration files and distribution](docs/type-declarations.md) and the [clojure.string audit](docs/clojure-string-audit.md).

**Regression checks.** `clojure -M:test` runs the checker suites, golden files, CLI checks, and the positive and negative static checks for [context-web-pg](examples/context-web-pg/README.md). With `JAVA_HOME` set to a JDK 25 or newer, run the acceptance checks on pinned upstream sources:

```sh
sh examples/context-pg-check/acceptance.sh
sh examples/context-http-check/run.sh --accept
```

These check all nine production source files in `pg` and all eight in `http`. The `pg` check preserves two known error locations, and the `http` check compares exact diagnostics with a reviewed snapshot. [pg results](docs/context-pg-check.md) · [http results](docs/context-http-check.md)

The Ring Core audit checks all 28 Clojure sources in the pinned release with the standard `check` command. [Ring Core results and reproduction](docs/ring-core-check.md)

**What does not work yet:** higher-rank generics and general generic overload inference, `reify`/`proxy`/protocols/multimethods, arbitrary project macro expansion, automatic declaration discovery and library-version matching, per-namespace caching and the daemon, `fix` and `annotate`, the trust budget in CI. Undescribed project macros remain incomplete; configured macro contracts are trusted assumptions, not verification of the macro implementation. Java classes are read by reflection, not from bytecode. JVM arrays in `join` and regex-group-specific callback inference remain outside the string contracts' supported scope.

**First run on real code.** Three modules of an internal library (5,800 lines, almost no specs) check in 1–4 seconds each, JVM start included. The result is 1 error and 39 warnings. The error is real: a `BufferedReader` is passed to a parameter hinted `^InputStream`, a `ClassCastException` at run time. The warnings are reflective calls and undescribed project macros. Code without specs gives the checker little to work with, so little is reported; getting to zero false errors took about ten fixes to the checker.

## More examples

Every `today` line was run on Clojure 1.13.0-alpha11. The prototype already reports all the `TSP` lines below except the one for `reify` (see `test/examples` and the demo); the wording of some messages differs.

### Type hints are types

Hints you already have work as a signature, and the checker tells you when a hint is wrong or in the wrong place.

```clojure
(defn slug ^String [^String title ^long max]          ; the hints are the signature
  (subs (.toLowerCase title) 0 (min max (.length title))))

(slug 42 "hello")
;;    ^^ TSP2345: 42 is not assignable to String
;;       ^^^^^^^ TSP2345: "hello" is not assignable to long
;; today: ClassCastException at run time

(defn ^long total [xs] (reduce + xs))
;;    ^^^^^ TSP7021: a hint on the name is not a return type; write (defn total ^long [xs] ...)
;; today: every caller fails to compile: "Unable to resolve classname: clojure.core$long@…"

(defn first-len
  {:t/sig '[(s/coll-of int?) :-> int?]}
  [ids]
  (let [^String s (first ids)]
;;      ^^^^^^^ TSP2322: hint String contradicts the type: int?
    (.length s)))
;; today: ClassCastException: Long cannot be cast to String
```

### Java interop

Java types are read from bytecode, so interop is checked without any declarations. Because the checker knows types the compiler does not see, it can also insert the hints that remove reflection.

```clojure
(defn size
  {:t/sig '[java.io.File :-> long]}
  [f]
  (.length f))
;; TSP7023: reflective call, but f is known to be java.io.File
;;          fix (safe): (.length ^java.io.File f)
;; today: a reflection warning and a slow call; (size "abc") quietly returns 3

(defn request-id-length ^long [^java.util.Map headers]
  (.length ^String (.get headers "x-request-id")))
;; TSP2531: (.get headers "x-request-id") may be nil: java.util.Map.get may return null
;; today: NullPointerException when the header is absent

(def by-pair
  (reify java.util.Comparator
    (compare [_ a b] (str a b))))
;;                   ^^^^^^^^^ TSP2322: String is not assignable to int, the return type of Comparator.compare
;; today: compiles; ClassCastException on the first comparison
```

### Destructuring

Destructuring is checked against the declared type, so a typo in a key is an error and not a silent nil.

```clojure
(defn display-name
  {:t/sig '[::patient :-> string?]}
  [{:patient/keys [id nmae]}]
;;                    ^^^^ TSP2339: :patient/nmae is not a key of ::patient. Did you mean :patient/name?
  (str id " " nmae))
;; today: returns "p1 " and nobody notices

(s/def :job/retries (s/nilable int?))

(defn next-try
  {:t/sig '[(s/keys :opt [:job/retries]) :-> int?]}
  [{:job/keys [retries] :or {retries 0}}]
  (inc retries))
;;     ^^^^^^^ TSP2531: retries may be nil: :job/retries is (s/nilable int?), and :or applies only when the key is absent
;; today: (next-try {:job/retries nil}) throws NullPointerException
```

### Clojure 1.13: `:keys!`, required keyword arguments, `:missing`

The new directives throw at run time when a key is missing. Typespec reports the same thing at the call site, before anything runs.

```clojure
(s/def :address/city string?)
(s/def :contact/address (s/keys :req [:address/city]))
(s/def ::contact (s/keys :req [:contact/id] :opt [:contact/address]))

(defn city
  {:t/sig '[::contact :-> string?]}
  [{{:address/keys! [city]} :contact/address}]
;;                          ^^^^^^^^^^^^^^^^ TSP2741: :keys! below requires :contact/address, but it is optional in ::contact
  city)
;; today: (city {:contact/id "c1"}) throws "Missing required key: :address/city"

(defn- connect [& {:keys! [host port]}] ...)

(connect :host "localhost")
;; TSP2741: required key :port is missing
;; today: IllegalArgumentException: Missing required key: :port

(defn- register [{:user/keys! [email name] :missing m}]
  (if m
    {:error m}                    ; here email and name may be nil
    (send-welcome email name)))   ; here both are narrowed back to non-nil
```

### Maps and narrowing

```clojure
(assoc patient :patient/status "archived")
;;                             ^^^^^^^^^^ TSP2322: "archived" is not assignable to :patient/status (#{"active" "inactive"})
;; today: the map now holds a status that no code expects

(get-in bundle [:bundle/entry 0 :entry/resourse])
;;                              ^^^^^^^^^^^^^^^ TSP2339: no such key. Did you mean :entry/resource?
;; today: nil

(defn label
  {:t/sig '[(s/nilable ::patient) :-> string?]}
  [p]
  (if p (:patient/id p) "unknown"))     ; ok: p is narrowed to ::patient inside the branch
```

### What an agent gets

Diagnostics as JSON, with a fix the agent can apply without thinking, and commands to ask about types instead of guessing. This is real output for the demo.

```json
{"id": "a34443c4", "code": "TSP7023", "severity": "warning", "file": "src/clinic/export.clj",
 "range": {"start": [39, 3], "end": [39, 17]},
 "message": "reflective call, but file is known to be java.io.File",
 "fixes": [{"title": "Add hint ^java.io.File", "risk": "safe",
            "edits": [{"range": {"start": [39, 12], "end": [39, 12]}, "text": "^java.io.File "}]}]}
```

```
$ clojure -M:check type-at src/clinic/patients.clj:57:9 --root demo
(s/nilable :clinic/patient)

$ clojure -M:check sig clinic.billing --root demo
line-total     [:clinic/line :-> number?]   ; from s/fdef
total          [:clinic/invoice :-> number?]   ; from s/fdef
new-invoice    [:invoice/id :clinic/patient :-> :clinic/invoice]   ; from s/fdef
with-tax       [:clinic/invoice number? :-> number?]   ; from the body
...
```


## Typespec and TypeScript

`+` Typespec is ahead by design, `=` the same idea, `−` TypeScript is ahead. The Typespec column describes the design; the prototype covers part of it (see [Try it](#try-it)).

| | TypeScript | Typespec | |
|---|---|---|:-:|
| Where types are written | syntax of the language, erased by the compiler | quoted data in metadata, `s/def`, EDN; nothing to erase | = |
| Shape of an object | `interface`, `type` | `s/keys` | = |
| Type of a field | declared per interface | declared per key, once for the whole project | + |
| Structural typing, open objects | yes | yes | = |
| Extra-key check on literals | yes | yes | = |
| Exact (closed) objects | no | `t/exact` | + |
| Unions and literal types | `"a" \| "b"` | `#{"a" "b"}`, `s/or` | = |
| Intersections | `A & B` | `s/merge`, `t/and` | = |
| Null safety | `strictNullChecks` | `s/nilable`; rules for nil-punning | = |
| Narrowing | `typeof`, `in`, `instanceof`, type predicates | `if`/`when`/`cond`/`case`, `some?`, `instance?`, `contains?`, checked guards | = |
| Exhaustiveness | the `never` idiom | built in: `case`, `cond`, closed multimethods | + |
| Generics with bounds | `<T extends X>` | `(t/all [(a :< X)] ...)` | = |
| Utility types | `Pick`, `Omit`, `Partial`, `keyof`, `T[K]` | `t/pick`, `t/omit`, `t/partial`, `t/keys-of`, `t/get` | = |
| Conditional, mapped, template literal types | yes | no | − |
| `unknown` vs `any` | yes | `t/unknown` vs `any?` with a reason | = |
| `as const` | yes | `#_(t/const)` | = |
| Escape hatches | `as`, `@ts-ignore`, `@ts-expect-error` | `#_(t/cast)`, `#_(t/ignore "reason")`, counted by the trust budget | + |
| Overloads | yes | several signatures in one `t/sig` | = |
| Inference | local, contextual for callbacks | local; contextual for `map`/`filter`/`reduce`; partial for `comp`, `partial`, transducers | − |
| Modules | file; `isolatedDeclarations` | namespace; public signatures required in strict mode | = |
| Types for libraries | `.d.ts`, DefinitelyTyped | EDN externs, `typespec.exports/` in jars; no ecosystem yet | − |
| Platform types | `lib.d.ts`, maintained separately from the runtime | Java types read from bytecode | + |
| Nominal types | no (brands by convention) | records and Java classes; `t/brand` (optional) | + |
| Runtime validation | a separate tool (zod, io-ts) | the same `s/def` validates at run time | + |
| Mutation and variance | `readonly`, known unsound spots | data is immutable; atoms and channels are invariant | + |
| Macros | none in the language | must be described (`:lint-as`, hooks); weaker inside custom DSLs | − |
| Dynamic code | `eval` is rare | REPL redefinition, `binding`, `resolve` are common; typed as `t/unknown` | − |
| Data DSLs | typed builders and JSX are common | honeysql, hiccup, Integrant configs are typed as plain data, not by meaning | − |
| Keys built at run time | template literal types | `t/unknown` or `s/map-of` | − |
| Editor | tsserver | through clojure-lsp, planned | − |
| Incremental checking | `--incremental`, project references | interface cache per namespace, parallel checking | = |
| Gradual adoption | `allowJs`, strictness flags | `:strict`, baseline file | = |
| Support for agents | text diagnostics, language server | JSON with stable ids and root causes, risk-rated fixes, `sig`/`type-at`/`order`, trust budget | + |
| Maturity | 12+ years, huge ecosystem | a design and a prototype | − |

The `−` rows are the honest limits. Some are permanent: the checker sees files and not the running REPL image, macros must be described, and data DSLs get their meaning only when interpreted. Others are a matter of work: distribution of externs with libraries, the editor, inference for `comp` and transducers.

## Design decisions

Each decision has a default. Where a real project may need something else, there is an option.

**1. No runtime.** Typespec adds no checks, no wrappers and no dependency. Signatures are quoted data in var metadata; escape hatches are `#_` comments that Clojure discards.
*Option:* `:signatures :sidecar` keeps every signature in `typespec/types.edn` and leaves source files untouched.

**2. One type language, and it is data.** A subset of spec forms plus a few `t/` forms. The same notation is used in `s/def`, signatures, externs, error messages and JSON, so whatever the checker prints can be pasted back into code. Anything outside the subset is an error, never a silent `any?`.

**3. Key = type.** A qualified key such as `:patient/id` means the same thing everywhere. This is the core idea of spec, and it is why renaming and typo detection work across a whole project.
*Option:* declare a key in `s/def` (real spec, also usable at runtime) or type-only in `typespec/types.edn`.

**4. Signatures at boundaries, inference within.** A public function has a signature: an `s/fdef`, type hints, or `{:t/sig '[...]}`. Existing annotations come first; `t/sig` is only a shorter notation for code that has none. Bodies and private functions are inferred locally. There is no inference across call chains. This is the TypeScript model, and it is what makes checking fast and errors local.
*Option:* `:strict false` plus a baseline file for gradual adoption: old code is not blocked, new errors are.

**5. Read source, do not load it.** Declarations come from the text of files. Checking a namespace does not run its top-level code, so there are no side effects and no stale registry.

**6. Macros are described, not executed.** Core macros have built-in rules. Project macros are described the clj-kondo way: `:lint-as` or a small hook. An undescribed macro is an explicit `TSP7031`, and its body is `t/unknown`.
*Option:* `:macros {:expand true}` expands unknown macros in a sandboxed worker process.

**7. Type hints are types.** `^String`, `^long` and `^[String]` are read as types; a fully hinted function needs no other signature. The checker reports hints that are wrong, missing or misplaced, and can insert proven hints that remove reflection. Hints are compiler input, so Typespec inserts them only through an explicit `fix`.

**8. Java types come from bytecode.** Classes, subtyping, overloads, generics and nullability annotations are read from the classpath. No declarations are needed for Java.
A hinted value or a Java result is a *platform type*, as in Kotlin: nil is not checked, but the place is counted.
*Option:* `:strict-nil true` turns platform types into `T | nil`.

**9. Maps are structural and open.** A map type lists the keys it needs and accepts extra ones. Reading an undeclared key is an error. Literals are checked for extra keys against the declared type. `t/exact` closes a map.

**10. Destructuring is checked, not trusted.** With a declared type, destructuring is checked against it. Clojure 1.13 `:keys!` and friends are understood, but they are code that changes behavior, so Typespec never adds or removes them.

**11. A namespace is a module.** A namespace is checked against the *signatures* of its dependencies, not their bodies. This gives caching, parallel checking and a fast editor loop.

**12. Libraries get externs.** Types are inert EDN declaration files. Local files already work; built-in declarations are classpath resources. User-library JAR declarations and separately packaged declarations (similar to `.d.ts` / `@types`) are the next distribution step, not automatic discovery already available. See [the decision and current status](docs/type-declarations.md).

**13. Honest gaps.** Everything unproven is visible and counted: `t/unknown`, `any?` with a reason, casts, externs, platform types. The trust budget keeps that number from growing.

**14. TypeScript first.** Where TypeScript has an answer, we take it: structural types, narrowing, literal unions, `unknown` vs `any`, `isolatedDeclarations`, error codes. A few ideas from OCaml and F# (type providers, brands, row polymorphism) are optional extensions, each behind a flag.

## Next to existing tools

| Tool | What it is | Why it is not enough here |
|---|---|---|
| clojure.spec, malli | runtime validation and generation | checks happen when code runs |
| clj-kondo | fast static linter that reads source | types are shallow: arity and basic type mismatches |
| [crabjure](https://github.com/ribelo/crabjure) | a clj-kondo-style linter in Rust, a proof of concept, about 20× faster than clj-kondo by its own benchmarks | a linter; its type checker is a gated experiment with types written in comments (`@param`, `@return`) |
| Typed Clojure | a real static type system (a port of Typed Racket) | loads the code it checks, has its own type syntax, one maintainer |

Typespec takes the reading model from clj-kondo, the type system core from Typed Clojure, the type notation from spec, and the product decisions from TypeScript.

## Documents

- [Feature cards](features/README.md) — accepted Flow-inspired proposals, examples, implementation status and open questions.

- [docs/reference.md](docs/reference.md) — type language, sources of types, type system rules, hints and Java, externs, escape hatches, configuration, CLI and JSON, optional extensions, error codes.
- [demo/README.md](demo/README.md) — the demo project: 23 mistakes, what they do at run time, what Typespec reports.
- [PLAN.md](PLAN.md) — architecture, implementation choices, what already exists, milestones, open questions.
