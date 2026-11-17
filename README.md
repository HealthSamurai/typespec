# Typespec — a static type checker for Clojure, built for agentic code

> 🇬🇧 English · 🇷🇺 [Русский](README.ru.md)
>
> Status: **design document, no code yet.** This page holds the design decisions. Details are in [docs/reference.md](docs/reference.md), implementation and milestones in [PLAN.md](PLAN.md).

Typespec checks Clojure code before it runs, the way `tsc` checks TypeScript. Types are written as spec-style data, type hints and Java classes are understood as they are, and nothing is added at runtime.

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

(defn find-patient
  {:t/sig '[::db :patient/id :-> (s/nilable ::patient)]}
  [db id]
  ...)

(defn greeting
  {:t/sig '[::patient :-> string?]}
  [{:patient/keys [name status]}]
  (str (case status "active" "Hi, ")
       (str/upper-case name)))

(defn greet-first
  {:t/sig '[::db :-> string?]}
  [db]
  (greeting (find-patient db 42)))
```

```
src/app/patients.clj:19:8   TSP8001  case is not exhaustive: "inactive" is not handled
src/app/patients.clj:20:24  TSP2531  name may be nil: :patient/name is optional in ::patient
src/app/patients.clj:25:13  TSP2345  (s/nilable ::patient) is not assignable to ::patient
src/app/patients.clj:25:30  TSP2345  42 is not assignable to :patient/id (string?)
```

The specs are ordinary spec. The signature is a quoted vector in the `defn` attr-map: the compiler stores it and does nothing with it.

## Design decisions

Each decision has a default. Where a real project may need something else, there is an option.

**1. No runtime.** Typespec adds no checks, no wrappers and no dependency. Signatures are quoted data in var metadata; escape hatches are `#_` comments that Clojure discards.
*Option:* `:signatures :sidecar` keeps every signature in `typespec/types.edn` and leaves source files untouched.

**2. One type language, and it is data.** A subset of spec forms plus a few `t/` forms. The same notation is used in `s/def`, signatures, externs, error messages and JSON, so whatever the checker prints can be pasted back into code. Anything outside the subset is an error, never a silent `any?`.

**3. Key = type.** A qualified key such as `:patient/id` means the same thing everywhere. This is the core idea of spec, and it is why renaming and typo detection work across a whole project.
*Option:* declare a key in `s/def` (real spec, also usable at runtime) or type-only in `typespec/types.edn`.

**4. Signatures at boundaries, inference within.** Public functions declare a signature; bodies and private functions are inferred locally. There is no inference across call chains. This is the TypeScript model, and it is what makes checking fast and errors local.
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

**12. Libraries get externs.** Types for third-party Clojure libraries are versioned EDN files, never loaded as code. Libraries can ship their own inside the jar, like `.d.ts`.

**13. Honest gaps.** Everything unproven is visible and counted: `t/unknown`, `any?` with a reason, casts, externs, platform types. The trust budget keeps that number from growing.

**14. TypeScript first.** Where TypeScript has an answer, we take it: structural types, narrowing, literal unions, `unknown` vs `any`, `isolatedDeclarations`, error codes. A few ideas from OCaml and F# (type providers, brands, row polymorphism) are optional extensions, each behind a flag.

## Limits

Some things TypeScript covers cannot be covered in Clojure, and the design does not pretend otherwise.

| Limit | What happens |
|---|---|
| REPL-driven change: redefinition, `binding`, `eval`, `resolve` | the checker sees files, not the running image; dynamic values are `t/unknown` |
| Arbitrary macros | checked only when described or expanded; errors inside a custom DSL are weaker |
| Data DSLs (honeysql, hiccup, Integrant configs) | typed as data structures, not by meaning |
| Keys built at runtime | `t/unknown` or a `map-of` |
| Type-level programming (conditional, mapped, template types) | only the common cases: `pick`, `omit`, `partial`, `keys-of` |
| `comp`, `partial`, `apply`, transducers | partial support through overloads |
| Ecosystem of ready-made types | none yet; externs are written by us and by agents |

Some things are easier than in TypeScript: data is immutable, qualified keys are global, and Java types are already there in bytecode.

## Next to existing tools

| Tool | What it is | Why it is not enough here |
|---|---|---|
| clojure.spec, malli | runtime validation and generation | checks happen when code runs |
| clj-kondo | fast static linter that reads source | types are shallow: arity and basic type mismatches |
| Typed Clojure | a real static type system (a port of Typed Racket) | loads the code it checks, has its own type syntax, one maintainer |

Typespec takes the reading model from clj-kondo, the type system core from Typed Clojure, the type notation from spec, and the product decisions from TypeScript.

## Documents

- [docs/reference.md](docs/reference.md) — type language, sources of types, type system rules, hints and Java, externs, escape hatches, configuration, CLI and JSON, optional extensions, error codes.
- [PLAN.md](PLAN.md) — architecture, implementation choices, what already exists, milestones, open questions.
