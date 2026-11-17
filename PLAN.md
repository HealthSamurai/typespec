# Typespec — implementation plan

Design decisions: [README.md](README.md) · [README.ru.md](README.ru.md). Details: [docs/reference.md](docs/reference.md). Estimates are rough.

## Architecture in one picture

```
 source .clj ──► clj-kondo parser + analysis ──► ns graph, cache, :lint-as / hooks
                        │
                        ▼
   declarations, read from source:              Java, read from bytecode:
   s/def · t/sig · s/fdef · externs EDN         classes · methods · overloads ·
   type hints ^String ^long ^[String]           generics · @Nullable / JSpecify
                        │                              │
                        └──────────────┬───────────────┘
                                       ▼
            Typed Clojure core (fork ts-features): narrowing, generics, unions,
            subtyping of Clojure maps and Java classes, overload resolution
                                       │
                                       ▼
            TSP diagnostics (text / JSON) ──► CLI, CI, clojure-lsp, agents
            reflection / boxing warnings, hint fixes
```

## Prototype (done)

A first end-to-end slice exists in `src/typespec` (about 2,100 lines of Clojure) with a demo project and tests. Run `clojure -M:test`.

| Part | File | What it does |
|---|---|---|
| Reader | `parse.clj` | rewrite-clj → a small AST with positions; `#_(t/...)` directives are kept; reader conditionals take the `:clj` branch |
| Types | `types.clj` | representation, the type language (read and print), subtyping, Java class subtyping |
| Declarations | `env.clj` | `s/def`, `s/fdef`, `{:t/sig}`, type hints, `:pre`, destructuring patterns → one environment, from text only |
| Checker | `check.clj` | inference, narrowing, map and collection operations, Clojure 1.13 destructuring, Java calls, diagnostics |
| Java | `java.clj` | members by reflection on classes that are loaded but not initialized |
| Core signatures | `core_types.clj` | about 70 signatures of `clojure.core` and `clojure.string`, in the same notation as externs |
| CLI | `main.clj` | `check` (text and JSON), `sig`, `type-at` |

**Where the prototype differs from the plan below, and why:**

- **Own checker core, not the Typed Clojure fork.** The fork resolves names through the live runtime; a from-scratch core over text was faster to get end to end. The fork stays as the reference for generics and for the algorithms to port.
- **rewrite-clj, not the clj-kondo parser.** In a trial, clj-kondo's own parse entry point dropped `#_` forms, and the escape hatches need them. clj-kondo's `:lint-as`, hooks and exports are still the plan for macros.
- **Java by reflection, not ASM.** Simpler to start with; classes are loaded without initialization. ASM is still needed for a GraalVM binary.
- **Whole-program in one process.** No per-namespace interfaces, cache or daemon yet.

M0's question is therefore answered differently than planned: the front end is rewrite-clj plus our own environment builder, and the checker core is ours.

**First run on real code** (context-clj: `system`, `pg`, `http`; 5,800 lines, almost no specs): 1–4 s per module including JVM start, 1 true error (`BufferedReader` passed as `^InputStream`), 39 warnings, 0 false errors after about ten checker fixes. Lessons that went into the code: element access (`first`, `nth`, vector destructuring) is unchecked by default, as in TypeScript without `noUncheckedIndexedAccess`; a map whose type was inferred from a literal or a pattern is open to unknown keys; a spec the checker cannot read is a warning (`TSP9002`), not an error; a var defined in the namespace shadows a built-in rule of the same name; with an unknown argument and several overloads the result is unknown.

**Prior art to watch:** [crabjure](https://github.com/ribelo/crabjure) (Rust, proof of concept, one author, no license file). It confirms the read-source architecture and its speed (17–21× clj-kondo by its own benchmarks), resolves macros by "archetypes" (`let-like`, `defn-params`, `for-like`) instead of hooks, and reads `.clj-kondo/config.edn`. Its type checker is a gated experiment with types in comments. Worth borrowing: archetypes as a simpler first step than SCI hooks.

**Next steps for the prototype:** generics (`map`, `filter` and friends are special-cased today), `reify`/protocols/multimethods, `:lint-as` and hooks for project macros, externs for libraries, `fix` and `annotate`, a larger core signature table, then a first real namespace from context-clj.

## Implementation choices

Design decisions live in the README. These are choices about *how* to build it; any of them may change without changing the design.

| Topic | Choice | Fallback |
|---|---|---|
| Parser and ns graph | rewrite-clj (positions, comments, `#_` forms) plus our own ns graph; clj-kondo for `:lint-as`, hooks and exports | clj-kondo's analysis, if we need more of it |
| Checker core | our own, text-based; algorithms for generics ported from Typed Clojure | the Typed Clojure fork (`ts-features`), which stays as a reference |
| Macros | built-in rules → `:lint-as` → SCI hooks → `:t/macro` | opt-in expansion in a worker JVM: no network, project read-only, only macro namespaces loaded |
| Java | read bytecode with ASM, no class loading | reflection in a worker JVM |
| Ns interfaces | computed from one file's text (signatures only), cached in `.typespec/cache/<ns>.edn`, keyed by the hash of the source and of dependency interfaces | — |
| Parallelism | one task per ns once interfaces are known | — |
| Daemon | warm JVM holding the ns graph, used by `watch` and the LSP; target <300 ms per saved file, tracked as a CI benchmark | — |
| Editor | plug into clojure-lsp | own LSP server |
| Shipping interfaces | `typespec emit` writes `typespec.exports/<lib>.edn` into the jar | shared `typespec-externs` repo |
| Destructuring rules | precise types for what the form expands into (`req!`, `get`, `select-keys`, `merge`, `dissoc`, `some-vals`) plus a syntax-level rule for messages; a differential test against the real `destructure` keeps them in sync | — |
| Externs drafts | `typespec extern <lib>`: an agent drafts from arglists, docstrings, source and tests; `--verify` runs the library tests with `instrument` and `:ret` checks (a sample, not a proof) | — |

## What already exists

| Layer | Foundation | Status |
|---|---|---|
| Core: narrowing, generics, unions, body inference | Typed Clojure (a port of Typed Racket) | available |
| TS-style errors, Did you mean, Why | fork [niquola/typedclojure@ts-features](https://github.com/niquola/typedclojure/tree/ts-features) | available |
| Pick/Omit/Partial/Required, exhaustive `case` | fork | available |
| spec forms → types | fork `74c55cc`, reads the spec registry | partial: must read source; no `or`/`and` groups in `s/keys`, no recursion, `s/and` takes the first part |
| `req!`, nested destructuring | fork `c0f507b` | partial: literal-key lookup only |
| Java classes and methods | Typed Clojure, via reflection | partial: must read bytecode; no hint checks |
| reader-first declarations, `t/sig`, `types.edn`, EDN externs | — | new |
| ns interfaces, cache, parallelism, daemon | — | new; the bulk of the work |
| JSON, fix, coverage, baseline, trust budget | — | new |

## M0 — spike: front end on clj-kondo (2 weeks)

Answer one question: can we build the front end on clj-kondo's parser and analysis?

- [ ] Use clj-kondo as a library: parse a namespace, get positions, the ns graph and the analysis export.
- [ ] Feed parsed forms into the Typed Clojure checker without loading the ns.
- [ ] Handle `:lint-as` and one hook (SCI) for a project macro.
- [ ] `#_(t/ignore ...)`, `#_(t/cast ...)` directives seen by the parser.
- [ ] Measure: checking `app2` and context-clj, cold and warm.
- **Exit:** a go/no-go on clj-kondo as the front end. Fallback: tools.reader + our own ns graph.

## M1 — declarations and the type language (4–6 weeks)

- [ ] Type language from section 1: spec subset + `t/` forms; `TSP9001` for anything else.
- [ ] Read `s/def`, `s/fdef`, `{:t/sig '[...]}` and `typespec/types.edn` from source (no spec registry).
- [ ] Type hints as signatures (`^String`, `^long`, `^[String]` param-tags); class names in the type language.
- [ ] Java from bytecode: classes, subtyping, methods, overloads, generics; hint checks `TSP7020`–`TSP7026`.
- [ ] `fix` for reflection: insert proven hints (`safe` when the overload is unique).
- [ ] Recursive specs, `(or ...)`/`(and ...)` groups in `s/keys`, `s/?`/`s/*` as arities.
- [ ] Check destructuring against declared types (`TSP2339`, Did you mean), `TSP2304` for keys without a spec.
- [ ] JSON diagnostics v1: stable ids, `cause_of`, `fixes` with `risk`.
- [ ] Per-code levels `:off`/`:warning`/`:error` in `typespec.edn` (as in clj-kondo).
- **Exit:** `app2` and context-clj type-check without `t/ann`; all 7 seeded bugs caught.

## M2 — modules, speed, libraries (4–6 weeks)

- [ ] Ns interfaces from `t/sig` (like TS `isolatedDeclarations`); check against interfaces, not bodies.
- [ ] Interface cache keyed by source hash; parallel checking; `watch` daemon.
- [ ] CI benchmark: <300 ms per saved file of average size.
- [ ] Externs in EDN with versions; lookup order: own → `typespec.exports/` in jars → `s/fdef` in library sources → shared repo → `t/unknown`.
- [ ] Opt-in macro expansion in a sandboxed worker (`:macros {:expand true}`).
- [ ] Import from `clj-kondo.exports/`: `:lint-as`, hooks, `:type-mismatch` types and arities.
- [ ] Externs for next.jdbc, ring, http-kit, aidbox-sdk.
- **Exit:** a real Aidbox module checks incrementally in the editor loop.

## M3 — Clojure semantics (6–8 weeks)

- [ ] Clojure 1.13 destructuring table (`:keys!`, `:req!`, `:select`, `:excess`, `:missing`, `:all`, `selector`) with a differential test against the real `destructure`.
- [ ] Map operations: `assoc`, `dissoc`, `merge`, `update`, `select-keys`, `get-in`/`assoc-in`/`update-in` with literal paths.
- [ ] Records, protocols (incl. `extend-*` in other namespaces), multimethods with keyword dispatch, atoms, core.async.
- [ ] Java: platform types, JSpecify/`@Nullable`/`@NonNull`/`@NullMarked`, `:strict-nil`; `reify`/`proxy`/`deftype` against interfaces; functional interfaces; arrays; boxed-math warnings.
- [ ] FHIR types generated from FHIR Schema (type provider, an optional extension).
- **Exit:** a typical Aidbox namespace checks in strict mode with a trust budget under an agreed threshold.

## M4 — editors and agents (ongoing)

- [ ] Plug into clojure-lsp: diagnostics, `type-at` hover, go-to-definition for keys and specs.
- [ ] Commands `sig`, `type-at`, `order`, `annotate` (inline or `:sidecar`), `fix --risk safe`, `coverage --trust`.
- [ ] Baseline file; trust budget in CI (no growth of `any`/`cast`/`ignore`/weakening).
- [ ] Agent loop on a real Aidbox module: `order` → `annotate` → `check --json` → fix → repeat.

## Optional (after M3)

- Brands / units (`t/brand`), row polymorphism, counterexamples in exhaustiveness errors (reference, optional extensions).
- `typespec emit`: ship interfaces inside library jars (`typespec.exports/`), like `.d.ts`.
- CLJS / CLJC.

## Open questions

1. clj-kondo as a library or as a fork? (decided in M0)
2. Typed Clojure core: keep the fork or extract only the checker core?
3. Where should the shared externs repo live, and who maintains it?
4. Pin Clojure 1.13 semantics to which release, while it is still alpha?
5. CLJS / CLJC: reader conditionals and a second platform — in scope for 1.0 or later?
6. Should `types.edn` be one file or one per namespace?
