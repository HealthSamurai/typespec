# Typespec — implementation plan

Design: [README.md](README.md) · [README.ru.md](README.ru.md). Estimates are rough.

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

## M0 — spike: front end on clj-kondo (2 weeks)

Answer one question: can we build the front end on clj-kondo's parser and analysis?

- [ ] Use clj-kondo as a library: parse a namespace, get positions, the ns graph and the analysis export.
- [ ] Feed parsed forms into the Typed Clojure checker without loading the ns.
- [ ] Handle `:lint-as` and one hook (SCI) for a project macro.
- [ ] `#_:typespec/ignore` seen by the parser.
- [ ] Measure: checking `app2` and context-clj, cold and warm.
- **Exit:** a go/no-go on clj-kondo as the front end. Fallback: tools.reader + our own ns graph.

## M1 — declarations and the type language (4–6 weeks)

- [ ] Type language from section 1: spec subset + `t/` forms; `TSP9001` for anything else.
- [ ] Read `s/def`, `s/fdef`, `{:t/sig '[...]}` from source (no spec registry).
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
- [ ] Import from `clj-kondo.exports/`: `:lint-as`, hooks, `:type-mismatch` types and arities.
- [ ] Externs for next.jdbc, ring, http-kit, aidbox-sdk.
- **Exit:** a real Aidbox module checks incrementally in the editor loop.

## M3 — Clojure semantics (6–8 weeks)

- [ ] Clojure 1.13 destructuring table (`:keys!`, `:req!`, `:select`, `:excess`, `:missing`, `:all`, `selector`) with a differential test against the real `destructure`.
- [ ] Map operations: `assoc`, `dissoc`, `merge`, `update`, `select-keys`, `get-in`/`assoc-in`/`update-in` with literal paths.
- [ ] Records, protocols (incl. `extend-*` in other namespaces), multimethods with keyword dispatch, atoms, core.async.
- [ ] Java: platform types, JSpecify/`@Nullable`/`@NonNull`/`@NullMarked`, `:strict-nil`; `reify`/`proxy`/`deftype` against interfaces; functional interfaces; arrays; boxed-math warnings.
- [ ] FHIR types generated from FHIR Schema (type provider, optional section 12).
- **Exit:** a typical Aidbox namespace checks in strict mode with a trust budget under an agreed threshold.

## M4 — editors and agents (ongoing)

- [ ] Plug into clojure-lsp: diagnostics, `type-at` hover, go-to-definition for keys and specs.
- [ ] Commands `sig`, `type-at`, `order`, `annotate --types-only`, `fix --risk safe`, `coverage --trust`.
- [ ] Baseline file; trust budget in CI (no growth of `any`/`cast`/`ignore`/weakening).
- [ ] Agent loop on a real Aidbox module: `order` → `annotate` → `check --json` → fix → repeat.

## Optional (after M3)

- Brands / units (`t/brand`), row polymorphism, counterexamples in exhaustiveness errors (README section 12).
- `typespec emit`: ship interfaces inside library jars (`typespec.exports/`), like `.d.ts`.
- CLJS / CLJC.

## Open questions

1. clj-kondo as a library or as a fork? (decided in M0)
2. Typed Clojure core: keep the fork or extract only the checker core?
3. Where should the shared externs repo live, and who maintains it?
4. Pin Clojure 1.13 semantics to which release, while it is still alpha?
