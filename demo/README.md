# Demo: a small clinic app with 23 planted mistakes

The demo is ordinary Clojure. Its types come from what Clojure code already has:
`s/def` for data, `s/fdef` for function signatures, type hints, `:pre` conditions and Java classes.
Nothing in it was written specially for Typespec, except three `#_(t/...)` comments in `legacy.clj`.

```
cd ..                                   # the repository root
clojure -M:check check --root demo      # 27 errors, 3 warnings
clojure -M:check check --root demo --json
clojure -M:check sig clinic.billing --root demo
clojure -M:check type-at src/clinic/patients.clj:57:9 --root demo
clojure -M:test                         # compares the output with expected.txt
```

The expected output is in [expected.txt](expected.txt).

| File | What it shows |
|---|---|
| `specs.clj` | the domain model in plain `clojure.spec` |
| `patients.clj` | signatures from `s/fdef`: key typos, a non-exhaustive `case`, nil, wrong argument order |
| `billing.clj` | map literals, collections, `get-in` paths, types read from `:pre` |
| `export.clj` | type hints as signatures, Java interop, reflection warnings with a ready fix |
| `intake.clj` | Clojure 1.13 destructuring: `:keys!`, required keyword arguments, `:missing` |
| `legacy.clj` | escape hatches: `#_(t/cast ...)`, `#_(t/ignore "reason")`, and an unused ignore |
| `unresolved.clj` | names that do not exist |

## What the same code does without Typespec

Every namespace except `unresolved.clj` loads without errors on Clojure 1.13.0-alpha11. Each mistake was then run:

| # | Mistake | What happens at run time | Typespec |
|---|---|---|---|
| 1 | typo in a destructured key | returns `"p1 "`, silently | `TSP2339`, with a suggestion |
| 2 | `case` without `"deceased"` | `IllegalArgumentException` for such a patient | `TSP8001` |
| 3 | `upper-case` on an optional name | `NullPointerException` | `TSP2531` |
| 4 | patient not found | passes a nil on, returns `"Hello"` | `TSP2531` |
| 5 | status `"discharged"` | the map now holds an invalid status | `TSP2322` |
| 6 | arguments in the wrong order | returns `"Hello, stranger"`, silently | `TSP2345` ×2 |
| 7 | typo in a literal key, a required key missing | returns an invalid invoice | `TSP2304`, `TSP2741` |
| 8 | summing titles | `ClassCastException` | `TSP2345` |
| 9 | typo in a `get-in` path | returns `nil` | `TSP2339`, with a suggestion |
| 10 | a string for the rate | `AssertionError` from `:pre` | `TSP2345` |
| 11 | returns nil instead of a boolean | returns `nil` | `TSP2322` |
| 12 | swapped arguments of a hinted function | `ClassCastException` | `TSP2345` ×2 |
| 13 | a string for `plusDays` | `ClassCastException` | `TSP2345` |
| 14 | `Map.get` of a missing key | `NullPointerException` | `TSP2531` |
| 15 | a `^String` hint on a number | `ClassCastException` | `TSP2322` |
| 16 | reflective call, the type is known | works, slowly; `(file-size "abc")` returns 3 | `TSP7023` with a fix |
| 17 | reflective call, the type is unknown | works, slowly | `TSP7020` |
| 18 | `^long` on the function name | loads; a caller that does arithmetic on the result does not compile | `TSP7021` |
| 19 | `.getYears` does not exist | `IllegalArgumentException: No matching field found` | `TSP2339`, with a suggestion |
| 20 | `:keys!` under an optional key | `IllegalArgumentException: Missing required key` | `TSP2741` |
| 21 | a required keyword argument is missing | `IllegalArgumentException: Missing required key` | `TSP2741` |
| 22 | `:missing` is bound but not checked | returns `"Welcome,  <a@b.c>"` | `TSP2531` ×2 |
| 23 | an ignore that ignores nothing | — | `TSP2578` |
| — | `str/uppercase`, `patients/find-patients` | the namespace does not compile | `TSP2305`, with a suggestion |

Eight of the 23 fail silently: the code returns a wrong value and nothing is thrown.

The functions marked `;; OK` in the sources do the same things correctly and produce no diagnostics.
