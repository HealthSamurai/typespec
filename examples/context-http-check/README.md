# context-clj/http: untouched full-source static baseline

## Provenance and reproduction

Target: https://github.com/context-clj/http (not a consumer application).
Git pin: `e158b6b244c2b8e053de2a097d1071d9419adef9`.
GitHub API inspection: default branch `main`, 0 stars, not archived,
HEAD equal to pin, last push `2026-03-19T15:34:23Z`, commit authored
`2026-03-19T15:33:52Z` ("Exclude jetty cuz vulnerabilities").

API commands used:

```sh
curl -fsSL https://api.github.com/repos/context-clj/http
curl -fsSL https://api.github.com/repos/context-clj/http/commits/HEAD
curl -fsSL 'https://api.github.com/repos/context-clj/http/git/trees/e158b6b244c2b8e053de2a097d1071d9419adef9?recursive=1'
```

`manifest.edn` lists all 8 production .clj files under upstream `src/`.
At creation their raw bytes were verified against each GitHub tree Git blob
SHA-1 (`SHA1("blob " + byte-count + NUL + bytes)`), then SHA-256 recorded.
The harness checks the pin in the resource path, exact recursive source-file
set, and all eight SHA-256 values before parsing. No vendored modifications.
Tests, resources and dependency source are outside this production scope.
All top-level forms are submitted; none are manually curated or suppressed.

From typespec root:

```sh
JAVA_HOME=/opt/homebrew/opt/openjdk/libexec/openjdk.jdk/Contents/Home \
  examples/context-http-check/run.sh
# Plain static run; writes current/{summary,diagnostics}.edn, never the baseline.
# Exit 1 when current diagnostics include errors (initial capture: 7).
```

Portable equivalent: set JAVA_HOME to your JDK >=25, then invoke `run.sh`.
The script executes `clojure -Srepro -M:check` from its own directory.
Tested: Java **27.0.1.1**, Clojure CLI **1.12.6.1673**, Clojure **1.12.4**,
rewrite-clj **1.2.50**. Checker repository HEAD at capture:
`c7cb5572e943bd07526cb790e2be92a48ecaffa1` (local/root, not remotely pinned).
Upstream requests Clojure 1.12.0; harness deliberately selects 1.12.4 to
match the checker catalogue. This is not replication of upstream runtime.
No checker edits, extern contracts, macro models, t/cast, t/any or t/ignore.

Only dependency fetching needs network. The check never requires or evaluates
upstream namespaces, opens a server, connects to a DB, or invokes upstream
code. Assertions verify all eight upstream namespaces remain unloaded.
It uses rewrite-clj parsing and checker APIs, not Clojure load-file/read-eval.
Normal JVM/checker code runs; this is not an OS-level sandbox.

## Captured baseline

Immutable initial capture: `baseline-initial/`, protected by `SHA256SUMS`
(verified on every run). The top-level legacy artifacts below are retained
unchanged too. No run overwrites either copy.

Artifacts: `summary.edn` (hashes, coverage, forms, opaque-node positions,
trust and diagnostic counts), `diagnostics.edn` (all 51 diagnostic objects),
`baseline.txt` (human-readable console output). Parse exceptions would be
recorded per file and force exit 1, not silently discarded.

| File under src/ | Top-level forms | Errors | Warnings | Opaque AST nodes |
|---|---:|---:|---:|---:|
| http.clj | 75 | 2 | 15 | 5 |
| http/formats.clj | 40 | 0 | 7 | 0 |
| http/mcp.clj | 22 | 2 | 1 | 0 |
| http/openapi.clj | 4 | 0 | 1 | 1 |
| http/routing.clj | 16 | 0 | 10 | 0 |
| uui.clj | 26 | 2 | 9 | 4 |
| uui/heroicons.clj | 331 | 1 | 1 | 0 |
| uui/menu.clj | 7 | 0 | 0 | 1 |

8/8 files parsed; **0 parse exceptions**; 521 top-level forms.
`split-lines` counts 1757 lines (drops trailing blank lines, unlike wc -l).
**7 errors, 44 warnings**. Trust: `{:cast 0 :any 0 :ignore 0}`.

Diagnostic groups:
- TSP2345: 7 errors, all around clojure.string/replace overload matching.
  http.clj 103:47, 141:52; http/mcp.clj 74:76, 74:80;
  uui.clj 137:36, 148:36; uui/heroicons.clj 25:65.
  These are findings for parent triage, not demonstrated upstream bugs.
- TSP210-01: 35 warnings, unknown compatibility.
- TSP7031: 6 warnings, http/register-endpoint macro bodies not checked
  (http.clj 172:6; http/openapi.clj 10:4; uui.clj 45–48:4).
- TSP9002: 2 warnings, predicate specs opaque to checker (http.clj 111,113).
- TSP7023: 1 warning, reflective call on known InputStream
  (http/formats.clj 309:13).

## Regression acceptance

```sh
JAVA_HOME=/opt/homebrew/opt/openjdk/libexec/openjdk.jdk/Contents/Home \
  examples/context-http-check/run.sh --accept
```

Acceptance runs the same complete source check, then compares against the
explicitly reviewed `expected-current.edn`. It never creates or updates that
file. Missing expectations, changed diagnostics, hashes, forms, positions,
opaque nodes, parse failures, loaded upstream namespaces or trust escapes
fail the command. All 8 SHA-256 values, 8 parsed / 0 failed, 1757 lines and
per-file form counts (521 total) are checked. Diagnostic comparison is an
order-independent multiset of exact `[file pos severity code message reasons id]`
tuples: duplicates remain significant, and full source ranges are included.
No assertion of semantic completeness is implied by acceptance.

Measured candidate: **0 errors, 44 warnings**, with unchanged file coverage.
All seven initial TSP2345 errors disappear. Six TSP210-01 warnings in
http/routing.clj (rows 46, 60, 61, 67, 74, 77) now name `int?` instead of
`number?`: the checker chooses the first incomplete-compatible overload,
not the previous last fallback. This is only diagnostic target selection,
**not improved inferred types**; all six remain warnings. The other 38
warning objects are identical to the initial capture. The reviewed expected
snapshot records this measured delta, including new diagnostic IDs.

F220 acceptance records **0 errors and 48 warnings** with the same source
coverage. The three new `:accumulator` warnings are at `http.clj:286` and
`:306`, where the registered hook's result comes from runtime state, and
`http/routing.clj:53`, where the accumulator crosses the untyped recursive
`-match` return. The reducer in `match-fn-params` now infers a possible result
from `conj`, so the matching branch is checked and reports the unknown `wgt`
passed to `+` at `http/routing.clj:56`. These are unchecked contracts. All
44 diagnostic tuples from the replace audit are present in the F220 snapshot.

F233 keeps all 48 F220 diagnostics and adds one unchecked composition
warning at `http/formats.clj:239:70`. In `(comp #{:json :charset} first)`,
`first` has no function signature as a value. The composition
`(comp byte int)` at line 305 has a checked signature and accepts the
`Character` values produced by walking the string. Acceptance also asserts
the inferred signature at that source position. The F233 snapshot
records **0 errors and 49 warnings**.

F721 recognizes ten `#'var` forms in the parsed source. The reviewed
coverage snapshot records their AST positions as recognized Var references
and retains all 49 diagnostic tuples.

F112 extracts the original endpoint predicates at `http.clj:111` and
`:113`. The F112 snapshot had **0 errors and 48 warnings**. The delta
removes two `TSP9002` warnings and adds one `TSP210-01` for the unknown
request method at `http.clj:311:60`. Acceptance also checks
[`probe/endpoint.clj`](probe/endpoint.clj) alongside all eight production
sources. It expects both valid calls to pass and checks two type errors
for deliberately invalid endpoint maps.

F610 checks the argument of `http/register-endpoint` against the original
`::http/endpoint`. The current snapshot has **0 errors and 43 warnings**.
Five calls pass and the dynamic endpoint at `http.clj:172:37` yields one
`TSP210-01`. The endpoint probe compares direct checks and macro calls
against the same Spec.

Acceptance also checks [`probe/response.clj`](probe/response.clj).
The response contract accepts a string `"content-type"` and rejects an
integer stored by `assoc-in` at `[:headers "content-type"]`.

To review a future checker change, first run plain `run.sh`, inspect
`current/` against the immutable baseline and existing expected snapshot,
then deliberately edit the expected file in a separate review step. There
is intentionally no update-snapshot mode. Acceptance is allowed to fail
until that review has happened.

## Initial parser/support limits (no fixes applied at baseline)

Full-file parsing is not full semantic coverage. Eleven opaque AST nodes:
one syntax quote (register-endpoint macro implementation, http.clj149–153),
ten var quotes (#'dispatch, endpoint/hook function references; exact locations
in summary.edn). These forms are accepted structurally but not represented
with precise semantics. Six macro-use warnings explicitly mark unchecked
bodies. Unknown data flow and opaque specs further limit confidence.
No errors in uui/menu.clj does not establish correctness (it has an opaque
var quote). Comment forms are retained in the parsed AST but may not be
semantically checked; no claim of executing or checking every runtime path.
