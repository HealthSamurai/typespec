# FHIR package registry: pinned static baseline

Target: https://github.com/fhir-clj/fhir-package-registry
Pin: `ea79b6a18d9ae759593c3824f502bc1200209e22`.
GitHub metadata: 0 stars, main, not archived, pushed 2026-05-21T11:39:11Z.
Full metadata and commit are captured in JSON. No upstream edits/builds.

## Reproduce

From typespec root:

```sh
JAVA_HOME=/opt/homebrew/opt/openjdk/libexec/openjdk.jdk/Contents/Home \
  examples/fhir-registry-check/run.sh
```

Requires git, Clojure CLI and JDK >=25 (captured Java 27.0.1.1).
`prepare.sh` clones only when absent, checks the exact detached root SHA,
checks tracked worktree cleanliness and submodule status. SSH submodule URLs
are rewritten to public HTTPS for fetching, without editing upstream files.
Checkout lives at `.hyper/tmp/ceya/upstream`, outside the owned example.
`run.sh` verifies immutable `baseline-initial/SHA256SUMS` before running
`clojure -Srepro -M:check`. Current output is always separate in `current/`.
Exit **1 is expected** for this initial diagnostic baseline (not a passing check).
The initial capture is never rewritten by either script; `check.clj` and deps
used for the capture are also archived there. No accept/update-baseline mode.

Inventory was obtained with:

```sh
gh api 'repos/fhir-clj/fhir-package-registry/git/trees/ea79b6a18d9ae759593c3824f502bc1200209e22?recursive=1'
gh api repos/fhir-clj/fhir-package-registry
gh api repos/fhir-clj/fhir-package-registry/commits/ea79b6a18d9ae759593c3824f502bc1200209e22
```

All **11 production files under src/** were verified initially against GitHub
blob SHA1, and their SHA256 is in `manifest.edn`. Every run checks the exact
recursive .clj/.cljc/.cljs inventory and byte SHA256. Tests, dev, resources
and dependency sources are outside this application-source scope. In particular,
tracked first-party `libs/gcs/src/gcs.clj` is a dependency, not a gitlink;
its SHA256 and deps metadata SHA256 are in `dependency-provenance.json`.
This is not a claim to have typechecked every production file in the repository.

## Dependency provenance and resolution

All local-root submodules are resolved at upstream gitlinks, not newer context
baselines:

| Dependency | Exact gitlink |
|---|---|
| context/http | af7e22dec39b2f261de9a7c2959a7eed1d06ce82 |
| context/pg | 6be838f56a258ef70c86a7c2cc78123d6171ed94 |
| context/system | b94d9ae289abd7551a8d360db10946249df4df75 |
| fhir-schema | 929b01a05f06f05f287610766ed3a72b82507405 |

The first resolution failed: tools.deps could not compare transitive
context/system git coordinates with upstream's local-root coordinate.
`resolution-first-attempt.txt` preserves the exact error. Harness deps now
explicitly select **the upstream root's pinned system gitlink** at top level,
over transitive refs d178613a466fdd74aa73a178ffa2851919d78122 (http) and
e61f3e7febba3408f076ccae043384a3ac933cb0 (pg). No dependency sources changed.
This is an explicit resolution decision, not the unmodified upstream invocation.

Subsequent `clojure -Srepro -Spath` succeeded: complete classpath captured in
`resolution-classpath.txt`, Maven download log in `resolution-stderr.txt`.
**No unresolved dependency blockers and no source-only fallback.** Classpath
file contains machine-local paths for audit, not portable paths for reuse.
Clojure is deliberately **1.12.4** (checker metadata), upstream asks **1.12.0**.
Checker is local/root ../.. at capture HEAD
`9a997297564fb4e2a820185699e8d95f037fe501`, rewrite-clj 1.2.50.
Dependency sources are available for classpath/reflection, not submitted as
additional check targets. No extra contracts, casts, any, ignore, macro models
or checker changes were added.

## Initial results

- 11/11 files parsed, **2405 lines**, **204 top-level forms**.
- Parse failures **0**, opaque AST nodes **0**.
- **2 errors, 121 warnings**, trust `{:cast 0 :any 0 :ignore 0}`.
- Warning codes: TSP210-01 **72**, TSP7020 **43**, TSP7023 **6**.
- TSP2305: `src/fhir/registry/diff.clj:148:14`, `fhir.registry has no var context`.
- TSP2339: `src/fhir/registry/gcs.clj:49:76`,
  `com.google.cloud.ServiceOptions$Builder has no method build`.

These are checker observations, not confirmed runtime defects. Zero opaque
nodes measures parser coverage, not successful typing or macro semantic coverage.
`summary.edn` records per-file forms, final positions, opaque positions,
parse errors, diagnostic counts and hashes. `diagnostics.edn` contains all
123 diagnostics with positions, codes, messages and IDs; `baseline.txt` is
the full human-readable capture. Nothing is filtered or suppressed.

Only checker/JVM code executes: upstream files are read with rewrite-clj,
never required, evaluated or loaded. All 11 target namespaces are asserted
unloaded. No server, database, GCS client operation, credentials, application
entrypoint or upstream build script is invoked. Dependency resolution uses
network; this is a static harness, not an OS sandbox. Java class metadata may
be inspected by the existing checker.
