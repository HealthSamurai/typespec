# Pinned context-clj/pg source check

Static analysis of the original complete `src/pg.clj` and, in acceptance,
all nine production source files. The checker reads source as text.

From the repository root, using an installed JDK 25:

```sh
export JAVA_HOME=/path/to/jdk-25/home
sh examples/context-pg-check/run.sh baseline
sh examples/context-pg-check/run.sh contracts
```

Both commands intentionally exit **1** because upstream errors are found. Java
major version is asserted. `run.sh` sets `JAVA_CMD` explicitly: this Clojure CLI
can prefer PATH's java over JAVA_HOME. Clojure dependencies may be downloaded.
The harness is independent of any agent scratch directory. Direct equivalent
from this directory: `JAVA_CMD="$JAVA_HOME/bin/java" clojure -M:check baseline`.

- `deps.edn`: pinned pg/system, JDBC/Hikari transitively; checker from `../..`.
- `fixture.clj`: inert shared fixture; resolves `pg.clj` as a classpath resource,
  checks SHA-256 and submits whole files to `check-files`.
- `manifest.edn`: the exact set of nine source files under the pinned `src/`
  tree and their SHA-256 values.
- `check.clj`: original diagnostic CLI, still restricted to Java 25.
- `typespec.edn`, `externs.edn`: contracts mode only; one datasource contract
  and two explicit lifecycle macro body contracts, with source provenance.
- `baseline.txt`, `contracts.txt`: captured Java 25.0.5.1 results, summary first.

## Automated acceptance (Java >=25)

From the repository root:

```sh
export JAVA_HOME=/path/to/jdk-25-or-newer/home
sh examples/context-pg-check/acceptance.sh
# macOS with the installed JDK: JAVA_HOME="$(/usr/libexec/java_home)" sh examples/context-pg-check/acceptance.sh
```

This command exits **0** when both static checks match their expectations
and **1** on any mismatch or analysis failure. It prints the actual Java version
and runs the single-file regression before checking all production sources together.

Single-file regression on JDK 27, using the contracts mode:

- Upstream pin `aa8ea0eb565e7f70f80f3248e6a83198628e1047` and complete source
  SHA-256 `dca4826d414a66c22dc10ebb2a01e1d8dccaf5cf64cdfe9b8bb952987d023db5`.
- 451 source lines, 54 top-level forms, namespace `pg`, one complete input file,
  last form spanning `437:1` through `451:28`.
- Exactly `TSP2322` at `57:15` and `TSP2345` at `206:41` (errors).
- Exactly seven `TSP210-01` warnings at `30:44`, `49:3`, `53:50`, `57:44`,
  `101:48`, `115:45`, `117:45`; one `TSP7023` warning at `308:35`.
  Codes, severities, filenames and positions are compared as a multiset.
- `pg` absent before and after checking; trust counters `cast/any/ignore = 0/0/0`.

The production-tree check verifies all nine hashes and **351 top-level forms**.
It checks three errors at their exact locations. The two `pg.clj` errors
are also covered by the single-file regression. The third, `TSP2531` at
`pg/docker.clj:72:55`, covers a failed Docker result whose absent `:output`
can reach `str/split` as `nil`. It pins the warning counts by diagnostic
code (**93 total**) and verifies warnings inside the conditional step in
`mk-table-constraint` and the reducers in `mk-columns` and `mk-options`.
The reducers now report five unknown arguments to `name` at their source
locations. The inferred accumulators resolve two downstream collection
warnings at `str/join` and `ql/fast-join`. The upstream namespaces
remain unloaded and the trust counters stay at zero in both checks.

Acceptance also checks the entire tree with a temporary extra `(Object.)`
binding in `free-port`'s `with-open`. The additional `TSP2339` points at
the initializer in `pg/docker.clj:8:64`.

Historical captured outputs in baseline.txt/contracts.txt: baseline **1 error / 10 warnings**, contracts **2 errors / 16 warnings**. These snapshots precede the precision pass and are not current expected-output tests.
Both historical runs covered 451 source lines and 54 top-level forms. The
checker skips `comment`/`defmacro` bodies and discarded forms. Dependencies
outside the pinned `src/` tree are available on the classpath.

Detailed findings, contract assumptions and limitations:
[`../../docs/context-pg-check.md`](../../docs/context-pg-check.md).
