# Whole pinned context-clj/pg check

Static analysis of **the original complete `src/pg.clj`**, not copied functions.
No `require pg`, macro execution, datasource creation, SQL or services.

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
  asserts SHA-256, parses all source forms, calls `check-files` once on that entire
  parsed namespace. Loading the fixture alone does not run analysis.
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

This separate command exits **0** when all expectations hold, **1** on mismatch
or analysis failure. It does not treat the diagnostic CLI's expected exit 1 as
an acceptance failure: both commands call the same static fixture directly.
The actual Java version is printed in the summary. `run.sh` and its Java-25-only
check remain unchanged in policy; no Java 25 run is claimed for this acceptance.

Verified at checker revision `a2a86a22a479aed2d15d96e38b81ff375ec64b5c`
with Java **27.0.1.1**: contracts **2 errors / 8 warnings**. Acceptance asserts:

- Upstream pin `aa8ea0eb565e7f70f80f3248e6a83198628e1047` and complete source
  SHA-256 `dca4826d414a66c22dc10ebb2a01e1d8dccaf5cf64cdfe9b8bb952987d023db5`.
- 451 source lines, 54 top-level forms, namespace `pg`, one complete input file,
  last form spanning `437:1` through `451:28`.
- Exactly `TSP2322` at `57:15` and `TSP2345` at `206:41` (errors).
- Exactly seven `TSP210-01` warnings at `30:44`, `49:3`, `53:50`, `57:44`,
  `101:48`, `115:45`, `117:45`; one `TSP7023` warning at `308:35`.
  Codes, severities, filenames and positions are compared as a multiset.
- `pg` absent before and after checking; trust counters `cast/any/ignore = 0/0/0`.

No `pg` require, database connection, SQL or production service is used.
The complete-file hash and coverage checks do not imply support for every
Clojure construct (see limitations below).

Historical captured outputs in baseline.txt/contracts.txt: baseline **1 error / 10 warnings**, contracts **2 errors / 16 warnings**. These snapshots precede the precision pass and are not current expected-output tests.
Both: 451 source lines, 54 top-level parsed forms, `pg-loaded? false`, zero
casts/any/ignores. Whole-file input does not mean every Clojure feature is
supported: `comment`/`defmacro` bodies and discarded forms are not checked.
Dependencies' namespaces are on the classpath but not recursively analyzed.

Detailed findings, contract assumptions and limitations:
[`../../docs/context-pg-check.md`](../../docs/context-pg-check.md).
