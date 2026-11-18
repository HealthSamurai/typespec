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
- `check.clj`: resolves `pg.clj` as a classpath resource, asserts SHA-256, parses
  all source forms, calls `check-files` once on that entire parsed namespace.
- `typespec.edn`, `externs.edn`: contracts mode only; one datasource contract
  and two explicit lifecycle macro body contracts, with source provenance.
- `baseline.txt`, `contracts.txt`: captured Java 25.0.5.1 results, summary first.

Baseline: **1 error / 10 warnings**. Contracts: **2 errors / 16 warnings**.
Both: 451 source lines, 54 top-level parsed forms, `pg-loaded? false`, zero
casts/any/ignores. Whole-file input does not mean every Clojure feature is
supported: `comment`/`defmacro` bodies and discarded forms are not checked.
Dependencies' namespaces are on the classpath but not recursively analyzed.

Detailed findings, contract assumptions and limitations:
[`../../docs/context-pg-check.md`](../../docs/context-pg-check.md).
