#!/bin/sh
set -eu
cd "$(dirname "$0")"
if [ -n "${JAVA_HOME:-}" ]; then export JAVA_CMD="$JAVA_HOME/bin/java"; fi
# -Srepro excludes user-level deps.edn, preventing accidental injected dependencies.
case "${1:-}" in
  ""|--accept) ;;
  *) echo 'Usage: run.sh [--accept]' >&2; exit 2 ;;
esac
# Historical baseline files are immutable inputs, even for plain exploratory runs.
(cd baseline-initial && shasum -a 256 -c SHA256SUMS)
exec clojure -Srepro -M:check "$@"
