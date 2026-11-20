#!/bin/sh
set -eu
cd "$(dirname "$0")"
[ "$#" -le 1 ] || { echo 'Usage: run.sh [baseline|contracts]' >&2; exit 2; }
case "${1:-baseline}" in baseline|contracts) ;; *) echo 'Use baseline or contracts' >&2; exit 2;; esac
sh prepare.sh
if [ -n "${JAVA_HOME:-}" ]; then export JAVA_CMD="$JAVA_HOME/bin/java"; fi
(cd baseline-initial && shasum -a 256 -c SHA256SUMS)
mkdir -p current
export FHIR_CLASSPATH_MODE=full
exec clojure -Srepro -M:check "${1:-baseline}"
