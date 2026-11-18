#!/bin/sh
# Run from anywhere. Set JAVA_HOME to an installed JDK 25.
set -eu
cd "$(dirname "$0")"
: "${JAVA_HOME:?Set JAVA_HOME to an installed JDK 25}"
export JAVA_CMD="$JAVA_HOME/bin/java"
[ -x "$JAVA_CMD" ] || { echo "No Java executable: $JAVA_CMD" >&2; exit 2; }
exec clojure -M:check "${1:-baseline}"
