#!/bin/sh
# Separate Java >=25 acceptance runner; run.sh retains its Java 25 pin.
set -eu
cd "$(dirname "$0")"
: "${JAVA_HOME:?Set JAVA_HOME to an installed JDK >=25}"
export JAVA_CMD="$JAVA_HOME/bin/java"
[ -x "$JAVA_CMD" ] || { echo "No Java executable: $JAVA_CMD" >&2; exit 2; }
exec clojure -M acceptance.clj
