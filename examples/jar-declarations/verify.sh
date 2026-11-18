#!/usr/bin/env bash
set -euo pipefail
HERE="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)"
REPO="$(cd "$HERE/../.." && pwd)"
# All archives, CLI caches and temporary config stay OUTSIDE the repository.
WORK="$(mktemp -d "${TMPDIR:-/tmp}/typespec-jar-demo.XXXXXX")"
trap 'rm -rf "$WORK"' EXIT
mkdir -p "$WORK/bundled" "$WORK/split"
jar --create --file "$WORK/bundled/vendor-bundled.jar" \
  -C "$HERE/library/src" . -C "$HERE/library/resources" .
jar --create --file "$WORK/split/vendor-code.jar" -C "$HERE/library/src" .
jar --create --file "$WORK/split/vendor-types.jar" -C "$HERE/library/resources" .
for layout in bundled split; do
  cp -R "$HERE/consumer/src" "$HERE/consumer/negative" "$WORK/$layout/"
  cp "$HERE/consumer/typespec.edn" "$WORK/$layout/"
  # The checked-in consumer uses a relative local/root; relocated consumer needs
  # an absolute root computed here, never a machine-specific checked-in path.
  if [[ "$layout" == bundled ]]; then
    jars='"vendor-bundled.jar"'
  else
    jars='"vendor-code.jar" "vendor-types.jar"'
  fi
  printf '{:paths ["src" %s] :deps {local/typespec {:local/root "%s"}}}\n' \
    "$jars" "$REPO" > "$WORK/$layout/deps.edn"
  cd "$WORK/$layout"
  echo "=== $layout: actual runtime, outside repository ==="
  clojure -M -e '(require (quote consumer.good)) (assert (= "Hello, Ada!" (consumer.good/greeting))) (assert (= 42 (consumer.good/integer-result))) (println "runtime: greeting and generic integer result OK")'
  echo "=== $layout: static checker in a fresh JVM, tripwire armed ==="
  clojure -J-Dvendor-demo.check-only=true -M "$HERE/verify.clj" "$PWD" "$layout"
done
