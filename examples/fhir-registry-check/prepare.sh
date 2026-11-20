#!/bin/sh
set -eu
cd "$(dirname "$0")"
root=../../.hyper/tmp/ceya/upstream
pin=ea79b6a18d9ae759593c3824f502bc1200209e22
if [ ! -d "$root/.git" ]; then
  mkdir -p "$(dirname "$root")"
  git clone --no-checkout https://github.com/fhir-clj/fhir-package-registry "$root"
  git -C "$root" checkout --detach "$pin"
fi
[ "$(git -C "$root" rev-parse HEAD)" = "$pin" ]
# Recover missing submodule initialization after an interrupted first fetch.
# Refuse changed initialized submodules rather than silently moving their refs.
if git -C "$root" submodule status --recursive | grep -Eq '^[+U]'; then exit 1; fi
git -C "$root" -c url.https://github.com/.insteadOf=git@github.com: submodule update --init --recursive
[ -z "$(git -C "$root" status --porcelain --untracked-files=no)" ]
git -C "$root" submodule status --recursive | tee /dev/stderr | grep -Eq '^[+-U]' && exit 1
exit 0
