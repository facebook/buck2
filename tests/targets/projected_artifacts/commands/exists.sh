#!/bin/bash
set -o errexit
set -o nounset
set -o xtrace

OUT="$1"
shift

for f in "$@"; do
  echo "check $f" >&2
  test -f "$f"
done


touch "$OUT"
