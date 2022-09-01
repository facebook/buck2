#!/bin/sh

OUT="$1"
shift
mkdir -p "$(dirname "$OUT")"
rm -f "$OUT"
for arg in "$@"; do echo "$arg" >> "$OUT"; done
