#!/bin/bash
OUT="$1"
mkdir "$OUT"

touch "$OUT/a"

mkdir "$OUT/b"
echo "This is b" > "$OUT/b/b"

mkdir "$OUT/c"
echo "This is c" > "$OUT/c/c"
ln -s "../b/b" "$OUT/c/b"
