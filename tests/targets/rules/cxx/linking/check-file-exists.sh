#!/bin/sh

set -x
set -o

for f in $1; do
    test -s "$f";
done
