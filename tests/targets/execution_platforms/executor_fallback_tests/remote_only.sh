#!/bin/bash

OUT="$1"

if [[ ! -f "/run/re_worker/beacon" ]]; then
  echo "This only runs on RE" >&2
  exit 1
fi

exec touch "$OUT"
