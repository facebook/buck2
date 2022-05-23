#!/bin/bash

BASE="$(basename "$(pwd)")"
ARTIFACT="$1"

if [[ "$BASE" = "fbcode" ]]; then
  echo "Test is running from fbcode" >&2
  exit 1
fi

if [[ "$ARTIFACT" != /* ]]; then
  echo "Artifact path is not absolute" >&2
  exit 1
fi

if [[ ! -f "$ARTIFACT" ]]; then
  echo "Artifact path is not valid" >&2
  exit 1
fi

exit 0
