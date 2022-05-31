#!/bin/bash

if [[ ! -f "/run/re_worker/beacon" ]]; then
  echo "Not inside RE" >&2
  exit 1
fi

exit 0
