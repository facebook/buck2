#!/bin/bash
# This command runs slow on both local and RE, but it runs slower locally. The
# upshot is that local should start before RE is done, but RE will finish
# first, and then will cancel local. If it doesn't cancel local, the test will
# fail.

OUT="$1"

if [[ -f "/run/re_worker/beacon" ]]; then
  sleep 5
  touch "$OUT"
  exit 0
fi

sleep 10
exit 1
