#!/usr/bin/env bash
# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

# Builds every target twice against bazel-remote behind an nginx gRPC proxy
# that sends GOAWAY every 100 requests: first with an empty cache, uploading
# the results, then after `buck2 clean`, downloading them. Both builds have to
# succeed, see https://github.com/facebook/buck2/issues/1294.
#
# Needs buck2, nginx and bazel-remote, taken from $BUCK2, $NGINX and
# $BAZEL_REMOTE or else from $PATH.

set -euo pipefail

cd "$(dirname "${BASH_SOURCE[0]}")"

BUCK2=${BUCK2:-buck2}
NGINX=${NGINX:-nginx}
BAZEL_REMOTE=${BAZEL_REMOTE:-bazel-remote}

keepalive_requests=$(awk '$1 == "keepalive_requests" { print $2 + 0 }' nginx.conf)

work=$(mktemp -d)
cleanup() {
  "$BUCK2" kill >/dev/null 2>&1 || true
  # shellcheck disable=SC2046
  kill $(jobs -p) 2>/dev/null || true
  wait 2>/dev/null || true
  rm -rf "$work"
}
trap cleanup EXIT

"$BAZEL_REMOTE" \
  --dir "$work/cache" \
  --max_size 1 \
  --grpc_address 127.0.0.1:19092 \
  --http_address 127.0.0.1:19080 \
  --access_log_level none \
  >"$work/bazel-remote.log" 2>&1 &
"$NGINX" -p "$work" -c "$PWD/nginx.conf" -e stderr &

for port in 19092 18443; do
  for _ in $(seq 100); do
    if (exec 3<>"/dev/tcp/127.0.0.1/$port") 2>/dev/null; then
      continue 2
    fi
    sleep 0.1
  done
  echo "Nothing is listening on port $port" >&2
  cat "$work/bazel-remote.log" >&2
  exit 1
done

"$BUCK2" clean
targets=$("$BUCK2" targets : | grep -c ':write_')

echo "::group::Build with an empty cache" >&2
"$BUCK2" build :
echo "::endgroup::" >&2

echo "::group::Build from the cache" >&2
"$BUCK2" clean
"$BUCK2" build :
echo "::endgroup::" >&2

# A failed upload in the first build or a failed lookup in the second one
# doesn't fail the build, it shows up as a local run instead of a cache hit.
executors=$("$BUCK2" log what-ran --format json | grep -o '"executor":"[A-Za-z]*"' | sort | uniq -c || true)
if [[ "$(echo "$executors" | awk '{ print $1, $2 }')" != "$targets \"executor\":\"Cache\"" ]]; then
  echo "Expected all $targets actions to be cache hits in the second build, got:" >&2
  echo "$executors" >&2
  exit 1
fi

goaways=$(awk -v n="$keepalive_requests" '$2 == n' "$work/access.log" | wc -l)
if [[ "$goaways" -eq 0 ]]; then
  echo "nginx never sent GOAWAY, so this tested nothing" >&2
  exit 1
fi

echo "Both builds succeeded across $goaways GOAWAYs" >&2
