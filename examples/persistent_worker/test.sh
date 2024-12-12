#!/usr/bin/env bash
# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

set -euo pipefail

echo "::group::Local build without persistent worker" >&2
echo '<file:.buckconfig.no-workers>' > .buckconfig.local
buck2 clean; buck2 build : -vstderr
echo "# Verifying Buck2 log" >&2
buck2 log what-ran --show-std-err --format json \
  | jq -s '
      [
        .[]
        | select(.identity | startswith("root//:demo-"))
      ]
      | if length == 4 then
          .
        else
          error("expected 4 demo targets, got " + (length | tostring))
        end
      | .[]
      | if .reproducer.executor == "Local" and (.std_err | startswith("one-shot.py")) then
          true
        else
          error("expected local without persistent worker, got " + ([.reproducer.executor, .std_err] | tostring))
        end
    '
echo "::endgroup::" >&2

echo "::group::Local build with persistent worker" >&2
echo '<file:.buckconfig.local-persistent-workers>' > .buckconfig.local
buck2 clean; buck2 build : -vstderr
echo "# Verifying Buck2 log" >&2
buck2 log what-ran --show-std-err --format json \
  | jq -s '
      [
        .[]
        | select(.identity | startswith("root//:demo-"))
      ]
      | if length == 5 then
          .
        else
          error("expected 5 demo targets, got " + (length | tostring))
        end
      | .[]
      | if (.reproducer.executor == "Worker" or .reproducer.executor == "WorkerInit") and (.std_err | startswith("Buck2 persistent worker")) then
          true
        else
          error("expected local without persistent worker, got " + ([.reproducer.executor, .std_err] | tostring))
        end
    '
echo "::endgroup::" >&2

echo "::group::Remote build without persistent worker" >&2
if [[ -z ${BUILDBUDDY_API_KEY:+x} ]]; then
  echo "::notice file=$(realpath --relative-to=../.. ${BASH_SOURCE[0]}),line=${LINENO}::SKIPPED Missing BuildBuddy token. See examples/persistent_worker/README.md" >&2
else
  echo '<file:.buckconfig.buildbuddy>' > .buckconfig.local
  buck2 clean; buck2 build : -vstderr
  echo "# Verifying Buck2 log" >&2
  buck2 log what-ran --show-std-err --format json \
    | jq -s '
        [
          .[]
          | select(.identity | startswith("root//:demo-"))
        ]
        | if length == 4 then
            .
          else
            error("expected 4 demo targets, got " + (length | tostring))
          end
        | .[]
        | if .reproducer.executor == "Re" and (.std_err | startswith("one-shot.py")) then
            true
          else
            error("expected local without persistent worker, got " + ([.reproducer.executor, .std_err] | tostring))
          end
      '
fi
echo "::endgroup::" >&2

echo "::group::Remote build with persistent worker" >&2
if [[ -z ${BUILDBUDDY_API_KEY:+x} ]]; then
  echo "::notice file=$(realpath --relative-to=../.. ${BASH_SOURCE[0]}),line=${LINENO}::SKIPPED Missing BuildBuddy token. See examples/persistent_worker/README.md" >&2
else
  echo '<file:.buckconfig.buildbuddy-persistent-workers>' > .buckconfig.local
  buck2 clean; buck2 build : -vstderr
  echo "# Verifying Buck2 log" >&2
  buck2 log what-ran --show-std-err --format json \
    | jq -s '
        [
          .[]
          | select(.identity | startswith("root//:demo-"))
        ]
        | if length == 4 then
            .
          else
            error("expected 4 demo targets, got " + (length | tostring))
          end
        | .[]
        | if .reproducer.executor == "Re" and (.std_err | startswith("Bazel persistent worker")) then
            true
          else
            error("expected remote persistent worker, got " + ([.reproducer.executor, .std_err] | tostring))
          end
      '
fi
echo "::endgroup::" >&2
