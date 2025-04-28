#!/usr/bin/env bash
# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

# We use this script to load the data for quick local iteration on the frontend
# $1: buck2 executable
set -e
$1 explain --output /dev/null --fbs-dump /tmp/fbs
cd $(dirname "$0")
echo "export const DATA = '$(cat /tmp/fbs)';" > src/data.ts
cp -rfX "$(buck2 build //buck2/app/buck2_explain:schema_ts --show-full-simple-output)" src
