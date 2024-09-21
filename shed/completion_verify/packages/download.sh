#!/bin/bash
# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

set -e

dnf download "$1" --destdir "$BUCK_SCRATCH_PATH"
rpm=$(echo "$BUCK_SCRATCH_PATH"/*)
mkdir -p "$2"
rpm2archive - < "$rpm" | tar -xvzf - -C "$(realpath "$2")"
