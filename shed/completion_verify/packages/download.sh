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

if [[ $1 =~ fish ]]; then
    # In order to get fish to behave like it's been installed into a relocatable
    # directory, we need to move things out of `usr/`
    mv "$2/usr/"* "$2"
    rmdir "$2/usr"
fi
