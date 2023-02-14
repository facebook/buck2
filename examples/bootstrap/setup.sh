#!/bin/bash
# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

CWD="$(pwd)"
BASENAME="$(basename "$CWD")"
DIRNAME="$(basename "$(dirname "$CWD")")"
if [ "$DIRNAME" != "examples" ] && [ "$BASENAME" != "bootstrap" ]; then
    echo "$0 should be run from directory 'examples/bootstrap'"
    exit 1
fi

# Link 'prelude'.
if [ ! -L prelude ]; then
    ln -s "$(realpath ../../prelude)" prelude
else
    echo "Link 'prelude' exists. To overwrite it, first remove it and run $0 again"
fi
