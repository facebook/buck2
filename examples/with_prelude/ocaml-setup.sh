#!/bin/bash
# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

# The commands in this script assume an activated opam switch. They create
# symlinks in the 'third-party/opam' directory to support building the example
# OCaml targets.

set -euo pipefail

# Check for opam.
if ! command -v opam &> /dev/null
then
    echo "Failed to run 'opam'. An installation of 'opam' is a required dependency for building the example OCaml targets."
    exit 1
fi

# Check that an opam switch is active.
set +u
if [ -z "$OPAM_SWITCH_PREFIX" ]; then
    echo "OPAM_SWITCH_PREFIX is undefined. First execute \`eval \$(opam env)\` and then try running $0 again."
    exit 3
fi
set -u

# Link 'third-party/ocaml/opam'.
if [ ! -L third-party/ocaml/opam ]; then
  (cd third-party/ocaml && ln -s "$OPAM_SWITCH_PREFIX" opam)
else
    echo "Link 'third-party/ocaml/opam' exists. To overwrite it, first remove it and run $0 again"
    exit 2
fi
