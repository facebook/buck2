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
if [ "$DIRNAME" != "examples" ] && [ "$BASENAME" != "prelude" ]; then
    echo "$0 should be run from directory 'examples/prelude'"
    exit 1
fi

# Bring the OCaml toolchain into scope.
eval "$(opam env --switch=default --set-switch)"

# Link 'prelude'.
if [ ! -L prelude ]; then
    ln -s "$(realpath ../../prelude)" prelude
else
    echo "Link 'prelude' exists. To overwrite it, first remove it and run $0 again"
fi

# Link 'third-party/ocaml/standard_library'.
if [ ! -L third-party/ocaml/standard_library ]; then
  (cd third-party/ocaml && ln -s "$(ocamlopt.opt -config | grep standard_library: | awk '{ print $2 }' )" standard_library)
else
    echo "Link 'third-party/ocaml/standard_library' exists. To overwrite it, first remove it and run $0 again"
fi

# Link 'third-party/ocaml/opam'.
if [ ! -L third-party/ocaml/opam ]; then
  (cd third-party/ocaml && ln -s "$OPAM_SWITCH_PREFIX" opam)
else
    echo "Link 'third-party/ocaml/opam' exists. To overwrite it, first remove it and run $0 again"
fi
