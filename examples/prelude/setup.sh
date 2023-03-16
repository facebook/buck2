# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

# The commands in this script are to be executed in the current shell
# and so invoke it via the builtin 'source' command e.g. `source
# setup.sh`. It assumes an opam installation. In addition to
# symlinking the 'prelude' directory it activates the 'default' opam
# switch and create symlinks in the 'third-party/opam' directory to
# support building the example OCaml targets.

if ! command -v opam &> /dev/null
then
    echo "opam is not installed, which is a dependency for building targets in ocaml."
    exit
fi

# Bring the OCaml toolchain into scope.
eval "$(opam env --switch=default --set-switch)"

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
