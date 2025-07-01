#!/bin/bash
# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

set -euo pipefail

# Check for ghc-9.10.1.
if [ ! -e "$HOME/.ghcup/ghc/9.10.1/lib/ghc-9.10.1" ]; then
    echo "$HOME/.ghcup/ghc/9.10.1/lib/ghc-9.10.1 does not exist. First \"ghcup install ghc 9.10.1\" and then try running $0 again."
    exit 3
fi

# Link 'third-party/haskell/ghc'.
if [ ! -L third-party/haskell/ghc ]; then
  (cd third-party/haskell && ln -s "$HOME/.ghcup/ghc/9.10.1/lib/ghc-9.10.1" ghc)
else
    echo "Link 'third-party/haskell/ghc' exists. To overwrite it, first remove it and run $0 again"
    exit 2
fi
