#!/usr/bin/env python3
# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

# pyre-strict

"""
Usage: remap_cwd.py path/to/compiler [args...]

Runs `path/to/compiler [args...] -ffile-prefix-map=$PWD/= -Wa,--debug-prefix-map=$PWD=.`

Flags are appended so that compiler wrappers using "$@" see the real binary as $1.
-Wa,--debug-prefix-map is needed because GCC does not pass -ffile-prefix-map through to the
assembler for .s/.S/.sx files.
"""

import os
import subprocess
import sys


if __name__ == "__main__":
    cwd = os.getcwd()

    ret = subprocess.call(
        [
            *sys.argv[1:],
            f"-ffile-prefix-map={cwd}/=",
            f"-Wa,--debug-prefix-map={cwd}=.",
        ],
    )
    sys.exit(ret)
