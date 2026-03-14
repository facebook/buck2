#!/usr/bin/env python3
# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

"""
Usage: remap_cwd.py path/to/compiler [args...]

Runs `path/to/compiler -ffile-prefix-map=$PWD/= [args...]`

Also adds -Wa,--debug-prefix-map=$PWD=. so that the assembler remaps
DW_AT_comp_dir for hand-written assembly files. GCC does not pass
-ffile-prefix-map through to the assembler, so this is needed for
assembly (.s, .S, .sx) sources to get relative debug-info paths.
"""

import os
import subprocess
import sys


if __name__ == "__main__":
    cwd = os.getcwd()

    ret = subprocess.call(
        [
            sys.argv[1],
            f"-ffile-prefix-map={cwd}/=",
            f"-Wa,--debug-prefix-map={cwd}=.",
            *sys.argv[2:],
        ],
    )
    sys.exit(ret)
