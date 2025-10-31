#!/usr/bin/env python3
# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

"""
Usage: remap_cwd.py path/to/clang++ [args...]

Runs `path/to/clang++ -ffile-prefix-map=$PWD= [args...]`
"""

import os
import subprocess
import sys


if __name__ == "__main__":
    cwd = os.getcwd()
    # Add trailing slash
    cwd = os.path.join(cwd, "")

    ret = subprocess.call(
        [
            sys.argv[1],
            f"-ffile-prefix-map={cwd}=",
            *sys.argv[2:],
        ],
    )
    sys.exit(ret)
