#!/usr/bin/env python3
# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

# Changes directory and then runs a command

import subprocess
import sys


def main():
    d = sys.argv[1]
    res = subprocess.run(sys.argv[2:], cwd=d)
    sys.exit(res.returncode)


if __name__ == "__main__":
    main()
