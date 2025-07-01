#!/usr/bin/env python3
# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

# Wrapper around `cargo llvm-lines` that redirects stdout to a file

import subprocess
import sys


def main():
    if len(sys.argv) < 3:
        raise ValueError("expected at least 2 arguments")
    out = sys.argv[1]
    with open(out, "w") as f:
        res = subprocess.run(sys.argv[2:], stdout=f)
    sys.exit(res.returncode)


if __name__ == "__main__":
    main()
