#!/usr/bin/env python3
# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

"""
Runs a command and writes its stdout to a file, on every host OS.

Usage: stdout_to_file.py --out=path/to/output path/to/command [args...]
"""

import argparse
import subprocess
import sys


def main() -> int:
    parser = argparse.ArgumentParser(fromfile_prefix_chars="@")
    parser.add_argument("--out", required=True)
    parser.add_argument("command", nargs=argparse.REMAINDER)
    args = parser.parse_args()
    with open(args.out, "wb") as out:
        return subprocess.run(args.command, stdout=out).returncode


if __name__ == "__main__":
    sys.exit(main())
