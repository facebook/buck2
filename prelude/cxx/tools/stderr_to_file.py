#!/usr/bin/env python3
# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

"""
Usage: stderr_to_file.py --out=path/to/output path/to/clang++ [args...]
"""

import argparse
import shutil
import subprocess
import sys
from pathlib import Path
from typing import List, NamedTuple


class Args(NamedTuple):
    out: Path
    command: List[str]


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--out", type=Path, required=True)
    parser.add_argument("command", nargs=argparse.REMAINDER)
    args = Args(**vars(parser.parse_args()))

    with open(args.out, "w+") as out:
        ret = subprocess.call(args.command, stderr=out)
        out.seek(0)
        shutil.copyfileobj(out, sys.stderr)

    sys.exit(ret)


if __name__ == "__main__":
    main()
