# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

import argparse
import sys
from pathlib import Path


def main(argv):
    parser = argparse.ArgumentParser(fromfile_prefix_chars="@")
    parser.add_argument("--output", required=True, type=Path)
    parser.add_argument("files", type=Path, nargs="*")
    args = parser.parse_args(argv[1:])

    if len(args.files) == 0:
        print(
            "usage: concat_files.py --output out.txt in1.txt in2.txt", file=sys.stderr
        )
        return 1

    with open(args.output, "wb") as outfile:
        for f in args.files:
            with open(f, "rb") as infile:
                outfile.write(infile.read())

    return 0


sys.exit(main(sys.argv))
