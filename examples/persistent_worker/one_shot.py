#!/usr/bin/env python3
# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

import argparse
import os
import sys


def main():
    parser = argparse.ArgumentParser(
        fromfile_prefix_chars="@", prog="one_shot", description="One-shot command"
    )
    parser.add_argument("outfile", type=argparse.FileType("w"), help="Output file.")

    args = parser.parse_args()

    print("one-shot.py", file=sys.stderr)
    print("ONE-SHOT START", file=sys.stderr)
    name = os.path.basename(args.outfile.name)
    args.outfile.write(name + "\n")
    args.outfile.close()
    print("ONE-SHOT END", file=sys.stderr)


if __name__ == "__main__":
    main()
