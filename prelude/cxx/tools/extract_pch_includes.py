#!/usr/bin/env python3
# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

import argparse
import sys


def main(argv):
    parser = argparse.ArgumentParser()
    parser.add_argument("--header", default=sys.stdout, type=argparse.FileType("wb"))
    parser.add_argument("--source", type=argparse.FileType("wb"))
    parser.add_argument("input")
    args = parser.parse_args(argv[1:])

    with open(args.input, "rb") as f:
        for line in f:
            stripped = line.lstrip()
            # NOTE(agallagher): We should improve this to include all non-C/C++
            # tokens (but this would required understanding `#if*` pragmas
            # properly).
            if stripped and not stripped.startswith((b"//", b"/*", b"*", b"#include")):
                if args.source is not None:
                    args.source.write(line)
                    for line in f:
                        args.source.write(line)
                break
            if args.source is not None:
                args.source.write(b"\n")
            args.header.write(line)


sys.exit(main(sys.argv))
