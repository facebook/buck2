#!/usr/bin/env python3
# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

"""
This script is used to transform an opaque directory Buck artifact filled with
files into a number of Buck artifacts that can each be operated on individually.

This script is used to take a directory filled with object files created
through dynamic_output and a list of name, output destination path pairs.
For each such pair, we look for a file in the directory matching the name,
and copy it to the destination path.

This allows buck to declare and operate on each file individually, rather than
the entire opaque directory.
"""

import argparse
import pathlib
import shutil
import sys
from typing import List

EXIT_SUCCESS = 0


def main(argv: List[str]) -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("--objects_dir")
    parser.add_argument("--object_to_map", nargs=2, action="append")
    args = parser.parse_args(argv[1:])

    copy_map = {}
    for object_mapping in args.object_to_map:
        directory_member_name = object_mapping[0]
        dest = object_mapping[1]
        copy_map[directory_member_name] = pathlib.Path(dest)

    directory_to_map = pathlib.Path(args.objects_dir)
    for file in directory_to_map.iterdir():
        if file.name in copy_map:
            shutil.copy(file, copy_map[file.name])
        else:
            raise Exception("Object mapping is missing a file.")

    return EXIT_SUCCESS


if __name__ == "__main__":
    sys.exit(main(sys.argv))
