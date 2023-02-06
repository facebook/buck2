#!/usr/bin/env fbpython
# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

# pyre-unsafe

import argparse
import json
import pathlib
import shutil
import subprocess
import sys


def _parse_args():
    parser = argparse.ArgumentParser(
        description="Tool for converting used_classes.json to a format that buck2 understands."
    )

    parser.add_argument(
        "--always-used-files",
        type=pathlib.Path,
        required=True,
    )

    parser.add_argument(
        "--used-classes",
        type=pathlib.Path,
        nargs="+",
    )

    parser.add_argument(
        "--output",
        type=pathlib.Path,
        required=True,
    )

    return parser.parse_known_args()


def rewrite_dep_file(always_used_files_path, used_classes_paths, dst_path):
    """
    Convert a used_classes.json to a depfile suitable for use by Buck2. The files we
    rewrite are JSON where the keys are the jars that were used.
    """
    shutil.copyfile(always_used_files_path, dst_path)

    all_used_classes = []
    for used_classes_path in used_classes_paths:
        with open(used_classes_path) as f:
            used_classes_body = f.read()
            used_classes_map = json.loads(used_classes_body)
            all_used_classes.extend(used_classes_map.keys())

    with open(dst_path, "a") as f:
        f.write("\n")
        f.write("\n".join(all_used_classes))


def main():
    """
    Unparsed args are the command that should be run.
    """
    parsed_args, unparsed_args = _parse_args()

    ret = subprocess.call(unparsed_args)
    if ret == 0:
        rewrite_dep_file(
            parsed_args.always_used_files, parsed_args.used_classes, parsed_args.output
        )
    sys.exit(ret)


if __name__ == "__main__":
    main()
