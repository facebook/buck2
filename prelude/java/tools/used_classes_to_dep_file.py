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
import os
import pathlib
import shutil
import subprocess
import sys


def _parse_args():
    parser = argparse.ArgumentParser(
        description="Tool for converting used_classes.json to a format that buck2 understands."
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

    parser.add_argument(
        "--jar-to-jar-dir-map",
        type=pathlib.Path,
    )

    return parser.parse_known_args()


def rewrite_dep_file(used_classes_paths, dst_path, jar_to_jar_dir_map_file):
    """
    Convert a used_classes.json to a depfile suitable for use by Buck2. The files we
    rewrite are JSON where the keys are the jars that were used.
    """
    jar_to_jar_dir_map = {}
    if jar_to_jar_dir_map_file is not None:
        with open(jar_to_jar_dir_map_file, "r") as f:
            for line in f.readlines():
                jar, jar_dir = line.strip().split()
                jar_to_jar_dir_map[jar] = jar_dir

    all_used_classes = []
    for used_classes_path in used_classes_paths:
        with open(used_classes_path) as f:
            used_classes_body = f.read()
            used_classes_map = json.loads(used_classes_body)
            for used_jar, used_classes in used_classes_map.items():
                used_jar_dir = jar_to_jar_dir_map.get(used_jar)
                if used_jar_dir is None:
                    all_used_classes.append(used_jar)
                else:
                    all_used_classes.extend(
                        [
                            os.path.join(used_jar_dir, used_class)
                            for used_class in used_classes.keys()
                        ]
                    )

    with open(dst_path, "w") as f:
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
            parsed_args.used_classes,
            parsed_args.output,
            parsed_args.jar_to_jar_dir_map,
        )
    sys.exit(ret)


if __name__ == "__main__":
    main()
