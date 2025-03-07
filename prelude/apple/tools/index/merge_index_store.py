# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

# pyre-strict

import argparse
import os
import subprocess
import sys
from argparse import Namespace
from concurrent.futures import as_completed, ThreadPoolExecutor
from pathlib import Path

MAX_WORKERS = 8


def parse_arguments() -> Namespace:
    parser = argparse.ArgumentParser()
    parser.add_argument("-d", "--dest", type=str, required=True)
    parser.add_argument("-s", "--sources", nargs="+", type=str, required=True)
    return parser.parse_args()


def merge_directories(source: str, destination: str) -> None:
    if os.path.isdir(source):
        print(f"Merging {source} to {destination}", file=sys.stderr)
        if not source.endswith("/"):
            source = source + "/"
        # Use rsync to copy files from source to destination
        # shutil.copytree will show file eixst errors when mergeing parallelly
        result = subprocess.run(
            ["rsync", "-a", "--ignore-existing", source, destination],
            stderr=subprocess.PIPE,
            text=True,
        )
        if result.returncode != 0:
            raise Exception(
                f"Failed to merge {source} to {destination}:\n\t{result.stderr}"
            )
    else:
        raise Exception(f"Directory {source} does not exist or is not a directory")


def main() -> None:
    args = parse_arguments()
    destination = args.dest

    directories = []
    for source in args.sources:
        if source.startswith("@"):
            with open(source[1:]) as f:
                for line in f.readlines():
                    directories.append(line.strip())
        else:
            directories.append(source)

    Path(destination).mkdir(parents=True, exist_ok=True)

    with ThreadPoolExecutor(max_workers=MAX_WORKERS) as executor:
        futures = [
            executor.submit(merge_directories, index_dir, destination)
            for index_dir in directories
        ]
        for future in as_completed(futures):
            future.result()  # This will raise any exceptions that occurred


if __name__ == "__main__":
    main()
