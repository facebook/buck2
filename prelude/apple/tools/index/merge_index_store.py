# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

import argparse
import os
import subprocess
import sys
from argparse import Namespace
from concurrent.futures import as_completed, ThreadPoolExecutor
from pathlib import Path

MAX_WORKDERS = 8


def parse_arguments() -> Namespace:
    parser = argparse.ArgumentParser()
    parser.add_argument("-d", "--dest", type=str, required=True)
    parser.add_argument("-s", "--sources", nargs="+", type=str, required=True)
    parser.add_argument("--dummy-output", type=str, required=False)
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
    directories = args.sources

    Path(destination).mkdir(parents=True, exist_ok=True)
    if args.dummy_output:
        # For dummy output, create a file to avoid empty output for buck2
        Path(args.dummy_output).touch()

    with ThreadPoolExecutor(max_workers=MAX_WORKDERS) as executor:
        futures = [
            executor.submit(merge_directories, index_dir, destination)
            for index_dir in directories
        ]
        for future in as_completed(futures):
            future.result()  # This will raise any exceptions that occurred


if __name__ == "__main__":
    main()
