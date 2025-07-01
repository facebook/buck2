# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

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


def merge_directory(source: str, destination: str) -> None:
    if os.path.isdir(source):
        print(f"Merging {source} to {destination}", file=sys.stderr)
        if not source.endswith("/"):
            source = source + "/"

        # Each index store is a directory containing record and unit files.
        #
        # $ ls my_index_store
        # v5/records/93/scope6_var.h-3KOUVOFGN7X93
        # v5/records/93/ShareViewController.swift-336T0RUILP193
        # v5/records/95/host_security.h-NRLXTM4VIC95
        # v5/units/ShareViewController.swift-5TSSN9QOIJ15
        #
        # We want a single destination directory containing all the record and unit
        # files from all the source directories.
        #
        # There's no built-in way to merge directories in buck. In Python, there is
        # `shutil.copytree(source, dest, dirs_exist_ok=True)` but that overwrites
        # files in the destination when there are multiple sources with the same
        # file. That's slower.
        #
        # Instead, use rsync to merge the directories.
        #
        # Use `--no-owner` to ensure the files in the destination directory are
        # owned by the current user, even if some of the input files were owned by
        # root.
        result = subprocess.run(
            [
                "rsync",
                "-a",
                "--ignore-existing",
                "--no-owner",
                "--no-group",
                source,
                destination,
            ],
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
            executor.submit(merge_directory, index_dir, destination)
            for index_dir in directories
        ]
        for future in as_completed(futures):
            future.result()  # This will raise any exceptions that occurred


if __name__ == "__main__":
    main()
