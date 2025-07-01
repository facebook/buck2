# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.


import argparse
import multiprocessing
import subprocess
from argparse import Namespace
from pathlib import Path

from merge_index_store import merge_directory

BXL = "prelude//ide_integrations/apple/build_index/swift_index_store.bxl:main"


def parse_arguments() -> Namespace:
    parser = argparse.ArgumentParser()
    parser.add_argument("-d", "--dest", type=str, required=True)
    parser.add_argument("-t", "--target", type=str, required=True, nargs="+")
    parser.add_argument(
        "-c", "--config", action="append", help="Buck configuration flags"
    )
    return parser.parse_args()


def run_bxl_and_merge_index(
    targets: list[str], dest: str, configs: list[str] = None
) -> None:
    targets_str = " ".join(targets)
    config_flags = ""
    if configs:
        config_flags = " ".join([f"-c {config}" for config in configs])
        command = f"buck2 bxl {config_flags} {BXL} -- --target {targets_str}"
    else:
        command = f"buck2 bxl {BXL} -- --target {targets_str}"
    process = subprocess.Popen(
        command,
        shell=True,
        stdout=subprocess.PIPE,
        text=True,
        bufsize=1,  # Line buffering to ensure real-time output capture
    )

    # Process pool for running merge_directory in separate processes
    # Default number of pool processes is the number of CPUs
    with multiprocessing.Pool() as pool:
        processes = []
        for line in iter(process.stdout.readline, ""):
            index_store_path = line.strip()
            if index_store_path:
                # Run merge_directory in a separate process
                result = pool.apply_async(merge_directory, (index_store_path, dest))
                processes.append(result)

        # Wait for all processes to complete
        for p in processes:
            p.get()

    process.stdout.close()
    return_code = process.wait()

    if return_code != 0:
        error_output = process.stderr.read()
        print(f"\033[91mError: {error_output}\033[0m")
        process.stderr.close()
        raise SystemExit(1)


def main() -> None:
    args = parse_arguments()
    Path(args.dest).mkdir(parents=True, exist_ok=True)
    run_bxl_and_merge_index(args.target, args.dest, args.config)


if __name__ == "__main__":
    main()
