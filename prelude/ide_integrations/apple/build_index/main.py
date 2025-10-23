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
    parser.add_argument(
        "-s",
        "--subtarget",
        type=str,
        help="Subtarget to build (e.g., swift-index-store)",
    )
    parser.add_argument(
        "--isolation-dir",
        type=str,
        help="Isolation directory for buck2",
    )
    return parser.parse_args()


def parse_targets(target_args: list[str]) -> list[str]:
    """Parse target arguments, handling multiple targets separated by newlines or spaces."""
    targets = []
    for target_arg in target_args:
        split_targets = []
        for line in target_arg.split("\n"):
            split_targets.extend(line.split())
        targets.extend([t.strip() for t in split_targets if t.strip()])
    return targets


def run_bxl_and_merge_index(
    targets: list[str],
    dest: str,
    configs: list[str] = None,
    subtarget: str = None,
    isolation_dir: str = None,
) -> None:
    targets_str = " ".join(targets)
    config_flags = ""
    subtarget_arg = f" --subtarget {subtarget}" if subtarget else ""
    isolation_dir_flag = f" --isolation-dir {isolation_dir}" if isolation_dir else ""
    if configs:
        config_flags = " ".join([f"-c {config}" for config in configs])
        command = f"buck2{isolation_dir_flag} bxl {config_flags} {BXL} -- --target {targets_str}{subtarget_arg}"
    else:
        command = f"buck2{isolation_dir_flag} bxl {BXL} -- --target {targets_str}{subtarget_arg}"
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
        error_output = process.stderr.read() if process.stderr else ""
        print(f"\033[91mError: {error_output}\033[0m")
        if process.stderr:
            process.stderr.close()
        raise SystemExit(1)


def main() -> None:
    args = parse_arguments()
    Path(args.dest).mkdir(parents=True, exist_ok=True)
    targets = parse_targets(args.target)
    run_bxl_and_merge_index(
        targets, args.dest, args.config, args.subtarget, args.isolation_dir
    )


if __name__ == "__main__":
    main()
