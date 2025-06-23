#!/usr/bin/env python3
# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

import argparse
import platform
import subprocess
from typing import List, Tuple


def parse_arguments() -> Tuple[argparse.Namespace, List[str]]:
    parser = argparse.ArgumentParser(
        add_help=False,  # This allows us to pass --help to the inner command
        description="Builds buck2 locally and then runs it.",
        formatter_class=argparse.RawTextHelpFormatter,
    )
    parser.add_argument(
        "--run-isolation-dir",
        type=str,
        default="",
        help="Isolation dir for the inner command",
    )
    parser.add_argument(
        "--echo-run-cmd",
        action="store_true",
        help="Echo the run command before executing",
    )
    return parser.parse_known_args()


def get_extra_build_params(args: argparse.Namespace) -> List[str]:
    system_platform = platform.system()
    if system_platform == "Windows":
        return ["@fbcode//mode/opt-win"]

    params = ["-m", "opt"]

    arch_platform = platform.machine()
    if arch_platform == "x86_64":
        params.extend(["-m", "x86_64"])
    elif arch_platform == "arm64":
        params.extend(["-m", "arm64"])
    elif arch_platform == "riscv64":
        params.extend(["-m", "riscv64"])

    return params


def build_command(args: argparse.Namespace, extra_args: List[str]) -> List[str]:
    cmd = ["buck2", "run", "fbcode//buck2:buck2_bundle"]
    inner_buck_isolation_dir = (
        args.run_isolation_dir if args.run_isolation_dir else "v2.self"
    )
    inner_buck_isolation_dir_arg = [f"--isolation-dir={inner_buck_isolation_dir}"]

    cmd.extend(get_extra_build_params(args))

    cmd.append("--")
    cmd.extend(inner_buck_isolation_dir_arg)
    cmd.extend(extra_args)

    if args.echo_run_cmd:
        print(" ".join(cmd))

    return cmd


def main() -> None:
    args, extra_args = parse_arguments()
    cmd = build_command(args, extra_args)
    subprocess.run(cmd)


if __name__ == "__main__":
    main()
