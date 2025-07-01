# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

# pyre-strict

import argparse
import asyncio
import json
import sys
from enum import Enum

from .ios import prepare_simulator


def _args_parser() -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser(
        description="Utility to set up iOS simulators which are used by buck to run tests locally."
    )
    parser.add_argument(
        "--simulator-manager",
        required=True,
        type=str,
        help="Tool to manage simulators and their lifecycle. Required.",
    )
    parser.add_argument(
        "--type",
        metavar="<TYPE>",
        type=_ResourceType,
        choices=[e.value for e in _ResourceType],
        required=True,
        help=f"""
            Type of required resources.
            Pass `{_ResourceType.iosUnbootedSimulator}` to get an unbooted iOS simulator.
            Pass `{_ResourceType.iosBootedSimulator}` to get a booted iOS simulator.
        """,
    )
    parser.add_argument(
        "--os_version",
        required=False,
        type=str,
        help="OS version to use for simulator",
    )
    parser.add_argument(
        "--device",
        required=False,
        type=str,
        help="Device to use for simulator",
    )
    return parser


class _ResourceType(str, Enum):
    iosUnbootedSimulator = "ios_unbooted_simulator"
    iosBootedSimulator = "ios_booted_simulator"


def main() -> None:
    args = _args_parser().parse_args()
    booted = args.type == _ResourceType.iosBootedSimulator
    sim = asyncio.run(
        prepare_simulator(
            simulator_manager=args.simulator_manager,
            booted=booted,
            os_version=args.os_version,
            device=args.device,
        )
    )
    result = {
        "resources": [
            {
                "udid": sim.udid,
                "device_set_path": sim.device_set_path,
            }
        ]
    }
    json.dump(result, sys.stdout)


if __name__ == "__main__":
    main()
