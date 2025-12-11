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

from .debug import debug_dump_replay
from .ios import prepare_simulator
from .simulator import SimulatorType


def _args_parser() -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser(
        description="Utility to set up simulators which are used by buck to run tests locally."
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
        type=SimulatorType,
        choices=[e.value for e in SimulatorType],
        required=True,
        help=f"""
            Type of required resources.
            Pass `{SimulatorType.iphoneUnbooted}` to get an unbooted iPhone simulator.
            Pass `{SimulatorType.iphoneBooted}` to get a booted iPhone simulator.
            Pass `{SimulatorType.ipad}` to get an iPad simulator.
            Pass `{SimulatorType.watch}` to get an Apple Watch simulator.
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


def main() -> None:
    debug_dump_replay()
    args = _args_parser().parse_args()
    device = args.device if args.device else args.type.default_device()
    sim = asyncio.run(
        prepare_simulator(
            simulator_manager=args.simulator_manager,
            simulator_type=args.type,
            os_version=args.os_version,
            device=device,
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
