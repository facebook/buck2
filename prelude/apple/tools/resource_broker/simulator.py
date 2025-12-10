# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

# pyre-strict

import json
from dataclasses import dataclass
from enum import Enum
from typing import Optional

from dataclasses_json import dataclass_json


class SimulatorState(str, Enum):
    booted = "Booted"
    booting = "Booting"
    creating = "Creating"
    shutdown = "Shutdown"
    shutting_down = "Shutting Down"
    unknown = "Unknown"


class SimulatorType(str, Enum):
    iphoneUnbooted = "iphone_unbooted_simulator"
    iphoneBooted = "iphone_booted_simulator"
    ipad = "ipad_simulator"
    watch = "watch_simulator"

    def booted(self) -> bool:
        return self in (
            SimulatorType.iphoneBooted,
            SimulatorType.ipad,
            SimulatorType.watch,
        )

    def default_device(self) -> str:
        return {
            SimulatorType.iphoneUnbooted: "iPhone 11",
            SimulatorType.iphoneBooted: "iPhone 11",
            SimulatorType.ipad: "iPad (A16)",
            SimulatorType.watch: "Apple Watch Series 10 (46mm)",
        }[self]

    def matches_device_identifier(self, identifier: str) -> bool:
        identifier_prefix = {
            SimulatorType.iphoneUnbooted: "iPhone",
            SimulatorType.iphoneBooted: "iPhone",
            SimulatorType.ipad: "iPad",
            SimulatorType.watch: "Apple-Watch",
        }[self]
        return identifier_prefix in identifier


@dataclass_json
@dataclass
class Simulator:
    name: str = ""
    device_type_identifier: str = ""
    os_version: str = ""
    udid: str = ""
    state: SimulatorState = SimulatorState.shutdown

    def is_type(self, simulator_type: SimulatorType) -> bool:
        return simulator_type.matches_device_identifier(self.device_type_identifier)

    def is_valid(self):
        return self.os_version != "" and self.udid != ""


@dataclass
class SimulatorInfo:
    udid: str
    device_set_path: str


def managed_simulator_from_stdout(stdout: Optional[str]) -> Simulator:
    if not stdout:
        return None
    # pyre-ignore[16]: `from_dict` is dynamically provided by `dataclass_json`
    return Simulator.from_dict(json.loads(stdout))


def managed_simulators_list_from_stdout(stdout: Optional[str]) -> list[Simulator]:
    if not stdout:
        return []
    targets = map(
        # pyre-ignore[16]: `from_dict` is dynamically provided by `dataclass_json`
        Simulator.from_dict,
        json.loads(stdout),
    )
    return list(filter(lambda target: target.is_valid(), targets))
