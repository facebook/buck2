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
from typing import List, Optional

from dataclasses_json import dataclass_json


class SimState(str, Enum):
    booted = "Booted"
    booting = "Booting"
    creating = "Creating"
    shutdown = "Shutdown"
    shutting_down = "Shutting Down"
    unknown = "Unknown"


@dataclass_json
@dataclass
class IdbTarget:
    name: str = ""
    os_version: str = ""
    udid: str = ""
    state: SimState = SimState.shutdown

    def is_valid(self):
        return self.os_version != "" and self.udid != ""


@dataclass
class SimulatorInfo:
    udid: str
    device_set_path: str


def managed_simulator_from_stdout(stdout: Optional[str]) -> IdbTarget:
    if not stdout:
        return None
    # pyre-ignore[16]: `from_dict` is dynamically provided by `dataclass_json`
    return IdbTarget.from_dict(json.loads(stdout))


def managed_simulators_list_from_stdout(stdout: Optional[str]) -> List[IdbTarget]:
    if not stdout:
        return []
    targets = map(
        # pyre-ignore[16]: `from_dict` is dynamically provided by `dataclass_json`
        IdbTarget.from_dict,
        json.loads(stdout),
    )
    return list(filter(lambda target: target.is_valid(), targets))
