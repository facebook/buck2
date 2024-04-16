# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

# pyre-strict

import json
from dataclasses import dataclass
from enum import Enum
from typing import List, Optional

from dataclasses_json import dataclass_json


class SimState(str, Enum):
    booted = "Booted"
    shutdown = "Shutdown"


@dataclass_json
@dataclass
class IdbTarget:
    name: str
    os_version: str
    udid: str
    state: SimState
    host: str = ""
    port: int = 0


@dataclass
class SimulatorInfo:
    udid: str
    device_set_path: str


def managed_simulators_from_stdout(stdout: Optional[str]) -> List[IdbTarget]:
    if not stdout:
        return []
    targets = map(
        # pyre-ignore[16]: `from_dict` is dynamically provided by `dataclass_json`
        IdbTarget.from_dict,
        json.loads(stdout),
    )
    return list(targets)
