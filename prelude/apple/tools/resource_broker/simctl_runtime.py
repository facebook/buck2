# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

import json
from dataclasses import dataclass, field
from typing import List, Optional

from dataclasses_json import config, dataclass_json

from .utils import execute_generic_text_producing_command


@dataclass_json
@dataclass
class XCSimDevice:
    name: str
    product_family: str = field(metadata=config(field_name="productFamily"))


@dataclass_json
@dataclass
class XCSimRuntime:
    name: str
    version: str
    supported_device_types: List[XCSimDevice] = field(
        metadata=config(field_name="supportedDeviceTypes")
    )


@dataclass_json
@dataclass
class _XCSimRuntimes:
    runtimes: List[XCSimRuntime]


def _list_ios_runtimes_command() -> List[str]:
    return [
        "xcrun",
        "simctl",
        "list",
        "runtimes",
        "iOS",
        "available",
        "--json",
    ]


def _simctl_runtimes_from_stdout(stdout: Optional[str]) -> List[XCSimRuntime]:
    if not stdout:
        return []
    data = json.loads(stdout)
    # pyre-ignore[16]: `from_dict` is dynamically provided by `dataclass_json`
    return _XCSimRuntimes.from_dict(data).runtimes


async def list_ios_runtimes() -> List[XCSimRuntime]:
    stdout = await execute_generic_text_producing_command(
        name="list iOS runtimes", cmd=_list_ios_runtimes_command()
    )
    return _simctl_runtimes_from_stdout(stdout)
