# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

# pyre-strict

import asyncio
from typing import cast, List

from .idb_companion import IdbCompanion

from .utils import IdbCompanionProcess, spawn_companion, wait_for_idb_companions


def _boot_macos_companion_command(grpc_domain_sock: str) -> List[str]:
    return [
        "idb_companion",
        "--udid",
        "mac",
        "--grpc-domain-sock",
        grpc_domain_sock,
    ]


async def macos_idb_companions() -> List[IdbCompanion]:
    addresses = [(i, f"/tmp/buck2_idb_companion_mac_{i}") for i in range(10)]
    awaitables = [
        spawn_companion(
            command=_boot_macos_companion_command(addr),
            log_file_suffix=f"macos_companion_{i}.log",
        )
        for i, addr in addresses
    ]
    results = await asyncio.gather(*awaitables, return_exceptions=True)

    if exception := next(filter(lambda r: isinstance(r, BaseException), results), None):
        [r.cleanup() for r in results if isinstance(r, IdbCompanionProcess)]
        raise cast(BaseException, exception)

    return await wait_for_idb_companions(cast(List[IdbCompanionProcess], results))
