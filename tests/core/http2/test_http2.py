# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

# pyre-strict


from __future__ import annotations

import json

from buck2.tests.e2e_util.api.buck import Buck
from buck2.tests.e2e_util.buck_workspace import buck_test


@buck_test()
async def test_http2_enabled(buck: Buck) -> None:
    # Get a daemon to start
    await buck.build()
    result = await buck.status()
    status = json.loads(result.stdout)
    assert status["http2"] is True, "http2 is enabled by default"

    # Insert necessary buckconfig to pick up http2 configuration.
    with open(f"{buck.cwd}/.buckconfig", "a") as buckconfig:
        buckconfig.writelines(["[http]\n", "http2 = false\n"])

    # Get a daemon to start
    await buck.build()
    result = await buck.status()
    status = json.loads(result.stdout)
    assert status["http2"] is False, "http2 was disabled by buckconfig"
