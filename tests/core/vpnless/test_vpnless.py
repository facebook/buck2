# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

# pyre-strict


from __future__ import annotations

import json

from buck2.tests.e2e_util.api.buck import Buck
from buck2.tests.e2e_util.buck_workspace import buck_test, env

# Note: for test scenarios where we want to ensure the `cpe` crate reports no
# vpnless support, we have to define the env var but =0. Otherwise these
# tests will erroneously fail on macOS.


@buck_test()
@env("CPE_RUST_X2P_SUPPORTS_VPNLESS", "0")
@env("CPE_RUST_X2P_HTTP1_PROXY_PORT", "5555")
async def test_vpnless_disabled_by_host(buck: Buck) -> None:
    # Get a daemon to start
    await buck.build()
    result = await buck.status()
    status = json.loads(result.stdout)
    assert not status["supports_vpnless"], (
        "vpnless should be disabled by non-supporting host"
    )


@buck_test()
@env("CPE_RUST_X2P_SUPPORTS_VPNLESS", "1")
# Need to set this so Windows doesn't go down the unix socket codepath.
@env("CPE_RUST_X2P_HTTP1_PROXY_PORT", "5555")
async def test_vpnless_enabled(buck: Buck) -> None:
    # Get a daemon to start
    await buck.build()
    result = await buck.status()
    status = json.loads(result.stdout)
    assert status["supports_vpnless"], "vpnless should be enabled by host"
