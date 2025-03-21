# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

# pyre-strict


from buck2.tests.e2e_util.api.buck import Buck
from buck2.tests.e2e_util.asserts import expect_failure
from buck2.tests.e2e_util.buck_workspace import buck_test, env


@buck_test(
    setup_eden=False,
    extra_buck_config={"buck2": {"file_watcher": "edenfs"}},
)
@env("BUCK2_HARD_ERROR", "false")
async def test_watchman_fallback(buck: Buck) -> None:
    res = await buck.targets("root//:")
    # fallback to watchman
    assert "Watchman fresh instance" in res.stderr


@buck_test(
    setup_eden=False,
    extra_buck_config={"buck2": {"file_watcher": "edenfs"}},
)
async def test_eden_fail(buck: Buck) -> None:
    res = await expect_failure(buck.targets("root//:"))
    assert "Failed to connect to EdenFS" in res.stderr
