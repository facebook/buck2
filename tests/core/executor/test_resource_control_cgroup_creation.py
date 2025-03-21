# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

# pyre-strict

from __future__ import annotations

from buck2.tests.e2e_util.api.buck import Buck
from buck2.tests.e2e_util.asserts import expect_failure
from buck2.tests.e2e_util.buck_workspace import buck_test


# To not fail listing on Mac or Windows
def test_dummy() -> None:
    pass


@buck_test(skip_for_os=["darwin", "windows"])
async def test_failed_to_start_transient_unit(buck: Buck) -> None:
    await buck.build("//:my_rule", "--no-remote-cache", "--local-only")
    await buck.kill()
    result = await expect_failure(
        buck.build("//:my_rule", "--no-remote-cache", "--local-only")
    )
    # TODO(yurysamkevich): this is a bug, we can't create a scope unit
    # if there is live process in an action cgroup after the action finished execution
    assert "Failed to start transient scope unit" in result.stderr
