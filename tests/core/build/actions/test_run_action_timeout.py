# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

# pyre-strict


from buck2.tests.e2e_util.api.buck import Buck
from buck2.tests.e2e_util.asserts import expect_failure
from buck2.tests.e2e_util.buck_workspace import buck_test


@buck_test(skip_for_os=["windows"])
async def test_run_action_timeout_expires(buck: Buck) -> None:
    await expect_failure(
        buck.build("//:slow_with_timeout"),
        stderr_regex="timed out after",
    )


@buck_test(skip_for_os=["windows"])
async def test_run_action_timeout_succeeds(buck: Buck) -> None:
    result = await buck.build("//:fast_with_timeout")
    output = result.get_build_report().output_for_target("//:fast_with_timeout")
    assert output.read_text().strip() == "hello"
