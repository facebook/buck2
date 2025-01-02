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


@buck_test()
@env(
    "BUCK2_HARD_ERROR",
    "true",
)
async def test_missing_source_file_when_hard_errors_enabled(buck: Buck) -> None:
    await expect_failure(
        buck.uquery("//package1:"),
        stderr_regex="Source file `non_existent_source_file.txt` does not exist as a member of package `prelude//package1`",
    )


@buck_test()
@env(
    "BUCK2_HARD_ERROR",
    "false",
)
async def test_missing_source_file_when_hard_errors_disabled(buck: Buck) -> None:
    await buck.uquery("//package1:")
