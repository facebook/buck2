# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

# pyre-strict


from buck2.tests.e2e_util.api.buck import Buck
from buck2.tests.e2e_util.asserts import expect_failure
from buck2.tests.e2e_util.buck_workspace import buck_test


@buck_test(inplace=True)
async def test_conflicting_fbcode_coverage_flags_fail(buck: Buck) -> None:
    await expect_failure(
        buck.test(
            *[
                "--config",
                "fbcode.coverage=true",
                "--config",
                "fbcode.coverage_selective=true",
                "fbcode//testing_frameworks/code_coverage/playground:test",
            ]
        ),
        stderr_regex=r"""fbcode.coverage and fbcode.coverage_selective are both true. Pick one.""",
    )


@buck_test(inplace=True)
async def test_fbcode_coverage_selective_require_filters(buck: Buck) -> None:
    await expect_failure(
        buck.test(
            *[
                "--config",
                "fbcode.coverage_selective=true",
                "fbcode//testing_frameworks/code_coverage/playground:test",
            ]
        ),
        stderr_regex=r"""fbcode.coverage_selective=true with no filters""",
    )
