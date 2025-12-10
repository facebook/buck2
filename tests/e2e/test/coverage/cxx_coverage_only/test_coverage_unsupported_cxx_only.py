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


@buck_test(inplace=True)
async def test_go_test_selective_coverage_doesnt_produce_coverage(buck: Buck) -> None:
    await expect_failure(
        buck.test(
            *[
                "--config",
                "fbcode.coverage_selective=true",
                "--config",
                "fbcode.cxx_coverage_only=testing_frameworks/code_coverage/go/add.go",
                "fbcode//testing_frameworks/code_coverage/go:test",
                "--",
                "--collect-coverage",
            ]
        ),
        stderr_regex=r"""2 TESTS FATALS
  ⚠ fbcode//testing_frameworks/code_coverage/go:test - Test(Add|Sub)
  ⚠ fbcode//testing_frameworks/code_coverage/go:test - Test(Add|Sub)""",
    )
