# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

# pyre-strict


from buck2.tests.e2e_util import asserts
from buck2.tests.e2e_util.api.buck import Buck
from buck2.tests.e2e_util.asserts import expect_failure
from buck2.tests.e2e_util.buck_workspace import buck_test


@buck_test()
async def test_build_nested_subtargets(buck: Buck) -> None:
    result = await buck.build(
        "//:nested[sub][nested_sub]",
    )
    build_report = result.get_build_report()

    output = build_report.output_for_target("//:nested", "sub|nested_sub")

    assert output.read_text().rstrip() == "foo_content"
    asserts.assert_not_executable(output)


@buck_test()
async def test_build_nested_subtargets_errors(buck: Buck) -> None:
    await expect_failure(
        buck.build(
            "//:nested[bad]",
        ),
        stderr_regex="Available subtargets are.*sub",
    )

    await expect_failure(
        buck.build(
            "//:nested[sub][bad]",
        ),
        stderr_regex="Available subtargets are.*nested_sub",
    )
