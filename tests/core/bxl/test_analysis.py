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


@buck_test()
async def test_bxl_analysis(buck: Buck) -> None:
    result = await buck.bxl(
        "//analysis.bxl:providers_test",
    )

    lines = result.stdout.splitlines()
    assert "provides_foo_foo" in lines[0]
    assert "provides_foo_foo" in lines[1]

    result = await buck.bxl(
        "//analysis.bxl:dependency_test",
    )

    assert result.stdout.splitlines() == [
        "Dependency",
        "root//:stub (<unspecified>)",
    ]


@buck_test(write_invocation_record=True)
async def test_bxl_analysis_missing_subtarget(buck: Buck) -> None:
    res = await expect_failure(
        buck.bxl(
            "//analysis.bxl:missing_subtarget_test",
        ),
        stderr_regex="requested sub target named `missing_subtarget` .* is not available",
    )

    record = res.invocation_record()
    errors = record["errors"]

    assert len(errors) == 1
    assert errors[0]["category"] == "USER"


@buck_test()
async def test_bxl_analysis_unconfigured_target_error(buck: Buck) -> None:
    await expect_failure(
        buck.bxl("//analysis.bxl:unconfigured_target_error_test"),
        stderr_regex="Type of parameter `labels` doesn't match",
    )
