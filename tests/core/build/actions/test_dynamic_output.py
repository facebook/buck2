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
from buck2.tests.e2e_util.helper.golden import golden, sanitize_stderr


@buck_test(data_dir="everything")
async def test_dynamic_output(buck: Buck) -> None:
    await buck.build("root//:")


@buck_test(data_dir="everything_new")
async def test_dynamic_output_new(buck: Buck) -> None:
    await buck.build("root//:")


@buck_test(data_dir="empty_dynamic_list")
async def test_empty_dynamic_list(buck: Buck) -> None:
    await buck.build("root//:empty_test")


@buck_test(data_dir="artifact_eq_bug")
async def test_artifact_eq_bug(buck: Buck) -> None:
    await buck.build("root//:bug")


@buck_test(data_dir="analysis_failure")
async def test_dynamic_output_analysis_failure(buck: Buck) -> None:
    result = await expect_failure(
        buck.build("root//:analysis_failure"),
        stderr_regex="Analysis failed: this is a test failure message",
    )
    golden(
        output=sanitize_stderr(result.stderr),
        rel_path="analysis_failure/golden/test_dynamic_output_analysis_failure.golden.txt",
    )
