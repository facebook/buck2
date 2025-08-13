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
from buck2.tests.e2e_util.helper.utils import random_string


@buck_test()
async def test_incremental_action_with_content_based_path(buck: Buck) -> None:
    await expect_failure(
        buck.build(
            "root//:basic_incremental_action",
            "-c",
            "test.use_content_based_path=True",
        ),
        stderr_regex=r"Action is marked with no_outputs_cleanup but output `out` is content-based, which is not allowed.",
    )


@buck_test()
async def test_basic_incremental_action(buck: Buck) -> None:
    result = await buck.run(
        "root//:basic_incremental_action",
        "--local-only",
        "-c",
        f"test.seed={random_string()}",
    )
    assert result.stdout == "foo"
    result = await buck.run(
        "root//:basic_incremental_action",
        "--local-only",
        "-c",
        f"test.seed={random_string()}",
    )
    assert result.stdout == "foo bar"
    result = await buck.run(
        "root//:basic_incremental_action",
        "--local-only",
        "-c",
        f"test.seed={random_string()}",
    )
    assert result.stdout == "foo bar bar"
