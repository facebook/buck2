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


@buck_test(data_dir="identifier")
async def test_analysis_action_ids_unique_identifier_within_category(
    buck: Buck,
) -> None:
    await expect_failure(
        buck.audit("providers", "//:yyy"),
        stderr_regex="Action category `foo` contains duplicate identifier `x`",
    )


@buck_test(data_dir="category")
async def test_analysis_action_ids_unique_singleton_category(buck: Buck) -> None:
    await expect_failure(
        buck.audit("providers", "//:zzz"),
        stderr_regex="Analysis produced multiple actions with category `foo` and at least one of them had no identifier",
    )
