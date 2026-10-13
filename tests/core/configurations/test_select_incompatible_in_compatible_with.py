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
async def test_select_incompatible_in_ordinary_attribute_is_incompatible(
    buck: Buck,
) -> None:
    # Requested explicitly, an incompatible target is an error that names the cause.
    await expect_failure(
        buck.build("//:attr_select_incompatible"),
        stderr_regex="is incompatible: select resolved to select_incompatible\\(\\): not for a",
    )


@buck_test()
async def test_select_incompatible_in_target_compatible_with_is_skipped(
    buck: Buck,
) -> None:
    # In `target_compatible_with` the `select_incompatible()` branch makes the target
    # incompatible, like in any other attribute: a package pattern skips it.
    result = await buck.build("//:")
    assert "Skipped 3 incompatible targets:" in result.stderr
    assert "root//:tcw_select_incompatible (" in result.stderr
    await expect_failure(
        buck.build("//:tcw_select_incompatible"),
        stderr_regex="select resolved to select_incompatible\\(\\): not for a",
    )
