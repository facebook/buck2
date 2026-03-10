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


@buck_test(inplace=False)
async def test_platform_conflicting_constraint_values_fails(buck: Buck) -> None:
    """Test that platform() with conflicting constraint_values for the same
    constraint_setting produces a clear error."""
    # Use audit providers to force analysis of the platform rule
    result = await expect_failure(
        buck.audit("providers", "root//:conflicting_platform")
    )
    assert "Conflicting constraint_values for constraint_setting" in result.stderr
    assert "root//:os" in result.stderr
    assert (
        "Multiple constraint_values for the same constraint_setting are not allowed"
        in result.stderr
    )


@buck_test(inplace=False)
async def test_platform_non_conflicting_constraint_values_succeeds(buck: Buck) -> None:
    """Test that platform() with non-conflicting constraint_values works."""
    await buck.audit("providers", "root//:valid_platform")


@buck_test(inplace=False)
async def test_config_setting_allows_constraint_values(buck: Buck) -> None:
    """Test that config_setting() still works (uses strict=False default)."""
    await buck.audit("providers", "root//:config_setting_example")
