# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

# pyre-strict

from buck2.tests.e2e_util.api.buck import Buck
from buck2.tests.e2e_util.buck_workspace import buck_test


@buck_test()
async def test_specific_target_success(buck: Buck) -> None:
    """Test unconfigured_targets_keep_going with a specific successful target."""
    await buck.bxl(
        "//:unconfigured_targets_keep_going.bxl:test_specific_target_success",
    )


@buck_test()
async def test_recursive_pattern_success(buck: Buck) -> None:
    """Test unconfigured_targets_keep_going with a recursive pattern that includes only successful packages."""
    await buck.bxl(
        "//:unconfigured_targets_keep_going.bxl:test_recursive_pattern_success",
    )


@buck_test()
async def test_recursive_pattern_mixed(buck: Buck) -> None:
    """Test unconfigured_targets_keep_going with a recursive pattern that includes both successful and failing packages."""
    await buck.bxl(
        "//:unconfigured_targets_keep_going.bxl:test_recursive_pattern_mixed",
    )


@buck_test()
async def test_failing_package_only(buck: Buck) -> None:
    """Test unconfigured_targets_keep_going with a pattern that only matches a failing package."""
    await buck.bxl(
        "//:unconfigured_targets_keep_going.bxl:test_failing_package_only",
    )


@buck_test()
async def test_specific_target_in_failing_package(buck: Buck) -> None:
    """Test unconfigured_targets_keep_going with a specific target in a failing package."""
    await buck.bxl(
        "//:unconfigured_targets_keep_going.bxl:test_specific_target_in_failing_package",
    )
