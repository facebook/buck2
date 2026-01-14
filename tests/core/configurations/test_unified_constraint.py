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


@buck_test(allow_soft_errors=True)
async def test_unified_constraint_defination(buck: Buck) -> None:
    await buck.bxl("//test_unified_constraint.bxl:main")


@buck_test()
async def test_unified_constraint_miss_default_fail(buck: Buck) -> None:
    await expect_failure(
        buck.targets("//miss_default:"),
        stderr_regex=".*Missing named-only parameter `default` for call to `constraint`.*",
    )


@buck_test()
async def test_unified_constraint_default_not_appear_in_value_fail(buck: Buck) -> None:
    await expect_failure(
        buck.audit("subtargets", "//default_value_not_appear:"),
        stderr_regex=r""".*default value 'linux' must be one of the declared values: \["macos", "windows"\].*""",
    )


# Test reserved keyword 'default' (lowercase) - using distinct test names because
# test discovery treats 'default' and 'DEFAULT' as the same name (case-insensitive)
@buck_test()
async def test_unified_constraint_reserved_keyword_default_lowercase_fail(
    buck: Buck,
) -> None:
    await expect_failure(
        buck.audit("subtargets", "//reserved_keyword_default_lowercase:"),
        stderr_regex=".*'default' is a reserved keyword and cannot be used as a constraint value.*",
    )


# Test reserved keyword 'DEFAULT' (uppercase)
@buck_test()
async def test_unified_constraint_reserved_keyword_default_uppercase_fail(
    buck: Buck,
) -> None:
    await expect_failure(
        buck.audit("subtargets", "//reserved_keyword_default_uppercase:"),
        stderr_regex=".*'DEFAULT' is a reserved keyword and cannot be used as a constraint value.*",
    )


@buck_test(allow_soft_errors=True)
async def test_unified_constraint_cfg_transition(buck: Buck) -> None:
    await buck.bxl("//test_unified_constraint.bxl:test_cfg_transition")


@buck_test(allow_soft_errors=True)
async def test_unified_constraint_cfg_transition_v2(buck: Buck) -> None:
    await buck.bxl("//test_unified_constraint.bxl:test_cfg_transition_v2")


@buck_test(allow_soft_errors=True)
async def test_unified_constraint_for_constraint_v2(buck: Buck) -> None:
    await buck.bxl("//test_unified_constraint.bxl:constraint_v2")
