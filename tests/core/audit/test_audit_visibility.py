# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

# pyre-strict


import pytest
from buck2.tests.e2e_util.api.buck import Buck
from buck2.tests.e2e_util.asserts import expect_failure
from buck2.tests.e2e_util.buck_workspace import buck_test


@buck_test()
@pytest.mark.parametrize(
    "rule, passes",
    [
        ("self//:pass1", True),
        ("self//:pass2", True),
        ("self//:pass3", True),
        ("self//:pass4", True),
        ("self//:fail1", False),
        ("self//:fail2", False),
        ("self//:fail3", False),
        ("self//:fail4", False),
        ("self//:fail5", False),
        ("self//:fail6", False),
    ],
)
async def test_audit_visibility(buck: Buck, rule: str, passes: bool) -> None:
    if passes:
        out = await buck.audit_visibility(rule)
        assert out.stdout == ""
    else:
        await expect_failure(
            buck.audit_visibility(rule),
            stderr_regex=f"not visible to `{rule}`",
        )
