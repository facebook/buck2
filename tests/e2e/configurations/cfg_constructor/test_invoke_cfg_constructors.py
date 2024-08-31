# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

# pyre-strict


from buck2.tests.e2e_util.api.buck import Buck
from buck2.tests.e2e_util.buck_workspace import buck_test


@buck_test(inplace=False)
async def test_invoke_cfg_constructors(buck: Buck) -> None:
    result = await buck.cquery("root//:test")
    assert "root//:test (post_constraint_analysis_test_label" in result.stdout


@buck_test(inplace=False)
async def test_invoke_cfg_constructors_without_aliases(buck: Buck) -> None:
    # This test ensures that for backwards compatibility, we can call
    # `set_cfg_constructor` without explicitly passing in aliases parameter.
    result = await buck.cquery("root//:test", "-c", "testing.no_aliases=true")
    assert "root//:test (post_constraint_analysis_test_label" in result.stdout


@buck_test(inplace=False)
async def test_invoke_cfg_constructors_unbound_platform(buck: Buck) -> None:
    result = await buck.cquery("root//:test_unbound")
    assert "root//:test_unbound (post_constraint_analysis_test_label" in result.stdout
