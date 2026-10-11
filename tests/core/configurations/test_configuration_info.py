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
async def test_configuration_info_api(buck: Buck) -> None:
    await buck.bxl("//test.bxl:configuration_info")


@buck_test()
async def test_select_resolves_constraint_setting_default(buck: Buck) -> None:
    result = await buck.cquery("root//:probe", "--output-attribute", "mode")
    assert '"mode": "dev"' in result.stdout, result.stdout


@buck_test()
async def test_transition_can_insert_constraint_value(buck: Buck) -> None:
    result = await buck.cquery(
        "deps(root//:with_transitioned_dep)", "--output-attribute", "mode"
    )
    assert '"mode": "dev"' in result.stdout, result.stdout
    assert '"mode": "opt"' in result.stdout, result.stdout
