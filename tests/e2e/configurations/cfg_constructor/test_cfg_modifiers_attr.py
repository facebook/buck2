# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

# pyre-strict

import json

from buck2.tests.e2e.configurations.cfg_constructor.modifiers_util import get_cfg
from buck2.tests.e2e_util.api.buck import Buck
from buck2.tests.e2e_util.asserts import expect_failure
from buck2.tests.e2e_util.buck_workspace import buck_test


@buck_test(inplace=False)
async def test_cfg_modifiers_attr(buck: Buck) -> None:
    result = await buck.targets(
        "root//:test",
        "--output-attribute=modifiers",
    )

    targets = json.loads(result.stdout)
    assert len(targets) == 1
    target = targets[0]
    target_modifiers = target["modifiers"]
    assert target_modifiers == ["root//:A_1"]


@buck_test(inplace=False)
async def test_cfg_modifiers_attr_ctargets(buck: Buck) -> None:
    result = await get_cfg(
        buck,
        "root//:test2",
    )
    assert ":A_1" in result


@buck_test(inplace=False)
async def test_cfg_modifiers_attr_and_metadata_together_fails(buck: Buck) -> None:
    await expect_failure(
        buck.ctargets(
            "root//:test3",
        ),
        stderr_regex="Usage of both `modifiers` attribute and modifiers in metadata is not allowed for target `root//:test3`",
    )
