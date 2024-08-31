# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

# pyre-strict


import json

from buck2.tests.e2e_util.api.buck import Buck
from buck2.tests.e2e_util.buck_workspace import buck_test


@buck_test(inplace=True)
async def test_cfg_modifiers(buck: Buck) -> None:
    result = await buck.targets(
        "fbcode//buck2/tests/e2e/configurations/cfg_constructor/cfg_modifiers:test",
        "--output-attribute=metadata",
    )
    targets = json.loads(result.stdout)
    assert len(targets) == 1
    target = targets[0]
    cfg_modifiers = target["metadata"]["buck.cfg_modifiers"]
    assert cfg_modifiers == [
        "ovr_config//os/constraints:linux",
        "ovr_config//cpu/constraints:arm64",
    ]
