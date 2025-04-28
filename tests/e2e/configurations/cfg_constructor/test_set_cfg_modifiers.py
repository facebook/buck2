# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

# pyre-strict


import json

from buck2.tests.e2e_util.api.buck import Buck
from buck2.tests.e2e_util.asserts import expect_failure
from buck2.tests.e2e_util.buck_workspace import buck_test


@buck_test(inplace=True)
async def test_set_cfg_modifiers(buck: Buck) -> None:
    result = await buck.targets(
        "fbcode//buck2/tests/e2e/configurations/cfg_constructor/test_clear_package_modifiers_data/set_cfg_modifiers/dir:test",
        "--package-values",
    )
    targets = json.loads(result.stdout)
    assert len(targets) == 1
    target = targets[0]
    cfg_modifiers = target["buck.package_values"]["buck.cfg_modifiers"]
    assert cfg_modifiers == [
        {
            "_type": "TaggedModifiers",
            "location": {
                "_type": "ModifierPackageLocation",
                "package_path": "fbcode//buck2/tests/e2e/configurations/cfg_constructor/test_clear_package_modifiers_data/set_cfg_modifiers/PACKAGE",
            },
            "modifiers": [
                {
                    "_type": "ModifiersMatch",
                    "ovr_config//os/constraints:linux": "ovr_config//cpu/constraints:arm64",
                    "ovr_config//os/constraints:macos": "ovr_config//cpu/constraints:x86_64",
                },
                {
                    "DEFAULT": "ovr_config//os/constraints:linux",
                    "_type": "ModifiersMatch",
                },
            ],
            "rule_name": None,
        },
        {
            "_type": "TaggedModifiers",
            "location": {
                "_type": "ModifierPackageLocation",
                "package_path": "fbcode//buck2/tests/e2e/configurations/cfg_constructor/test_clear_package_modifiers_data/set_cfg_modifiers/PACKAGE",
            },
            "modifiers": [
                "ovr_config//cpu/constraints:x86_64",
            ],
            "rule_name": "python_binary",
        },
        {
            "_type": "TaggedModifiers",
            "location": {
                "_type": "ModifierPackageLocation",
                "package_path": "fbcode//buck2/tests/e2e/configurations/cfg_constructor/test_clear_package_modifiers_data/set_cfg_modifiers/dir/PACKAGE",
            },
            "modifiers": [
                {
                    "_type": "ModifiersMatch",
                    "ovr_config//os/constraints:windows": "ovr_config//cpu/constraints:x86_64",
                },
                "ovr_config//os/constraints:macos",
            ],
            "rule_name": None,
        },
    ]


@buck_test(inplace=True)
async def test_set_cfg_modifiers_from_package_file_only(
    buck: Buck,
) -> None:
    await expect_failure(
        buck.targets(
            "fbcode//buck2/tests/e2e/configurations/cfg_constructor/test_clear_package_modifiers_data/set_cfg_modifiers/package_file_check:test",
            "-c",
            "buck_e2e.testing_failure=true",
        ),
        stderr_regex="set_cfg_modifiers is only allowed to be used from a PACKAGE or BUCK_TREE file, not a bzl file",
    )
