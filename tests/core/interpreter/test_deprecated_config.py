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
@pytest.mark.parametrize("section", ["some", "other"])
@pytest.mark.parametrize("root", ["true", "false"])
async def test_deprecated_config(buck: Buck, section: str, root: str) -> None:
    _ = await expect_failure(
        buck.build(
            f":test_target_{section}_config1",
            "-c",
            f"test.section={section}",
            "-c",
            "test.conf=config1",
            "-c",
            f"test.root={root}",
        ),
        stderr_regex=f"{section}.config1 is no longer used. Please use other.config2",
    )


@buck_test()
@pytest.mark.parametrize("root", ["true", "false"])
async def test_not_deprecated_config(buck: Buck, root: str) -> None:
    section = "other"
    _ = await buck.build(
        f":test_target_{section}_config2",
        "-c",
        f"test.section={section}",
        "-c",
        "test.conf=config2",
        "-c",
        f"test.root={root}",
    )


@buck_test()
async def test_deprecated_cell_config(buck: Buck) -> None:
    section = "other"
    _ = await expect_failure(
        buck.build(
            f"cell//:test_target_{section}_config1",
            "-c",
            f"test.section={section}",
            "-c",
            "test.conf=config1",
        ),
        stderr_regex=f"{section}.config1 is no longer used. Please use other.config2",
    )


@buck_test()
async def test_deprecated_cell_config2(buck: Buck) -> None:
    section = "other"
    _ = await expect_failure(
        buck.build(
            f"cell//:test_target_{section}_config2",
            "-c",
            f"test.section={section}",
            "-c",
            "test.conf=config2",
        ),
        stderr_regex=f"{section}.config2 is no longer used. Please use other.config3",
    )
