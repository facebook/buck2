# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.


from buck2.tests.e2e.configurations.cfg_constructor.modifiers_util import get_cfg
from buck2.tests.e2e_util.api.buck import Buck
from buck2.tests.e2e_util.asserts import expect_failure
from buck2.tests.e2e_util.buck_workspace import buck_test

DATA_DIR = (
    "fbcode//buck2/tests/e2e/configurations/cfg_constructor/test_cli_modifiers_data"
)
TARGET = f"{DATA_DIR}:test_target"
CONSTRAINT_A = f"{DATA_DIR}:A_1"
CONSTRAINT_B = f"{DATA_DIR}:B_1"
BUCK_TEST_MARKER = buck_test(
    inplace=True,
    extra_buck_config={
        # CLI modifier validation is disabled for users and enabled for CI. To make sure this test case always has CLI modifier validation enabled,
        # explicitly enable it here.
        "buck2": {"skip_cli_modifier_validation_DO_NOT_SET_TO_TRUE_ON_CI": ""}
    },
)


@BUCK_TEST_MARKER
async def test_one_cli_modifier(buck: Buck) -> None:
    # -m A
    assert CONSTRAINT_A in await get_cfg(buck, TARGET, "--modifier", CONSTRAINT_A)


@BUCK_TEST_MARKER
async def test_two_cli_modifier(buck: Buck) -> None:
    # -m A,B
    result = await get_cfg(
        buck, TARGET, "--modifier", CONSTRAINT_A, "--modifier", CONSTRAINT_B
    )
    assert CONSTRAINT_A in result
    assert CONSTRAINT_B in result


@BUCK_TEST_MARKER
async def test_cli_modifiers_bad_input(buck: Buck) -> None:
    # -m A B (error)
    await expect_failure(
        buck.cquery(f"deps({TARGET})", "--modifier", CONSTRAINT_A, CONSTRAINT_B),
        stderr_regex=f"got args `{CONSTRAINT_B}`",
    )


@BUCK_TEST_MARKER
async def test_cli_modifier_alias(buck: Buck) -> None:
    assert "ovr_config//os/constraints:linux" in await get_cfg(
        buck, TARGET, "--modifier", "linux"
    )
