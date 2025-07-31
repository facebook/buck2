# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

# pyre-strict

import sys
from os.path import exists
from pathlib import Path

from buck2.tests.e2e_util.api.buck import Buck
from buck2.tests.e2e_util.asserts import expect_failure
from buck2.tests.e2e_util.buck_workspace import buck_test


# Currently installer grpc doesn't compile on Mac
def linux_only() -> bool:
    return sys.platform == "linux"


if linux_only():

    @buck_test(inplace=True)
    async def test_install_modifiers(buck: Buck, tmp_path: Path) -> None:
        tmp_dir = tmp_path / "no_modifiers"
        tmp_dir.mkdir()
        args = ["--dst", f"{tmp_dir}/"]

        await buck.install(
            "fbcode//buck2/tests/targets/rules/install:installer_modifiers_test",
            "--",
            *args,
        )

        assert exists(f"{tmp_dir}/default")

        tmp_dir = tmp_path / "modifiers"
        tmp_dir.mkdir()
        args = ["--dst", f"{tmp_dir}/"]

        await buck.install(
            "fbcode//buck2/tests/targets/rules/install:installer_modifiers_test?asan",
            "--",
            *args,
        )

        assert exists(f"{tmp_dir}/asan")


@buck_test(inplace=True)
async def test_install_fails_with_global_modifiers(buck: Buck, tmp_path: Path) -> None:
    tmp_dir = tmp_path / "install_test"
    tmp_dir.mkdir()
    args = ["--dst", f"{tmp_dir}/"]
    await expect_failure(
        buck.install(
            "--modifier",
            "asan",
            "fbcode//buck2/tests/targets/rules/install:installer_modifiers_test?asan",
            "--",
            *args,
        ),
        stderr_regex=r"Cannot specify modifiers with \?modifier syntax when global CLI modifiers are set with --modifier flag",
    )
