# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

# pyre-strict

from pathlib import Path

from buck2.tests.e2e_util.api.buck import Buck
from buck2.tests.e2e_util.asserts import expect_failure
from buck2.tests.e2e_util.buck_workspace import buck_test


@buck_test(data_dir="include_external")
async def test_include_external_file(buck: Buck) -> None:
    # Note that the repo is inside a tempdir
    (buck.cwd.parent / "extra").write_text("[abc]\ndef=x", encoding="utf-8")
    await expect_failure(
        buck.audit_config("--cell", "root"),
        stderr_regex="Improperly include directive path",
    )


@buck_test(data_dir="empty", skip_for_os=["windows"])
async def test_external_symlink_resolution(buck: Buck, tmp_path: Path) -> None:
    base = tmp_path / "base"
    (base / "b" / "bb").mkdir(parents=True)
    (base / "a").mkdir()
    (base / "a" / "aa").symlink_to("../b/bb")
    (base / "b" / "included").write_text("[sec]\nval = physical", encoding="utf-8")
    (base / "a" / "included").write_text("[sec]\nval = logical", encoding="utf-8")

    (base / "b" / "bb" / "config").write_text("<file:../included>", encoding="utf-8")

    config_via_symlink = base / "a" / "aa" / "config"

    res = await buck.audit_config(
        "--cell", "root", "--config-file", str(config_via_symlink)
    )
    assert "val = physical" in res.stdout


@buck_test(data_dir="empty")
async def test_changing_external_include(buck: Buck) -> None:
    extra = buck.cwd.parent / "extra"
    extra.write_text("[abc]\n  def = 1", encoding="utf-8")

    # Start the daemon and build once
    await buck.audit_config(
        "--all-cells", env={"BUCK2_TEST_EXTRA_EXTERNAL_CONFIG": str(extra)}
    )

    # Change the file and build again
    extra.write_text("[abc]\n    def = 2", encoding="utf-8")

    res = await buck.audit_config("--cell", "root", "abc.def")
    assert "[abc]\n    def = 2" in res.stdout
    res = await buck.audit_config("--cell", "cell", "abc.def")
    assert "[abc]\n    def = 2" in res.stdout


@buck_test(data_dir="include_through_symlink")
async def test_external_symlink_source_file(buck: Buck) -> None:
    external_dir = buck.cwd.parent / "extra"
    external_dir.mkdir()
    (buck.cwd / "repo_dir").symlink_to(external_dir)

    await buck.audit_config("--cell", "root", "abc.def")
