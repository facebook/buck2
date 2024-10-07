# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

# pyre-strict


import os
from pathlib import Path

from buck2.tests.e2e_util.api.buck import Buck
from buck2.tests.e2e_util.buck_workspace import buck_test


@buck_test()
async def test_buckconfig_works_in_external_cells(buck: Buck) -> None:
    result = await buck.audit(
        "config", "--cell", "test_bundled_cell", "user_section.key"
    )
    assert "key = value" in result.stdout


@buck_test()
async def test_uquery(buck: Buck) -> None:
    result = await buck.uquery("deps(other//:other_alias)")
    assert result.stdout.strip().split() == [
        "test_bundled_cell//dir:test_hidden",
        "test_bundled_cell//dir:test",
        "other//:other_alias",
    ]
    result = await buck.uquery(
        "deps(test_bundled_cell//dir:test)", rel_cwd=Path("other")
    )
    assert result.stdout.strip().split() == [
        "test_bundled_cell//dir:test_hidden",
        "test_bundled_cell//dir:test",
    ]


@buck_test()
async def test_build_local(buck: Buck) -> None:
    result = await buck.build_without_report(
        "--show-full-simple-output", "--local-only", "other//:other_alias"
    )
    p = Path(result.stdout.strip())
    assert p.read_text().strip() == "\n".join(["value", "6", "foobar", "foobar2"])


@buck_test()
async def test_build_remote(buck: Buck) -> None:
    result = await buck.build_without_report(
        "--show-full-simple-output", "--remote-only", "other//:other_alias"
    )
    p = Path(result.stdout.strip())
    assert p.read_text().strip() == "\n".join(["value", "6", "foobar", "foobar2"])


@buck_test()
async def test_materialize_source_directly(buck: Buck) -> None:
    result = await buck.build_without_report(
        "--show-full-simple-output", "test_bundled_cell//dir:exported"
    )
    p = Path(result.stdout.strip())
    assert f"external_cells{os.path.sep}bundled" in str(p)
    assert str(p).endswith("src.txt")
    assert p.read_text().strip() == "foobar"


@buck_test()
async def test_expand_external_cell(buck: Buck) -> None:
    await buck.expand_external_cell("test_bundled_cell")
    assert (buck.cwd / "test_bundled_cell" / ".buckconfig").exists()

    # Remove the external cell declaration
    (buck.cwd / ".buckconfig_no_external").replace(buck.cwd / ".buckconfig")
    (buck.cwd / "test_bundled_cell" / "dir" / "src.txt").write_text("foobar3\n")

    result = await buck.build_without_report(
        "--show-full-simple-output", "other//:other_alias"
    )
    p = Path(result.stdout.strip())
    assert p.read_text().strip() == "\n".join(["value", "6", "foobar3", "foobar2"])
