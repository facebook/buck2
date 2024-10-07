# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

# pyre-strict

from pathlib import Path

from buck2.tests.e2e_util.api.buck import Buck
from buck2.tests.e2e_util.buck_workspace import buck_test


@buck_test()
async def test_argfile_with_cell(buck: Buck) -> None:
    res = await buck.audit_config("@cell1//argfile", "--cell", "root", "foo.bar")
    assert "bar = 1" in res.stdout


@buck_test()
async def test_argfile_from_cwd_cell(buck: Buck) -> None:
    res = await buck.audit_config(
        "@//argfile",
        "--cell",
        "root",
        "foo.bar",
        rel_cwd=Path("cell1"),
    )
    assert "bar = 1" in res.stdout


@buck_test()
async def test_executable_argfile(buck: Buck) -> None:
    res = await buck.audit_config(
        "@//exec_argfile.py#iphonesimulator-x86_64", "--cell", "root", "foo.bar"
    )
    assert "bar = 1" in res.stdout


@buck_test()
async def test_stdin_argfile(buck: Buck) -> None:
    res = await buck.audit_config(
        "@-",
        "--cell",
        "root",
        "foo.bar",
        input=str.encode("--config=foo.bar=1"),
    )
    assert "bar = 1" in res.stdout
