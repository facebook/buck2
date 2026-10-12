# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

from __future__ import annotations

import re
from pathlib import Path

from buck2.tests.e2e_util.api.buck import Buck
from buck2.tests.e2e_util.buck_workspace import buck_test


def _replace_hash(s: str) -> str:
    return re.sub(r"\b[0-9a-f]{16}\b", "<HASH>", s)


@buck_test(data_dir="simple")
async def test_query_owner(buck: Buck) -> None:
    result = await buck.cquery(
        "--target-universe=root//bin:the_binary", """owner(bin/TARGETS.fixture)"""
    )
    assert result.process.returncode == 0
    assert (
        _replace_hash(result.stdout)
        == "root//bin:the_binary (root//platforms:platform1#<HASH>)\n"
    )


@buck_test(data_dir="simple", allow_soft_errors=True)
async def test_cquery_owner_missing_file(buck: Buck) -> None:
    for path in (
        "bin/missing.file",
        "missing/file",
        "root//bin/missing.file",
        (buck.cwd / "bin/missing.file").as_posix(),
    ):
        result = await buck.cquery(
            "--target-universe=root//bin:the_binary", f"owner('{path}')"
        )
        assert result.process.returncode == 0
        assert result.stdout == ""
        assert "owner_invalid_path" in (await buck.log("show")).stdout

    result = await buck.cquery(
        "--target-universe=root//bin:the_binary",
        "owner(bin/TARGETS.fixture)",
        rel_cwd=Path("bin"),
    )
    assert result.process.returncode == 0
    assert result.stdout == ""
    log = (await buck.log("show")).stdout
    assert "owner_invalid_path" in log
    assert "File not found: root//bin/bin/TARGETS.fixture" in log


@buck_test(data_dir="simple", allow_soft_errors=True)
async def test_cquery_owner_missing_file_without_explicit_universe(buck: Buck) -> None:
    for query in (
        "owner(bin/missing.file)",
        "deps(root//bin:the_binary) intersect owner(bin/missing.file)",
    ):
        result = await buck.cquery(query)
        assert result.process.returncode == 0
        assert result.stdout == ""
        assert "owner_invalid_path" in (await buck.log("show")).stdout


@buck_test(data_dir="simple", allow_soft_errors=True)
async def test_cquery_owner_mixed_existing_and_missing_files(buck: Buck) -> None:
    result = await buck.cquery(
        "--target-universe=root//bin:the_binary",
        "owner(fileset(bin/TARGETS.fixture missing/file))",
    )
    assert result.process.returncode == 0
    assert (
        _replace_hash(result.stdout)
        == "root//bin:the_binary (root//platforms:platform1#<HASH>)\n"
    )
    assert "owner_invalid_path" in (await buck.log("show")).stdout


@buck_test(data_dir="simple")
async def test_cquery_owner_existing_file_without_owner(buck: Buck) -> None:
    for universe, path in (
        ("root//bin:the_binary", ".buckconfig"),
        ("root//:bin", "bin/TARGETS.fixture"),
    ):
        result = await buck.cquery(f"--target-universe={universe}", f"owner('{path}')")
        assert result.process.returncode == 0
        assert result.stdout == ""
        assert "No owner" in result.stderr


@buck_test(data_dir="simple", allow_soft_errors=True)
async def test_cquery_owner_file_created(buck: Buck) -> None:
    path = buck.cwd / "bin/new.file"
    args = ("--target-universe=root//bin:the_binary", "owner(bin/new.file)")
    result = await buck.cquery(*args)
    assert result.process.returncode == 0
    assert result.stdout == ""
    assert "owner_invalid_path" in (await buck.log("show")).stdout

    path.write_text("")
    result = await buck.cquery(*args)
    assert result.process.returncode == 0
    assert result.stdout == ""
    assert "owner_invalid_path" not in (await buck.log("show")).stdout

    path.unlink()
    result = await buck.cquery(*args)
    assert result.process.returncode == 0
    assert result.stdout == ""
    assert "owner_invalid_path" in (await buck.log("show")).stdout
