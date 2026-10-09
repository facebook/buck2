# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

# pyre-strict

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
    assert (
        _replace_hash(result.stdout)
        == "root//bin:the_binary (root//platforms:platform1#<HASH>)\n"
    )


@buck_test(data_dir="simple")
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

    result = await buck.cquery(
        "--target-universe=root//bin:the_binary",
        "owner(bin/TARGETS.fixture)",
        rel_cwd=Path("bin"),
    )
    assert result.process.returncode == 0
    assert result.stdout == ""


@buck_test(data_dir="simple")
async def test_cquery_owner_missing_file_without_explicit_universe(buck: Buck) -> None:
    for query in (
        "owner(bin/missing.file)",
        "deps(root//bin:the_binary) intersect owner(bin/missing.file)",
    ):
        result = await buck.cquery(query)
        assert result.process.returncode == 0
        assert result.stdout == ""
