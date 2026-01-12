# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

# pyre-strict


import os
from pathlib import Path
from typing import Any, Optional

import pytest
from buck2.tests.e2e_util.api.buck import Buck
from buck2.tests.e2e_util.api.fixtures import Fixture, Span
from buck2.tests.e2e_util.api.lsp import LSPResponseError
from buck2.tests.e2e_util.buck_workspace import buck_test


def _assert_range(range: dict[str, Any], expected: Optional[Span]) -> None:
    """Assert that this Span is equal to an LSP range dict"""
    if expected is None:
        expected = Span(0, 0, 0, 0)
    assert range["start"]["line"] == expected.start_line
    assert range["start"]["character"] == expected.start_col
    assert range["end"]["line"] == expected.end_line
    assert range["end"]["character"] == expected.end_col


def _assert_uris(actual: str, expected: str) -> None:
    if os.name == "nt":
        # Windows file paths are case-insensitive, and the LSP returns the drive identifier in upper-case.
        # Windows also allows paths to use forward and backward slashes interchangeably.
        # Normalize the paths only on Windows to avoid flakiness.
        assert actual.lower().replace("\\", "/") == expected.lower()
    else:
        assert actual == expected


def _assert_goto_result(
    res: list[dict[str, Any]],
    expected_src: Span,
    expected_dest_path: Path,
    expected_dest: Optional[Span],
) -> None:
    assert len(res) == 1
    _assert_range(res[0]["originSelectionRange"], expected_src)
    _assert_range(res[0]["targetRange"], expected_dest)
    _assert_range(res[0]["targetSelectionRange"], expected_dest)
    _assert_uris(res[0]["targetUri"], expected_dest_path.as_uri())


def fixture(buck: Buck, path: Path) -> Fixture:
    abs_path = buck.cwd / path
    fixture = Fixture(abs_path.read_text())
    abs_path.write_text(fixture.content)
    return fixture


@buck_test()
async def test_lsp_starts(buck: Buck) -> None:
    async with await buck.lsp() as lsp:
        # Will fail if the initialize response is not received
        await lsp.init_connection()


@buck_test()
async def test_lints_on_open(buck: Buck) -> None:
    async with await buck.lsp() as lsp:
        await lsp.init_connection()
        diags = await lsp.open_file(Path("clean_lint.bzl"))
        assert diags is not None
        assert len(diags["diagnostics"]) == 0

        diags = await lsp.open_file(Path("bad_syntax.bzl"))
        assert diags is not None
        assert len(diags["diagnostics"]) == 1


@buck_test()
async def test_goto_definition(buck: Buck) -> None:
    src_targets_path = Path("dir/TARGETS.fixture")
    dest_targets_path = Path("cell/sub/TARGETS.fixture")
    dest_bzl_path = Path("cell/sub/defs.bzl")

    src_targets = fixture(buck, src_targets_path)
    dest_targets = fixture(buck, dest_targets_path)
    dest_bzl = fixture(buck, dest_bzl_path)

    async with await buck.lsp() as lsp:
        await lsp.init_connection()
        diags = await lsp.open_file(src_targets_path)
        assert len(diags["diagnostics"]) == 0

        res = await lsp.goto_definition(
            src_targets_path,
            src_targets.start_line("load_click"),
            src_targets.start_col("load_click"),
        )
        _assert_goto_result(
            res, src_targets.spans["load"], buck.cwd / dest_bzl_path, None
        )

        res = await lsp.goto_definition(
            src_targets_path,
            src_targets.start_line("dummy_click"),
            src_targets.start_col("dummy_click"),
        )
        _assert_goto_result(
            res,
            src_targets.spans["dummy"],
            buck.cwd / dest_bzl_path,
            dest_bzl.spans["rule"],
        )

        res = await lsp.goto_definition(
            src_targets_path,
            src_targets.start_line("missing_click"),
            src_targets.start_col("missing_click"),
        )
        assert len(res) == 0

        res = await lsp.goto_definition(
            src_targets_path,
            src_targets.start_line("missing_foo_click"),
            src_targets.start_col("missing_foo_click"),
        )
        _assert_goto_result(
            res, src_targets.spans["missing_foo"], buck.cwd / dest_targets_path, None
        )

        res = await lsp.goto_definition(
            src_targets_path,
            src_targets.start_line("rule_click"),
            src_targets.start_col("rule_click"),
        )
        _assert_goto_result(
            res,
            src_targets.spans["rule"],
            buck.cwd / dest_bzl_path,
            dest_bzl.spans["rule"],
        )

        res = await lsp.goto_definition(
            src_targets_path,
            src_targets.start_line("baz_click"),
            src_targets.start_col("baz_click"),
        )
        _assert_goto_result(
            res,
            src_targets.spans["baz"],
            buck.cwd / dest_targets_path,
            dest_targets.spans["baz"],
        )


@buck_test()
async def test_returns_file_contents_for_starlark_types(buck: Buck) -> None:
    async with await buck.lsp() as lsp:
        await lsp.init_connection()

        res = await lsp.file_contents("starlark:/native/DefaultInfo.bzl")
        assert res["contents"] is not None

        res = await lsp.file_contents("starlark:/native/NonExistent.bzl")
        assert res["contents"] is None

        with pytest.raises(LSPResponseError):
            await lsp.file_contents(f"file:{lsp.cwd / '.buckconfig'}")


@buck_test()
async def test_goto_definition_for_globals(buck: Buck) -> None:
    globals_bzl_path = Path("globals.bzl")

    globals_bzl = fixture(buck, globals_bzl_path)
    async with await buck.lsp() as lsp:
        await lsp.init_connection()
        diags = await lsp.open_file(globals_bzl_path)
        assert len(diags["diagnostics"]) == 0

        res = await lsp.goto_definition(
            globals_bzl_path,
            globals_bzl.start_line("func2_click"),
            globals_bzl.start_col("func2_click"),
        )

        assert len(res) == 1
        _assert_range(res[0]["originSelectionRange"], globals_bzl.spans["func2"])
        assert res[0]["targetRange"]["start"]["line"] != 0
        assert res[0]["targetSelectionRange"]["start"]["line"] != 0
        _assert_uris(
            res[0]["targetUri"], (buck.cwd / "prelude" / "prelude.bzl").as_uri()
        )

        res = await lsp.goto_definition(
            globals_bzl_path,
            globals_bzl.start_line("info_click"),
            globals_bzl.start_col("info_click"),
        )

        assert len(res) == 1
        _assert_range(res[0]["originSelectionRange"], globals_bzl.spans["info"])
        _assert_uris(res[0]["targetUri"], "starlark:/native/DefaultInfo.bzl")

        res = await lsp.goto_definition(
            globals_bzl_path,
            globals_bzl.start_line("invalid_click"),
            globals_bzl.start_col("invalid_click"),
        )
        assert len(res) == 0


@buck_test()
async def test_supports_bxl_files(buck: Buck) -> None:
    src_bxl_path = Path("query.bxl")

    src_bxl = fixture(buck, src_bxl_path)

    async with await buck.lsp() as lsp:
        await lsp.init_connection()
        diags = await lsp.open_file(src_bxl_path)
        assert len(diags["diagnostics"]) == 0

        res = await lsp.goto_definition(
            src_bxl_path,
            src_bxl.start_line("foo_click"),
            src_bxl.start_col("foo_click"),
        )
        _assert_goto_result(
            res,
            src_bxl.spans["foo"],
            buck.cwd / src_bxl_path,
            src_bxl.spans["dest_foo"],
        )

        res = await lsp.goto_definition(
            src_bxl_path,
            src_bxl.start_line("f_click"),
            src_bxl.start_col("f_click"),
        )
        _assert_goto_result(
            res, src_bxl.spans["f"], buck.cwd / src_bxl_path, src_bxl.spans["dest_f"]
        )
