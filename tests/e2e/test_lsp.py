from pathlib import Path
from typing import Any, Optional

from xplat.build_infra.buck_e2e.api.buck import Buck
from xplat.build_infra.buck_e2e.api.fixtures import Fixture, Span
from xplat.build_infra.buck_e2e.buck_workspace import buck_test


"""
If you need to add a directory that's isolated in buck2/test/targets
(ex. some test of form @buck_test(inplace=False, data_dir=some_new_directory)),
then you will need to update isolated_targets in buck2/test/targets/TARGETS.
Otherwise the test will fail because it cannot recognize the new directory.
"""


def assert_range(range: Any, expected: Optional[Span]):
    """Assert that this Span is equal to an LSP range dict"""
    if expected is None:
        expected = Span(0, 0, 0, 0)
    assert range["start"]["line"] == expected.start_line
    assert range["start"]["character"] == expected.start_col
    assert range["end"]["line"] == expected.end_line
    assert range["end"]["character"] == expected.end_col


def assert_goto_result(
    res: Any,
    expected_src: Span,
    expected_dest_path: Path,
    expected_dest: Optional[Span],
):
    assert len(res) == 1
    assert_range(res[0]["originSelectionRange"], expected_src)
    assert_range(res[0]["targetRange"], expected_dest)
    assert_range(res[0]["targetSelectionRange"], expected_dest)
    assert res[0]["targetUri"] == expected_dest_path.as_uri()


def fixture(buck: Buck, path: Path) -> Fixture:
    abs_path = buck.cwd / path
    fixture = Fixture(abs_path.read_text())
    abs_path.write_text(fixture.content)
    return fixture


@buck_test(inplace=False, data_dir="lsp")
async def test_lsp_starts(buck: Buck) -> None:
    async with await buck.lsp() as lsp:
        # Will fail if the initialize response is not received
        await lsp.init_connection()


@buck_test(inplace=False, data_dir="lsp")
async def test_lints_on_open(buck: Buck) -> None:
    async with await buck.lsp() as lsp:
        await lsp.init_connection()
        diags = await lsp.open_file(Path("clean_lint.bzl"))
        assert diags is not None
        assert len(diags["diagnostics"]) == 0

        diags = await lsp.open_file(Path("bad_syntax.bzl"))
        assert diags is not None
        assert len(diags["diagnostics"]) == 1


@buck_test(inplace=False, data_dir="lsp")
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
        assert_goto_result(
            res, src_targets.spans["load"], buck.cwd / dest_bzl_path, None
        )

        res = await lsp.goto_definition(
            src_targets_path,
            src_targets.start_line("dummy_click"),
            src_targets.start_col("dummy_click"),
        )
        assert_goto_result(
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
        assert_goto_result(
            res, src_targets.spans["missing_foo"], buck.cwd / dest_targets_path, None
        )

        res = await lsp.goto_definition(
            src_targets_path,
            src_targets.start_line("rule_click"),
            src_targets.start_col("rule_click"),
        )
        assert_goto_result(
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
        assert_goto_result(
            res,
            src_targets.spans["baz"],
            buck.cwd / dest_targets_path,
            dest_targets.spans["baz"],
        )
