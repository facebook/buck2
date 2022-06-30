from pathlib import Path

from xplat.build_infra.buck_e2e.api.buck import Buck
from xplat.build_infra.buck_e2e.buck_workspace import buck_test


"""
If you need to add a directory that's isolated in buck2/test/targets
(ex. some test of form @buck_test(inplace=False, data_dir=some_new_directory)),
then you will need to update isolated_targets in buck2/test/targets/TARGETS.
Otherwise the test will fail because it cannot recognize the new directory.
"""


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


# TODO: Make sure that gotodefinition works
