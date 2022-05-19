from pathlib import Path

import pytest
from xplat.build_infra.buck_e2e.api.buck import Buck
from xplat.build_infra.buck_e2e.asserts import expect_failure
from xplat.build_infra.buck_e2e.buck_workspace import buck_test, data_path


@buck_test(inplace=False, data_dir="bql/simple")
async def test_simple(buck: Buck) -> None:
    await buck.query("--help")

    result = await buck.query("""print("xx")""")
    assert result.stderr == "xx\n"

    result = await buck.query("""print(deps("//bin:the_binary"))""")
    assert (
        result.stderr
        == "[root//platforms:platform1, root//:foo_toolchain, root//:bin, root//lib:file3, root//lib:lib3, root//lib:file2, root//lib:lib2, root//lib:file1, root//lib:lib1, root//:genrule_binary, root//:data, root//bin:the_binary]\n"
    )

    result = await buck.query("""print(deps(["//lib:lib1", "//:data"]))""")
    assert (
        result.stderr
        == "[root//platforms:platform1, root//lib:file1, root//lib:lib1, root//:genrule_binary, root//:data]\n"
    )


@buck_test(inplace=False, data_dir="bql/simple")
@pytest.mark.parametrize(  # type: ignore
    "entry",
    [
        entry
        for entry in sorted(Path(data_path(), "bql", "simple", "tests").iterdir())
        if entry.is_file()
    ],
    ids=lambda t: t.name,
)
async def test_bql(buck: Buck, entry: Path) -> None:
    if entry.name in ("invalid_parse.bql", "invalid_target.bql"):
        await expect_failure(buck.query(str(entry)))
    else:
        await buck.query(str(entry))
