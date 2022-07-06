import os

from xplat.build_infra.buck_e2e.api.buck import Buck
from xplat.build_infra.buck_e2e.buck_workspace import buck_test


def fixture(name: str) -> str:
    return os.path.join(os.environ["FIXTURES"], "fixtures", f"{name}.json-lines")


@buck_test(inplace=True)
async def test_console_facts(buck: Buck) -> None:
    res = await buck.debug(
        "replay", "--path", fixture("my_genrule0"), "--", "build", "--console", "simple"
    )
    assert "RE Session:" in res.stderr
    assert "Cache hits: 100%" in res.stderr
    assert "Commands: 1 (cached: 1, remote: 0, local: 0)" in res.stderr


@buck_test(inplace=True)
async def test_whatran(buck: Buck) -> None:
    res = await buck.log(
        "what-ran",
        "--path",
        fixture("my_genrule0"),
    )
    assert "cache" in res.stdout
    assert "c63fead395a232f021871f53d4eafb60038890f2:93" in res.stdout
