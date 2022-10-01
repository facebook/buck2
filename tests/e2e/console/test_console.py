import os
import re

from xplat.build_infra.buck_e2e.api.buck import Buck
from xplat.build_infra.buck_e2e.buck_workspace import buck_test


def fixture(name: str) -> str:
    return os.path.join(os.environ["FIXTURES"], "fixtures", f"{name}.proto")


@buck_test(inplace=True)
async def test_console_facts(buck: Buck) -> None:
    res = await buck.debug(
        "replay", "--path", fixture("my_genrule0"), "--", "build", "--console", "simple"
    )
    assert re.search("RE: .*([0-9.]+)([KMG]?) B", res.stderr) is not None
    assert "Cache hits: 100%" in res.stderr
    assert "Commands: 1 (cached: 1, remote: 0, local: 0)" in res.stderr


@buck_test(inplace=True)
async def test_super_console_facts(buck: Buck) -> None:
    res = await buck.debug(
        "replay", "--path", fixture("my_genrule0"), "--", "build", "--console", "super"
    )
    assert re.search("RE: .*([0-9.]+)([KMG]?) B", res.stderr) is not None
    assert "Cache hits: 100%" in res.stderr
    assert "Commands: 1" in res.stderr


@buck_test(inplace=True)
async def test_whatran(buck: Buck) -> None:
    res = await buck.log(
        "what-ran",
        "--path",
        fixture("my_genrule0"),
    )
    assert "cache" in res.stdout
    assert "4d538f9cdd06a7c7ab46b418799d02ccb60a7390:93" in res.stdout
