import json

from xplat.build_infra.buck_e2e.api.buck import Buck
from xplat.build_infra.buck_e2e.buck_workspace import buck_test


# Test select works with buckconfig.
@buck_test(inplace=True)
async def test_select_buckconfig(buck: Buck) -> None:
    out = await buck.cquery(
        "fbcode//buck2/tests/targets/configurations/select_buckconfig:the-test",
        "--output-attribute=cmd",
    )
    q = json.loads(out.stdout)
    assert len(q) == 1
    assert list(q.values())[0]["cmd"] == "NO"

    out = await buck.cquery(
        "fbcode//buck2/tests/targets/configurations/select_buckconfig:the-test",
        "--output-attribute=cmd",
        "-c",
        "aaa.bbb=ccc",
    )
    q = json.loads(out.stdout)
    assert len(q) == 1
    assert list(q.values())[0]["cmd"] == "YES"
