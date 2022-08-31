import json

from xplat.build_infra.buck_e2e.api.buck import Buck
from xplat.build_infra.buck_e2e.buck_workspace import buck_test


@buck_test(inplace=True)
async def test_target_call_stacks_default(buck: Buck) -> None:
    out = await buck.uquery(
        "--target-call-stacks",
        "fbcode//buck2/tests/e2e/uquery/test_target_call_stacks_data:test",
    )

    assert "export_file" in out.stdout


@buck_test(inplace=True)
async def test_target_call_stacks_json(buck: Buck) -> None:
    out = await buck.uquery(
        "--target-call-stacks",
        "--json",
        "fbcode//buck2/tests/e2e/uquery/test_target_call_stacks_data:test",
    )

    out = json.loads(out.stdout)
    call_stack = out[
        "fbcode//buck2/tests/e2e/uquery/test_target_call_stacks_data:test"
    ]["buck.target_call_stack"]
    assert "export_file" in call_stack
