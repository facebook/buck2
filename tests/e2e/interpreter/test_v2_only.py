from xplat.build_infra.buck_e2e.api.buck import Buck
from xplat.build_infra.buck_e2e.buck_workspace import buck_test


@buck_test(inplace=True)
async def test_v2_only(buck: Buck) -> None:
    # Just check it works.
    await buck.build("fbcode//buck2/tests/e2e/interpreter/test_v2_only_data:")
