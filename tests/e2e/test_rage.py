from xplat.build_infra.buck_e2e.api.buck import Buck
from xplat.build_infra.buck_e2e.buck_workspace import buck_test


@buck_test(inplace=False, data_dir="actions")
async def test_rage(buck: Buck) -> None:
    # Build a trivial action
    await buck.build("//write:simple")

    # Run rage aginst the most recent invocation.
    await buck.rage(input=b"0")
