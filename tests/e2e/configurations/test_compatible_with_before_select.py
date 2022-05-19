# Test compatible_with is evaluated before select,
# and if target is incompatible, select should not be evaluated at all.

from xplat.build_infra.buck_e2e.api.buck import Buck
from xplat.build_infra.buck_e2e.buck_workspace import buck_test


@buck_test(inplace=True)
async def test_compatible_with_before_select(buck: Buck) -> None:
    result = await buck.cquery(
        "--target-platforms=fbcode//buck2/tests/targets/configurations/compatible_with_before_select:p-linux",
        "fbcode//buck2/tests/targets/configurations/compatible_with_before_select:",
    )
    # Check it does not fail.
    result.check_returncode()
