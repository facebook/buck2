import re

from xplat.build_infra.buck_e2e.api.buck import Buck
from xplat.build_infra.buck_e2e.buck_workspace import buck_test


@buck_test(inplace=True)
async def test_target_platforms_arg(buck: Buck) -> None:
    out = await buck.cquery(
        # Specifying platform without cell to make sure it is resolved against current cell
        "--target-platforms=//buck2/tests/targets/configurations/target_platforms_arg:p-clouds",
        "deps(fbcode//buck2/tests/targets/configurations/target_platforms_arg:the-test, 1)",
    )
    out.check_returncode()
    # TODO(nga): why we have hashes here, but not in the tests above?
    stdout = re.sub(":p-clouds-[a-f0-9]+\\)", ":p-clouds-HASH)", out.stdout)
    # TODO(nga): `p-rain` should probably not be a dependency of `the-test`
    # TODO(nga): but `p-clouds` should probably be a dependency of `the-test`
    assert (
        stdout
        == """\
fbcode//buck2/tests/targets/configurations/target_platforms_arg:the-test (fbcode//buck2/tests/targets/configurations/target_platforms_arg:p-clouds-HASH)
fbcode//buck2/tests/targets/configurations/target_platforms_arg:p-rain (fbcode//buck2/tests/targets/configurations/target_platforms_arg:p-clouds-HASH)
fbcode//buck2/tests/targets/configurations/target_platforms_arg:t-clouds (fbcode//buck2/tests/targets/configurations/target_platforms_arg:p-clouds-HASH)
"""
    )
