import re

from xplat.build_infra.buck_e2e.api.buck import Buck
from xplat.build_infra.buck_e2e.buck_workspace import buck_test


@buck_test(inplace=True)
async def test_select_refine(buck: Buck) -> None:
    # Smoke test for select refinement:
    # the most specific option is picked even if it is not listed first.
    out = await buck.cquery(
        "--target-platforms=//buck2/tests/targets/configurations/select_refine:p-good-domestic",
        "deps(//buck2/tests/targets/configurations/select_refine:the-test)",
    )
    out.check_returncode()
    lines = [re.sub(" .*", "", line.strip()) for line in out.stdout.splitlines()]
    assert (
        "fbcode//buck2/tests/targets/configurations/select_refine:t-good-domestic"
        in lines
    ), lines
