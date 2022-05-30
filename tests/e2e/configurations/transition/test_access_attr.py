from xplat.build_infra.buck_e2e.api.buck import Buck
from xplat.build_infra.buck_e2e.buck_workspace import buck_test


@buck_test(inplace=True)
async def test_configuration_transition_access_attr(buck: Buck) -> None:
    # Trigger assertions in transition function implementation.
    await buck.cquery(
        "fbcode//buck2/tests/targets/configurations/transition/access_attr:faithful"
    )
