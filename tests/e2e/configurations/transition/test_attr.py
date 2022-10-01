from xplat.build_infra.buck_e2e.api.buck import Buck
from xplat.build_infra.buck_e2e.buck_workspace import buck_test


@buck_test(inplace=True)
async def test_configuration_transition_attr(buck: Buck) -> None:
    result = await buck.cquery(
        "deps(fbcode//buck2/tests/targets/configurations/transition/attr:the-test)"
    )
    result.check_returncode()
    # Default configuration is iphoneos and it should be transitioned to watchos
    assert ":watchos_resource" in result.stdout
    assert ":default_resource" not in result.stdout
