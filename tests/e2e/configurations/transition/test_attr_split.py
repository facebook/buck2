from xplat.build_infra.buck_e2e.api.buck import Buck
from xplat.build_infra.buck_e2e.buck_workspace import buck_test


@buck_test(inplace=True)
async def test_configuration_transition_attr_split_cquery(buck: Buck) -> None:
    result = await buck.cquery(
        "deps(fbcode//buck2/tests/targets/configurations/transition/attr_split:bb)"
    )
    result.check_returncode()
    # Check both transitioned deps are present.
    assert "attr_split:code (arm64-" in result.stdout
    assert "attr_split:code (arm32-" in result.stdout


@buck_test(inplace=True)
async def test_configuration_transition_attr_split_build(buck: Buck) -> None:
    result = await buck.build(
        "fbcode//buck2/tests/targets/configurations/transition/attr_split:bb"
    )
    result.check_returncode()
    # Rule implementations do the assertions.
