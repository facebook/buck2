from xplat.build_infra.buck_e2e.api.buck import Buck
from xplat.build_infra.buck_e2e.buck_workspace import buck_test


@buck_test(inplace=True)
async def test_configuration_rule_unbound(buck: Buck) -> None:
    result = await buck.cquery(
        # platform argument is ignored
        "--target-platforms=fbcode//buck2/tests/targets/configurations/configuration_rule_unbound:p",
        "fbcode//buck2/tests/targets/configurations/configuration_rule_unbound:the-test",
    )
    result.check_returncode()
    # Note configuration is unbound here.
    assert (
        "fbcode//buck2/tests/targets/configurations/configuration_rule_unbound:the-test (<unbound>)\n"
        == result.stdout
    )
