import json

from xplat.build_infra.buck_e2e.api.buck import Buck
from xplat.build_infra.buck_e2e.buck_workspace import buck_test


@buck_test(inplace=True)
async def test_configuration_transition_rule_cquery(buck: Buck) -> None:
    # For the reference, cquery output is: P467297091. Note the "forward" node.
    result = await buck.cquery(
        "deps(fbcode//buck2/tests/targets/configurations/transition/rule:the-test)"
    )
    result.check_returncode()
    # Watchos resource should be present twice: as forward and as transitioned.
    assert result.stdout.count(":watchos-resource") == 2
    # No transition for default resource, so it appears once in cquery output.
    assert result.stdout.count(":default-resource") == 1


@buck_test(inplace=True)
async def test_configuration_transition_rule_cquery_actual_attr(buck: Buck) -> None:
    result = await buck.cquery(
        "--target-platforms=fbcode//buck2/tests/targets/configurations/transition/rule:iphoneos-p",
        "fbcode//buck2/tests/targets/configurations/transition/rule:watchos-resource",
        "--output-attributes=actual",
    )
    result.check_returncode()
    q = json.loads(result.stdout)
    [t] = q.values()
    assert "transitioned-to-watch" in t["actual"]


@buck_test(inplace=True)
async def test_configuration_transition_rule_build(buck: Buck) -> None:
    # Rule implementations do the assertions.
    result = await buck.build(
        "fbcode//buck2/tests/targets/configurations/transition/rule:the-test"
    )
    result.check_returncode()
