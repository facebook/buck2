from xplat.build_infra.buck_e2e.api.buck import Buck
from xplat.build_infra.buck_e2e.asserts import expect_failure
from xplat.build_infra.buck_e2e.buck_workspace import buck_test


@buck_test(inplace=True)
async def test_configuration_transition_rule_infinite_bug(buck: Buck) -> None:
    # TODO(nga): this is a bug: query should not attempt to create an infinite graph.
    #   This command should succeed.
    #   It fails because `xx` target is transitioned,
    #   and transitioned target is transitioned again, and so on.
    result = await expect_failure(
        buck.cquery(
            "-c",
            "aaa.bbb=ccc",
            "deps(fbcode//buck2/tests/targets/configurations/transition/rule_infinite_bug:xx)",
        )
    )
    assert "did not produce identical" in result.stderr
