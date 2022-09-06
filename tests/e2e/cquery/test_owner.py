from xplat.build_infra.buck_e2e.api.buck import Buck
from xplat.build_infra.buck_e2e.asserts import expect_failure
from xplat.build_infra.buck_e2e.buck_workspace import buck_test


@buck_test(inplace=False, data_dir="bql/simple")
async def test_query_owner(buck: Buck) -> None:
    result = await buck.cquery("""owner(bin/TARGETS.fixture)""")
    assert result.stdout == "root//bin:the_binary (root//platforms:platform1)\n"


@buck_test(inplace=False, data_dir="bql/simple")
async def test_query_owner_with_explicit_package_boundary_violation(buck: Buck) -> None:
    result = await expect_failure(
        buck.cquery(
            """owner(package_boundary_violation/bin)""",
            "-c",
            "project.package_boundary_exceptions=",
        ),
        stderr_regex="Couldn't coerce `package_boundary_violation/bin` as a source.",
    )

    result = await buck.cquery("""owner(package_boundary_violation/bin)""")
    assert (
        "root//package_boundary_violation:bin (root//platforms:platform1)"
        in result.stdout
    )
    assert (
        "root//:package_boundary_violation (root//platforms:platform1)" in result.stdout
    )


@buck_test(inplace=True)
async def test_owner_skips_incompatible_targets(buck: Buck) -> None:
    result = await buck.cquery(
        "owner(buck2/tests/targets/configurations/cquery_owner_skip_incompatible_targets/src.txt)",
        "--target-platforms=fbcode//buck2/tests/targets/configurations/cquery_owner_skip_incompatible_targets:platform2",
    )
    assert "No owner" in result.stderr
    assert "Skipping target incompatible node" in result.stderr


@buck_test(inplace=True)
async def test_owner_without_universe_deprecated(buck: Buck) -> None:
    result = await buck.cquery(
        "--deprecated-owner",
        "owner(buck2/tests/e2e/cquery/test_owner_data/deprecated_correct/bin.sh)",
    )
    lines = result.stdout.splitlines()
    # Drop configuration.
    targets = [t.split()[0] for t in lines]
    assert [
        "fbcode//buck2/tests/e2e/cquery/test_owner_data/deprecated_correct:bin"
    ] == targets


@buck_test(inplace=True)
async def test_owner_without_universe_correct(buck: Buck) -> None:
    # TODO(nga): there should be a warning.
    result = await buck.cquery(
        "--correct-owner",
        "owner(buck2/tests/e2e/cquery/test_owner_data/deprecated_correct/bin.sh)",
    )
    assert "" == result.stdout


@buck_test(inplace=True)
async def test_owner_with_auto_universe_deprecated(buck: Buck) -> None:
    result = await buck.cquery(
        "--deprecated-owner",
        "deps(fbcode//buck2/tests/e2e/cquery/test_owner_data/deprecated_correct:test) intersect"
        " owner(buck2/tests/e2e/cquery/test_owner_data/deprecated_correct/bin.sh)",
    )
    # `owner()` returns the target outside of the universe.
    assert "" == result.stdout


@buck_test(inplace=True)
async def test_owner_with_auto_universe_correct(buck: Buck) -> None:
    result = await buck.cquery(
        "--correct-owner",
        "deps(fbcode//buck2/tests/e2e/cquery/test_owner_data/deprecated_correct:test) intersect owner(buck2/tests/e2e/cquery/test_owner_data/deprecated_correct/bin.sh)",
    )
    lines = result.stdout.splitlines()
    # Drop configuration.
    targets = [t.split()[0] for t in lines]
    assert [
        "fbcode//buck2/tests/e2e/cquery/test_owner_data/deprecated_correct:bin"
    ] == targets
