from xplat.build_infra.buck_e2e.api.buck import Buck
from xplat.build_infra.buck_e2e.buck_workspace import buck_test


@buck_test(inplace=True)
async def test_dummy_to_make_this_file_not_empty_on_windows(buck: Buck) -> None:
    pass


# TODO(nga): fix and enable on Windows.
@buck_test(inplace=True, skip_if_windows=True)
async def test_build_universe(buck: Buck) -> None:
    # Run the build without universe.
    result = await buck.build(
        "fbcode//buck2/tests/e2e/build/test_build_universe_data:test"
    )
    build_report = result.get_build_report()

    output = build_report.output_for_target(
        "fbcode//buck2/tests/e2e/build/test_build_universe_data:test"
    )
    assert output.read_text().rstrip() == "default"

    # Now build the same target, but with the universe.
    result = await buck.build(
        "fbcode//buck2/tests/e2e/build/test_build_universe_data:test",
        "--target-universe",
        "fbcode//buck2/tests/e2e/build/test_build_universe_data:universe",
    )
    build_report = result.get_build_report()

    output = build_report.output_for_target(
        "fbcode//buck2/tests/e2e/build/test_build_universe_data:test"
    )
    assert output.read_text().rstrip() == "cat"
