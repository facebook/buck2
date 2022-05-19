from xplat.build_infra.buck_e2e.api.buck import Buck
from xplat.build_infra.buck_e2e.asserts import expect_failure
from xplat.build_infra.buck_e2e.buck_workspace import buck_test


@buck_test(inplace=False, data_dir="configurations/target_incompatible")
async def test_incompatible_target_skipping(buck: Buck) -> None:
    # incompatible target should be skipped when a package
    await buck.build("//:")
    # when explicitly requested, it should be a failure
    await expect_failure(buck.build("//:incompatible"))
    # should be a failure if it's both explicitly requested and part of a package/recursive pattern
    # TODO(cjhopman): this doesn't work correctly yet
    # await expect_failure(
    # buck.build("//:", "//:incompatible")
    # )
