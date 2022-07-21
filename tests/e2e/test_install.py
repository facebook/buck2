import sys
from os.path import exists

from py._path.local import LocalPath
from xplat.build_infra.buck_e2e.api.buck import Buck
from xplat.build_infra.buck_e2e.asserts import expect_failure
from xplat.build_infra.buck_e2e.buck_workspace import buck_test

# Currently installer grpc doesn't compile on Mac
def linux_only() -> bool:
    return sys.platform == "linux"


if linux_only():

    @buck_test(inplace=True)
    async def test_success_install(buck: Buck, tmpdir: LocalPath) -> None:
        tmp_dir = tmpdir.mkdir("install_test")
        args = ["--dst", f"{tmp_dir}"]
        await buck.install(
            "fbcode//buck2/tests/targets/rules/install:installer_test", "--", *args
        )
        assert exists(f"{tmp_dir}/artifact_a.txt")
        assert exists(f"{tmp_dir}/artifact_b.txt")

    @buck_test(inplace=True)
    async def test_artifact_fails_to_install(buck: Buck) -> None:
        await expect_failure(
            buck.install(
                "fbcode//buck2/tests/targets/rules/install:installer_server_sends_error"
            ),
            stderr_regex=r"Failed to send artifacts to installer",
        )

    @buck_test(inplace=True)
    async def test_fail_to_build_artifact(buck: Buck) -> None:
        await expect_failure(
            buck.install("fbcode//buck2/tests/targets/rules/install:bad_artifacts"),
            stderr_regex=r"Failed to produce artifacts",
        )

    @buck_test(inplace=True)
    async def test_install_id_mismatch(buck: Buck) -> None:
        await expect_failure(
            buck.install(
                "fbcode//buck2/tests/targets/rules/install:installer_server_sends_wrong_install_info_response"
            ),
            stderr_regex=r"doesn't match with the sent one",
        )


@buck_test(inplace=True)
async def test_fail_to_build_installer(buck: Buck) -> None:
    await expect_failure(
        buck.install("fbcode//buck2/tests/targets/rules/install:bad_installer_target"),
        stderr_regex=r"Failed to build installer",
    )
