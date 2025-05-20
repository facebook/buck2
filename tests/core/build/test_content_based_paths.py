# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

# pyre-strict


import subprocess
from pathlib import Path
from typing import List

from buck2.tests.e2e_util.api.buck import Buck
from buck2.tests.e2e_util.asserts import expect_failure
from buck2.tests.e2e_util.buck_workspace import buck_test
from buck2.tests.e2e_util.helper.utils import read_what_ran


async def build_target_with_different_platforms_and_verify_output_paths_are_identical(
    buck: Buck,
    target: str,
    args: List[str] | None = None,
) -> None:
    if args is None:
        args = []
    result1 = await buck.build(
        target,
        "--target-platforms",
        "root//:p_default",
        "--show-output",
        *args,
    )
    result2 = await buck.build(
        target,
        "--target-platforms",
        "root//:p_cat",
        "--show-output",
        *args,
    )

    path1 = result1.get_target_to_build_output().get(target)
    path2 = result2.get_target_to_build_output().get(target)

    assert path1 is not None
    assert "output_artifact" not in path1
    assert path1 == path2


@buck_test()
async def test_write_with_content_based_path(buck: Buck) -> None:
    target = "root//:write_with_content_based_path"
    await build_target_with_different_platforms_and_verify_output_paths_are_identical(
        buck, target
    )


@buck_test()
async def test_write_macro_with_content_based_path(buck: Buck) -> None:
    target = "root//:write_macro_with_content_based_path"
    await build_target_with_different_platforms_and_verify_output_paths_are_identical(
        buck, target
    )


@buck_test()
async def test_write_json_with_content_based_path(buck: Buck) -> None:
    target = "root//:write_json_with_content_based_path"
    await build_target_with_different_platforms_and_verify_output_paths_are_identical(
        buck, target
    )


@buck_test()
async def test_run_remote_with_content_based_path(buck: Buck) -> None:
    target = "root//:run_remote_with_content_based_path"

    result1 = await buck.build(
        target,
        "--target-platforms",
        "root//:p_default",
        "--show-output",
        "--remote-only",
    )
    what_ran1 = await read_what_ran(buck)
    result2 = await buck.build(
        target,
        "--target-platforms",
        "root//:p_cat",
        "--show-output",
        "--remote-only",
    )
    what_ran2 = await read_what_ran(buck)

    assert (
        what_ran1[0]["reproducer"]["details"]["digest"]
        == what_ran2[0]["reproducer"]["details"]["digest"]
    )

    path1 = result1.get_target_to_build_output().get(target)
    path2 = result2.get_target_to_build_output().get(target)

    assert path1 is not None
    assert path1 == path2


@buck_test()
async def test_identical_dep_file_hit_with_content_based_path(buck: Buck) -> None:
    target = "root//:run_remote_with_content_based_path"

    result1 = await buck.build(target, "-c", "test.ignored_attr=run1", "--show-output")
    result2 = await buck.build(target, "-c", "test.ignored_attr=run2", "--show-output")
    what_ran2 = await read_what_ran(buck)
    assert len(what_ran2) == 0

    path1 = result1.get_target_to_build_output().get(target)
    path2 = result2.get_target_to_build_output().get(target)

    assert path1 is not None
    assert path1 == path2


@buck_test()
async def test_run_local_with_content_based_path(buck: Buck) -> None:
    target = "root//:run_local_with_content_based_path"
    await build_target_with_different_platforms_and_verify_output_paths_are_identical(
        buck, target
    )


@buck_test()
async def test_copy_with_content_based_path(buck: Buck) -> None:
    target = "root//:copy_with_content_based_path"
    await build_target_with_different_platforms_and_verify_output_paths_are_identical(
        buck, target
    )


@buck_test()
async def test_symlink_with_content_based_path(buck: Buck) -> None:
    target = "root//:symlink_with_content_based_path"
    await build_target_with_different_platforms_and_verify_output_paths_are_identical(
        buck, target
    )


@buck_test()
async def test_copied_dir_with_content_based_path(buck: Buck) -> None:
    target = "root//:copied_dir_with_content_based_path"
    await build_target_with_different_platforms_and_verify_output_paths_are_identical(
        buck, target
    )


@buck_test()
async def test_symlinked_dir_with_content_based_path(buck: Buck) -> None:
    target = "root//:symlinked_dir_with_content_based_path"
    await build_target_with_different_platforms_and_verify_output_paths_are_identical(
        buck, target
    )


@buck_test()
async def test_cas_artifact_with_content_based_path(buck: Buck) -> None:
    await build_target_with_different_platforms_and_verify_output_paths_are_identical(
        buck, "root//:empty_cas_artifact_with_content_based_path"
    )


@buck_test()
async def test_download_with_content_based_path(buck: Buck) -> None:
    await build_target_with_different_platforms_and_verify_output_paths_are_identical(
        buck, "root//:download_with_content_based_path"
    )


@buck_test()
async def test_download_with_content_based_path_and_no_metadata(buck: Buck) -> None:
    await expect_failure(
        buck.build(
            "root//:download_with_content_based_path_and_no_metadata",
        ),
        stderr_regex=r"Downloads using content-based path .* must supply metadata \(usually in the form of a sha1\)!",
    )


@buck_test()
async def test_incremental_action_with_content_based_path(buck: Buck) -> None:
    await expect_failure(
        buck.build(
            "root//:incremental_action_with_content_based_path",
        ),
        stderr_regex=r"Action is marked with no_outputs_cleanup but output `out` is content-based, which is not allowed.",
    )


def hg_init(cwd: Path) -> None:
    subprocess.run(["hg", "init"], check=True, cwd=cwd)
    hg_config_reponame(cwd)


def hg_config_reponame(cwd: Path) -> None:
    subprocess.run(
        ["hg", "config", "remotefilelog.reponame", "--local", "no-repo"],
        check=True,
        cwd=cwd,
    )


@buck_test()
async def test_offline_cas_artifact_with_content_based_path(buck: Buck) -> None:
    hg_init(cwd=buck.cwd)

    await buck.debug("trace-io", "enable")
    target = "root//:empty_cas_artifact_with_content_based_path"
    await buck.build(
        target,
        "--target-platforms",
        "root//:p_default",
        "--show-output",
    )

    await buck.debug("trace-io", "export-manifest")
    await buck.kill()

    await buck.build(
        target,
        "--target-platforms",
        "root//:p_default",
        "--show-output",
        "-c",
        "buck2.use_network_action_output_cache=true",
        "--no-remote-cache",
        "--local-only",
    )


@buck_test()
async def test_offline_download_with_content_based_path(buck: Buck) -> None:
    hg_init(cwd=buck.cwd)

    await buck.debug("trace-io", "enable")
    target = "root//:download_with_content_based_path"
    await buck.build(
        target,
        "--target-platforms",
        "root//:p_default",
        "--show-output",
    )

    await buck.debug("trace-io", "export-manifest")
    await buck.kill()

    await buck.build(
        target,
        "--target-platforms",
        "root//:p_default",
        "--show-output",
        "-c",
        "buck2.use_network_action_output_cache=true",
        "--no-remote-cache",
        "--local-only",
    )


@buck_test()
async def test_validation_with_content_based_path(buck: Buck) -> None:
    await expect_failure(
        buck.build(
            "root//:failing_validation_with_content_based_path",
            "--target-platforms",
            "root//:p_default",
            "--show-output",
        ),
        stderr_regex="This is a failing validation",
    )


@buck_test()
async def test_dynamic_with_content_based_path(buck: Buck) -> None:
    await build_target_with_different_platforms_and_verify_output_paths_are_identical(
        buck, "root//:dynamic_with_content_based_path"
    )


@buck_test()
async def test_dynamic_new_with_content_based_path(buck: Buck) -> None:
    await build_target_with_different_platforms_and_verify_output_paths_are_identical(
        buck, "root//:dynamic_new_with_content_based_path"
    )


@buck_test()
async def test_projection_with_content_based_path(buck: Buck) -> None:
    await build_target_with_different_platforms_and_verify_output_paths_are_identical(
        buck, "root//:use_projection_with_content_based_path"
    )


@buck_test()
async def test_ignores_content_based_artifact(buck: Buck) -> None:
    await expect_failure(
        buck.build(
            "root//:ignores_content_based_artifact",
            "--target-platforms",
            "root//:p_default",
            "--show-output",
        ),
        stderr_regex=r"Artifact\(s\).*ignored.txt.*cannot be used with ignore_artifacts as they are content-based",
    )


@buck_test()
async def test_local_actions_do_not_overwrite_each_other(buck: Buck) -> None:
    target1 = "root//:uses_slow_running_local_action_with_content_based_path1"
    target2 = "root//:uses_slow_running_local_action_with_content_based_path2"
    result = await buck.build(
        target1,
        target2,
        "--show-output",
    )

    path1 = result.get_target_to_build_output()[target1]
    path2 = result.get_target_to_build_output()[target2]

    path1 = path1.replace(
        "uses_slow_running_local_action_with_content_based_path1",
        "uses_slow_running_local_action_with_content_based_path2",
    )

    assert path1 != path2
