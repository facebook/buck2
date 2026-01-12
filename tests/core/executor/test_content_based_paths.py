# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

# pyre-strict


import json
import subprocess
from pathlib import Path

from buck2.tests.e2e_util.api.buck import Buck
from buck2.tests.e2e_util.api.buck_result import ExitCodeV2
from buck2.tests.e2e_util.asserts import expect_failure
from buck2.tests.e2e_util.buck_workspace import buck_test
from buck2.tests.e2e_util.helper.utils import filter_events, read_what_ran


async def is_eligible_for_action_dedup(buck: Buck) -> bool:
    events = await filter_events(
        buck,
        "Event",
        "data",
        "SpanEnd",
        "data",
        "ActionExecution",
        "eligible_for_dedupe",
    )

    assert len(events) >= 1
    return events[-1]


ELIGIBLE_FOR_DEDUPE = 0
INELIGIBLE_INPUT = 1
INELIGIBLE_OUTPUT = 2


async def build_target_with_different_platforms_and_verify_output_paths_are_identical(
    buck: Buck,
    target: str,
    args: list[str] | None = None,
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
    assert await is_eligible_for_action_dedup(buck) == ELIGIBLE_FOR_DEDUPE
    result2 = await buck.build(
        target,
        "--target-platforms",
        "root//:p_cat",
        "--show-output",
        *args,
    )
    assert await is_eligible_for_action_dedup(buck) == ELIGIBLE_FOR_DEDUPE

    path1 = result1.get_target_to_build_output().get(target)
    path2 = result2.get_target_to_build_output().get(target)

    assert path1 is not None
    assert "output_artifact" not in path1
    assert path1 != path2

    actual1 = (buck.cwd / path1).resolve()
    actual2 = (buck.cwd / path2).resolve()

    assert actual1.exists()
    assert actual2.exists()
    assert actual1 == actual2


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
    assert "output_artifact" not in path1
    assert path1 != path2

    actual1 = (buck.cwd / path1).resolve()
    actual2 = (buck.cwd / path2).resolve()

    assert actual1.exists()
    assert actual2.exists()
    assert actual1 == actual2


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
async def test_symlink_and_copy_with_content_based_path(buck: Buck) -> None:
    target = "root//:symlink_and_copy_with_content_based_path"
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
        buck,
        "root//:use_projection_with_content_based_path",
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


@buck_test()
async def test_uses_relative_to(buck: Buck) -> None:
    await build_target_with_different_platforms_and_verify_output_paths_are_identical(
        buck, "root//:uses_relative_to"
    )


@buck_test()
async def test_sets_inconsistent_params(buck: Buck) -> None:
    await expect_failure(
        buck.build(
            "root//:sets_inconsistent_params",
            "--target-platforms",
            "root//:p_default",
            "--show-output",
        ),
        stderr_regex="Artifact `out` was declared with `has_content_based_path = true`, but is now being used with `has_content_based_path = false`",
    )


@buck_test()
async def test_local_action_outputs_have_configuration_path_symlinks(
    buck: Buck,
) -> None:
    await buck.build(
        "root//:run_remote_with_dep_on_run_local",
    )
    materialized_out = await buck.log("what-materialized", "--format", "json")
    materialized = [
        json.loads(line) for line in materialized_out.stdout.splitlines() if line
    ]
    run_local_action_symlink_materialized = [
        entry
        for entry in materialized
        if "run_local_with_content_based_path" in entry["path"]
        and entry["path"].endswith("out")
        and entry["method"] == "copy"
        and entry["file_count"] == 0
    ]
    assert len(run_local_action_symlink_materialized) == 1


@buck_test()
async def test_local_action_inputs_have_configuration_path_symlinks(
    buck: Buck,
) -> None:
    await buck.build(
        "root//:run_local_with_dep_on_run_remote",
    )
    materialized_out = await buck.log("what-materialized", "--format", "json")
    materialized = [
        json.loads(line) for line in materialized_out.stdout.splitlines() if line
    ]
    run_remote_output_symlink_materialized = [
        entry
        for entry in materialized
        if "run_remote_with_content_based_path" in entry["path"]
        and entry["path"].endswith("out")
        and entry["method"] == "copy"
        and entry["file_count"] == 0
    ]
    assert len(run_remote_output_symlink_materialized) == 1


@buck_test()
async def test_output_symlink_is_updated(buck: Buck) -> None:
    target = "root//:run_remote_with_content_based_path"

    result1 = await buck.build(
        target, "-c", "test.data_string=hello world", "--show-output"
    )
    path1 = result1.get_target_to_build_output().get(target)

    actual1 = (buck.cwd / path1).resolve()
    assert actual1.exists()
    with open(actual1) as f:
        assert f.read() == "hello world"

    result2 = await buck.build(
        target, "-c", "test.data_string=goodbye world", "--show-output"
    )
    path2 = result2.get_target_to_build_output().get(target)

    assert path2 == path1

    actual2 = (buck.cwd / path2).resolve()
    assert actual2.exists()
    assert actual2 != actual1
    with open(actual2) as f:
        assert f.read() == "goodbye world"


@buck_test()
async def test_argsfile_with_incorrectly_declared_output(buck: Buck) -> None:
    target = "root//:argsfile_with_incorrectly_declared_output"
    await expect_failure(
        buck.build(target),
        stderr_regex="error: Artifact must be bound by now",
    )


@buck_test()
async def test_run_action_with_incremental_metadata(buck: Buck) -> None:
    target = "root//:incremental_action"

    await buck.build(
        target,
        "--target-platforms",
        "root//:p_default",
        "--show-output",
        "--remote-only",
    )
    what_ran1 = await read_what_ran(buck)
    await buck.build(
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


@buck_test()
async def test_resolve_promise_artifact(
    buck: Buck,
) -> None:
    await expect_failure(
        buck.build(
            "root//:resolve_promise_artifact",
            "-c",
            "test.artifact_has_content_based_path=true",
            "-c",
            "test.assert_promised_artifact_has_content_based_path=false",
        ),
        stderr_regex="Artifact promise resolved to artifact that uses content based paths. Call `actions.assert_has_content_based_path` on the promised artifact to assert that.",
    )

    await expect_failure(
        buck.build(
            "root//:resolve_promise_artifact",
            "-c",
            "test.artifact_has_content_based_path=false",
            "-c",
            "test.assert_promised_artifact_has_content_based_path=true",
        ),
        stderr_regex="Artifact promise resolved to artifact that does not use content based paths. Remove the `actions.assert_has_content_based_path` on the promised artifact.",
    )

    await buck.build(
        "root//:resolve_promise_artifact",
        "-c",
        "test.artifact_has_content_based_path=true",
        "-c",
        "test.assert_promised_artifact_has_content_based_path=true",
    )


@buck_test()
async def test_not_eligible_for_dedupe(buck: Buck) -> None:
    await buck.build(
        "root//:not_eligible_for_dedupe",
        "--target-platforms",
        "root//:p_default",
    )

    events = await filter_events(
        buck,
        "Event",
        "data",
        "SpanEnd",
        "data",
        "ActionExecution",
        "eligible_for_dedupe",
    )

    assert events == [INELIGIBLE_OUTPUT, INELIGIBLE_INPUT]


@buck_test()
async def test_expect_eligible_for_dedupe_ineligible_input(buck: Buck) -> None:
    await expect_failure(
        buck.build(
            "root//:not_eligible_for_dedupe",
            "--target-platforms",
            "root//:p_default",
            "-c",
            "test.expect_eligible_for_dedupe=true",
        ),
        stderr_regex="Action is marked with `expect_eligible_for_dedupe` but input.*script.py.*is not eligible for dedupe",
    )


@buck_test()
async def test_expect_eligible_for_dedupe_ineligible_output(buck: Buck) -> None:
    await expect_failure(
        buck.build(
            "root//:not_eligible_for_dedupe",
            "--target-platforms",
            "root//:p_default",
            "-c",
            "test.expect_eligible_for_dedupe=true",
            "-c",
            "test.run_action_output_has_content_based_path=false",
        ),
        stderr_regex="Action is marked with `expect_eligible_for_dedupe` but output `out` is not content-based",
    )


@buck_test()
async def test_failing_run_with_run_info(buck: Buck) -> None:
    await expect_failure(
        buck.build(
            "root//:failing_run_with_content_based_path",
            "--target-platforms",
            "root//:p_default",
            "--show-output",
        ),
        exit_code=ExitCodeV2.USER_ERROR,
        stderr_regex="Remote command returned non-zero exit code 1.*Tried to resolve a content-based path out without providing the content hash!",
    )
