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
import time
from pathlib import Path
from typing import Any

from buck2.tests.e2e_util.api.buck import Buck
from buck2.tests.e2e_util.api.buck_result import ExitCodeV2
from buck2.tests.e2e_util.asserts import expect_failure
from buck2.tests.e2e_util.buck_workspace import buck_test, BuckTestCase
from buck2.tests.e2e_util.helper.http_archive import make_archive
from buck2.tests.e2e_util.helper.http_server import download_configs, StaticHttpServer
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
EXECUTION_PLATFORM_UNKNOWN_ELIGIBILITY = 3

EXPECTED_INELIGIBLE_FOR_DEDUPE = 1
UNKNOWN_ELIGIBILITY = 2


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
    assert path2 is not None
    assert "output_artifact" not in path1
    assert path1 != path2

    actual1 = (buck.cwd / path1).resolve()
    actual2 = (buck.cwd / path2).resolve()

    assert actual1.exists()
    assert actual2.exists()
    assert actual1 == actual2


def hg_init(cwd: Path) -> None:
    subprocess.run(["hg", "init"], check=True, cwd=cwd)
    hg_config_reponame(cwd)


def hg_config_reponame(cwd: Path) -> None:
    subprocess.run(
        ["hg", "config", "remotefilelog.reponame", "--local", "no-repo"],
        check=True,
        cwd=cwd,
    )


async def _build_output_text(buck: Buck, target: str, *args: str) -> str:
    result = await buck.build(target, *args, "--show-output", "--remote-only")
    path = result.get_target_to_build_output().get(target)
    assert path is not None
    return (buck.cwd / path).read_text()


async def _what_ran_test_run(buck: Buck) -> list[dict[str, Any]]:
    return [e for e in await read_what_ran(buck) if "(test_run)" in e["identity"]]


class ContentBasedPathsTest(BuckTestCase):
    @buck_test()
    async def test_write_with_content_based_path(self, buck: Buck) -> None:
        target = "root//:write_with_content_based_path"
        await (
            build_target_with_different_platforms_and_verify_output_paths_are_identical(
                buck, target
            )
        )

    @buck_test()
    async def test_write_macro_with_content_based_path(self, buck: Buck) -> None:
        target = "root//:write_macro_with_content_based_path"
        await (
            build_target_with_different_platforms_and_verify_output_paths_are_identical(
                buck, target
            )
        )

    @buck_test()
    async def test_write_json_with_content_based_path(self, buck: Buck) -> None:
        target = "root//:write_json_with_content_based_path"
        await (
            build_target_with_different_platforms_and_verify_output_paths_are_identical(
                buck, target
            )
        )

    @buck_test()
    async def test_run_remote_with_content_based_path(self, buck: Buck) -> None:
        target = "root//:run_remote_with_content_based_path"

        result1 = await buck.build(
            target,
            "--target-platforms",
            "root//:p_default",
            "--show-output",
            "--remote-only",
        )
        what_ran1 = await read_what_ran(buck)
        # Flush the local dep file cache so the second (cross-configuration) build still goes through the
        # RE path and shows up in what-ran. Otherwise it would be served by the cross-configuration local
        # action cache (this action is dedupe-eligible), and what-ran would be empty. The cross-config
        # local cache hit itself is covered by test_dep_files.py::test_dep_file_hit_across_configurations.
        await buck.debug("flush-dep-files")
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
        assert path2 is not None
        assert "output_artifact" not in path1
        assert path1 != path2

        actual1 = (buck.cwd / path1).resolve()
        actual2 = (buck.cwd / path2).resolve()

        assert actual1.exists()
        assert actual2.exists()
        assert actual1 == actual2

    @buck_test()
    async def test_identical_dep_file_hit_with_content_based_path(
        self, buck: Buck
    ) -> None:
        target = "root//:run_remote_with_content_based_path"

        result1 = await buck.build(
            target, "-c", "test.ignored_attr=run1", "--show-output"
        )
        result2 = await buck.build(
            target, "-c", "test.ignored_attr=run2", "--show-output"
        )
        what_ran2 = await read_what_ran(buck)
        assert len(what_ran2) == 0

        path1 = result1.get_target_to_build_output().get(target)
        path2 = result2.get_target_to_build_output().get(target)

        assert path1 is not None
        assert path1 == path2

    @buck_test()
    async def test_run_local_with_content_based_path(self, buck: Buck) -> None:
        target = "root//:run_local_with_content_based_path"
        await (
            build_target_with_different_platforms_and_verify_output_paths_are_identical(
                buck, target
            )
        )

    @buck_test()
    async def test_copy_with_content_based_path(self, buck: Buck) -> None:
        target = "root//:copy_with_content_based_path"
        await (
            build_target_with_different_platforms_and_verify_output_paths_are_identical(
                buck, target
            )
        )

    @buck_test()
    async def test_symlink_with_content_based_path(self, buck: Buck) -> None:
        target = "root//:symlink_with_content_based_path"
        await (
            build_target_with_different_platforms_and_verify_output_paths_are_identical(
                buck, target
            )
        )

    @buck_test()
    async def test_symlink_and_copy_with_content_based_path(self, buck: Buck) -> None:
        target = "root//:symlink_and_copy_with_content_based_path"
        await (
            build_target_with_different_platforms_and_verify_output_paths_are_identical(
                buck, target
            )
        )

    @buck_test()
    async def test_copied_dir_with_content_based_path(self, buck: Buck) -> None:
        target = "root//:copied_dir_with_content_based_path"
        await (
            build_target_with_different_platforms_and_verify_output_paths_are_identical(
                buck, target
            )
        )

    @buck_test()
    async def test_symlinked_dir_with_content_based_path(self, buck: Buck) -> None:
        target = "root//:symlinked_dir_with_content_based_path"
        await (
            build_target_with_different_platforms_and_verify_output_paths_are_identical(
                buck, target
            )
        )

    @buck_test()
    async def test_assembled_dir_with_content_based_path(self, buck: Buck) -> None:
        target = "root//:assembled_dir_with_content_based_path"
        await (
            build_target_with_different_platforms_and_verify_output_paths_are_identical(
                buck, target
            )
        )

        # Entry modes must be honored: `assembled_dir.copy` entries are laid out
        # as real bytes, `assembled_dir.symlink` entries as symlinks.
        result = await buck.build(
            target,
            "--target-platforms",
            "root//:p_default",
            "--show-output",
        )
        path = result.get_target_to_build_output().get(target)
        assert path is not None
        out = buck.cwd / path
        assert out.is_dir()
        for name in ["copied", "copied_dep"]:
            assert (out / name).is_file()
            assert not (out / name).is_symlink()
        for name in ["symlinked", "symlinked_dep"]:
            assert (out / name).is_symlink()

    @buck_test()
    async def test_cas_artifact_with_content_based_path(self, buck: Buck) -> None:
        await (
            build_target_with_different_platforms_and_verify_output_paths_are_identical(
                buck, "root//:empty_cas_artifact_with_content_based_path"
            )
        )

    @buck_test()
    async def test_download_with_content_based_path(self, buck: Buck) -> None:
        files = {
            name: f"{name}\n".encode() for name in ("foo2", "foo/bar2", "foo/bar/qux")
        }
        content = make_archive("tgz", files)
        async with StaticHttpServer({"/archive": content}) as server:
            config = download_configs(server.url("/archive"), content)
            await build_target_with_different_platforms_and_verify_output_paths_are_identical(
                buck, "root//:download_with_content_based_path", config
            )

    @buck_test()
    async def test_download_with_content_based_path_and_no_metadata(
        self, buck: Buck
    ) -> None:
        await expect_failure(
            buck.build(
                "root//:download_with_content_based_path_and_no_metadata",
                "-c",
                "test.url=http://localhost:1/archive",
            ),
            stderr_regex=r"Downloads using content-based path .* must supply metadata \(usually in the form of a sha1\)!",
        )

    @buck_test()
    async def test_offline_cas_artifact_with_content_based_path(
        self, buck: Buck
    ) -> None:
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
    async def test_offline_download_with_content_based_path(self, buck: Buck) -> None:
        files = {
            name: f"{name}\n".encode() for name in ("foo2", "foo/bar2", "foo/bar/qux")
        }
        content = make_archive("tgz", files)
        async with StaticHttpServer({"/archive": content}) as server:
            config = download_configs(server.url("/archive"), content)
            hg_init(cwd=buck.cwd)

            await buck.debug("trace-io", "enable")
            target = "root//:download_with_content_based_path"
            await buck.build(
                *config,
                target,
                "--target-platforms",
                "root//:p_default",
                "--show-output",
            )

            await buck.debug("trace-io", "export-manifest")
            await buck.kill()
            assert server.count("GET", "/archive") > 0

        await buck.build(
            *config,
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
    async def test_validation_with_content_based_path(self, buck: Buck) -> None:
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
    async def test_dynamic_with_content_based_path(self, buck: Buck) -> None:
        await (
            build_target_with_different_platforms_and_verify_output_paths_are_identical(
                buck, "root//:dynamic_with_content_based_path"
            )
        )

    @buck_test()
    async def test_dynamic_new_with_content_based_path(self, buck: Buck) -> None:
        await (
            build_target_with_different_platforms_and_verify_output_paths_are_identical(
                buck, "root//:dynamic_new_with_content_based_path"
            )
        )

    @buck_test()
    async def test_projection_with_content_based_path(self, buck: Buck) -> None:
        await (
            build_target_with_different_platforms_and_verify_output_paths_are_identical(
                buck,
                "root//:use_projection_with_content_based_path",
            )
        )

    @buck_test()
    async def test_ignore_artifacts_content_based_input_is_not_materialized_but_path_is_real(
        self,
        buck: Buck,
    ) -> None:
        target = "root//:ignore_artifacts_content_based_artifact"
        result = await buck.build(target, "--show-output", "--remote-only")
        path = result.get_target_to_build_output().get(target)
        assert path is not None
        # The remote action asserts the stamp artifact is absent from its input tree;
        # its output records the rendered path.
        out = (buck.cwd / path).read_text()
        assert "stamp.txt" in out
        # The rendered path embeds the real content hash, not a placeholder.
        assert "output_artifact" not in out
        assert "dep_files_placeholder" not in out

    @buck_test()
    async def test_ignore_artifacts_content_based_input_content_change_reruns_consumer(
        self,
        buck: Buck,
    ) -> None:
        target = "root//:ignore_artifacts_content_based_artifact"
        result1 = await buck.build(
            target, "-c", "test.stamp_data=one", "--show-output", "--remote-only"
        )
        path1 = result1.get_target_to_build_output().get(target)
        assert path1 is not None
        out1 = (buck.cwd / path1).read_text()
        result2 = await buck.build(
            target, "-c", "test.stamp_data=two", "--show-output", "--remote-only"
        )
        path2 = result2.get_target_to_build_output().get(target)
        assert path2 is not None
        out2 = (buck.cwd / path2).read_text()
        # The stamp is not an input, but its content reaches the command line via the
        # rendered path, so changing it must re-run the consumer.
        assert out1 != out2

    @buck_test()
    async def test_ignore_artifacts_rejects_hidden_content_based_artifact(
        self,
        buck: Buck,
    ) -> None:
        # Nothing renders a hidden artifact, so nothing could carry a content-based
        # one's content to the action: analysis rejects it rather than dropping it.
        await expect_failure(
            buck.build(
                "root//:ignore_artifacts_rejects_hidden_content_based_artifact",
                "--target-platforms",
                "root//:p_default",
            ),
            stderr_regex=r"Artifact\(s\).*stamp.txt.*content-based and `hidden` under `ignore_artifacts`",
        )

    @buck_test()
    async def test_ignore_artifacts_nested_under_hidden_is_not_a_dependency(
        self,
        buck: Buck,
    ) -> None:
        # The content-based artifact's producer fails, but nothing renders it, so it
        # is not a dependency: the consumer builds without running the producer.
        await buck.build(
            "root//:ignore_artifacts_nested_under_hidden",
            "--target-platforms",
            "root//:p_default",
        )

    @buck_test()
    async def test_ignore_artifacts_does_not_depend_on_static_path_artifacts(
        self,
        buck: Buck,
    ) -> None:
        # The referenced artifact's producer fails, but a static path needs no
        # dependency at all, so the consumer builds without running it.
        await buck.build(
            "root//:ignore_artifacts_skips_failing_static_path_dep",
            "--target-platforms",
            "root//:p_default",
        )

    @buck_test()
    async def test_salt_hidden_is_a_content_dependency(self, buck: Buck) -> None:
        target = "root//:salt_hidden_dependency"
        # The remote action asserts the hidden stamp is absent from its input tree.
        await _build_output_text(buck, target, "-c", "test.stamp_data=one")
        what_ran1 = await _what_ran_test_run(buck)
        assert len(what_ran1) == 1

        # Nothing on the command line or in the input tree mentions the stamp's
        # content; the action must still re-run, under a different digest, when it changes.
        await _build_output_text(buck, target, "-c", "test.stamp_data=two")
        what_ran2 = await _what_ran_test_run(buck)
        assert len(what_ran2) == 1
        assert (
            what_ran1[0]["reproducer"]["details"]["digest"]
            != what_ran2[0]["reproducer"]["details"]["digest"]
        )

    @buck_test()
    async def test_local_actions_do_not_overwrite_each_other(self, buck: Buck) -> None:
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
    async def test_uses_relative_to(self, buck: Buck) -> None:
        await (
            build_target_with_different_platforms_and_verify_output_paths_are_identical(
                buck, "root//:uses_relative_to"
            )
        )

    @buck_test()
    async def test_sets_inconsistent_params(self, buck: Buck) -> None:
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
    async def test_local_action_inputs_have_configuration_path_symlinks(
        self,
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
    async def test_output_symlink_is_updated(self, buck: Buck) -> None:
        target = "root//:run_remote_with_content_based_path"

        result1 = await buck.build(
            target, "-c", "test.data_string=hello world", "--show-output"
        )
        path1 = result1.get_target_to_build_output().get(target)

        assert path1 is not None
        actual1 = (buck.cwd / path1).resolve()
        assert actual1.exists()
        with open(actual1) as f:
            assert f.read() == "hello world"

        result2 = await buck.build(
            target, "-c", "test.data_string=goodbye world", "--show-output"
        )
        path2 = result2.get_target_to_build_output().get(target)

        assert path2 is not None
        assert path2 == path1

        actual2 = (buck.cwd / path2).resolve()
        assert actual2.exists()
        assert actual2 != actual1
        with open(actual2) as f:
            assert f.read() == "goodbye world"

    @buck_test()
    async def test_argsfile_with_incorrectly_declared_output(self, buck: Buck) -> None:
        target = "root//:argsfile_with_incorrectly_declared_output"
        await expect_failure(
            buck.build(target),
            stderr_regex="error: Artifact must be bound by now",
        )

    @buck_test()
    async def test_run_action_with_incremental_metadata(self, buck: Buck) -> None:
        target = "root//:incremental_action"

        await buck.build(
            target,
            "--target-platforms",
            "root//:p_default",
            "--show-output",
            "--remote-only",
        )
        what_ran1 = await read_what_ran(buck)
        # Flush the local dep file cache so the second (cross-configuration) build still goes through the
        # RE path and shows up in what-ran. Otherwise it would be served by the cross-configuration local
        # action cache (this action is dedupe-eligible), and what-ran would be empty. The cross-config
        # local cache hit itself is covered by test_dep_files.py::test_dep_file_hit_across_configurations.
        await buck.debug("flush-dep-files")
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
        self,
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
    async def test_pass_cbp_promise_artifact_to_anon_target(
        self,
        buck: Buck,
    ) -> None:
        await buck.build(
            "root//:pass_cbp_promise_to_anon_target",
        )

    @buck_test()
    async def test_run_with_anon_non_cbp_dep_eligible_for_dedupe(
        self, buck: Buck
    ) -> None:
        await buck.build(
            "root//:run_with_anon_non_cbp_dep",
            "--target-platforms",
            "root//:p_default",
        )
        assert await is_eligible_for_action_dedup(buck) == ELIGIBLE_FOR_DEDUPE

    @buck_test()
    async def test_run_with_symlink_to_non_content_based_input_eligible_for_dedupe_bug(
        self,
        buck: Buck,
    ) -> None:
        # TODO(T276504188): This test documents a BUG and asserts the buggy
        # behavior, so it must be updated once the task is fixed.
        #
        # The `run_with_symlink_to_non_cbp_input` target's consuming `run` action
        # depends on a symlink whose own path is content-based, but which points at
        # a non-content-based (configuration-based) artifact. Because it is a `run`,
        # the symlink (and therefore its target) is actually materialized as an
        # input, so the action's resolved inputs still embed a configuration hash.
        # The action is therefore NOT actually eligible for dedupe and should be
        # reported as INELIGIBLE_INPUT.
        #
        # Instead it is wrongly reported as ELIGIBLE_FOR_DEDUPE: the eligibility
        # check (`ArtifactGroup::is_eligible_for_dedupe`) only inspects each input
        # artifact's own path and never follows a symlink to its target, so it
        # misses the configuration-based target.
        #
        # When T276504188 is fixed, the consuming `run` action should instead be
        # reported as INELIGIBLE_INPUT and this assertion must be flipped.
        await buck.build(
            "root//:run_with_symlink_to_non_cbp_input",
            "--target-platforms",
            "root//:p_default",
        )

        # The consuming run action executes last (it depends on every other action
        # in the rule), so its eligibility verdict is the last event.
        assert await is_eligible_for_action_dedup(buck) == ELIGIBLE_FOR_DEDUPE

    @buck_test()
    async def test_not_eligible_for_dedupe(self, buck: Buck) -> None:
        await buck.build(
            "root//:not_eligible_for_dedupe",
            "--target-platforms",
            "root//:p_default",
            "-c",
            "test.expect_eligible_for_dedupe=false",
        )

        eligible_for_dedupe_events = await filter_events(
            buck,
            "Event",
            "data",
            "SpanEnd",
            "data",
            "ActionExecution",
            "eligible_for_dedupe",
        )

        assert eligible_for_dedupe_events == [INELIGIBLE_OUTPUT, INELIGIBLE_INPUT]

        expected_eligible_for_dedupe_events = await filter_events(
            buck,
            "Event",
            "data",
            "SpanEnd",
            "data",
            "ActionExecution",
            "expected_eligible_for_dedupe",
        )

        assert expected_eligible_for_dedupe_events == [
            UNKNOWN_ELIGIBILITY,
            EXPECTED_INELIGIBLE_FOR_DEDUPE,
        ]

    @buck_test()
    async def test_expect_eligible_for_dedupe_ineligible_input(
        self, buck: Buck
    ) -> None:
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
    async def test_expect_eligible_for_dedupe_ineligible_input_for_execution_platform(
        self,
        buck: Buck,
    ) -> None:
        buck.build(
            "root//:not_eligible_for_dedupe",
            "--target-platforms",
            "root//platforms:default",
            "-c",
            "test.expect_eligible_for_dedupe=true",
        )

    @buck_test()
    async def test_expect_eligible_for_dedupe_ineligible_output(
        self, buck: Buck
    ) -> None:
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
    async def test_execution_platform_returns_unknown_eligibility(
        self, buck: Buck
    ) -> None:
        # When an action's owner is configured for an execution platform (i.e. via
        # exec_dep), eligible_for_dedupe will return EXECUTION_PLATFORM_UNKNOWN_ELIGIBILITY
        # if an input is configured for the same platform as the action itself.
        await buck.build(
            "root//:uses_exec_dep",
            "--target-platforms",
            "root//:p_default",
        )

        eligible_for_dedupe_events = await filter_events(
            buck,
            "Event",
            "data",
            "SpanEnd",
            "data",
            "ActionExecution",
            "eligible_for_dedupe",
        )

        # The exec dep (non_content_based_exec_dep) has a single run action that hits this.
        assert (
            eligible_for_dedupe_events.count(EXECUTION_PLATFORM_UNKNOWN_ELIGIBILITY)
            == 1
        )

    @buck_test()
    async def test_failing_run_with_run_info(self, buck: Buck) -> None:
        failure = await expect_failure(
            buck.build(
                "root//:failing_run_with_content_based_path",
                "--target-platforms",
                "root//:p_default",
                "--show-output",
            ),
            exit_code=ExitCodeV2.USER_ERROR,
            stderr_regex="Remote command returned non-zero exit code 1",
        )
        assert (
            "Tried to resolve a content-based path out without providing the content hash!"
            not in failure.stderr
        )

    @buck_test()
    async def test_shared_content_hash_race(self, buck: Buck) -> None:
        """Regression test for a writer-vs-writer race on shared content-based paths.

        Mirrors the pattern from prelude/rust/build.bzl
        _create_transitive_dependency_symlinks: a write_json output with
        has_content_based_path = True whose content does not depend on
        configuration. A single producer target is analyzed under N different
        platforms (via N consumer targets with distinct default_target_platform),
        so every analysis's write_json resolves to the SAME
        __<target>__/<content_hash>/artifacts.json on-disk path. Concurrently
        running those N writes exercises the cleanup_path -> write_file
        sequence in immediate::write_to_disk; before the fix, two writers
        would race on the same path and one's remove would return ENOENT,
        failing the build with:
            Action failed: ... (write_json artifacts.json)
            remove_file(.../<target>/<content_hash>/artifacts.json):
            No such file or directory (os error 2)
        """
        N = 200
        targets = [f"root//:rust_pattern_consumer_{i}" for i in range(N)]
        # Embed a distinct seed per iteration via buckconfig so every iteration
        # writes different JSON content and therefore misses the RE action cache
        # and the local materializer cache. All N consumers within an iteration
        # share the same seed, so they share the same content-hash path.
        base_seed = int(time.time() * 1000)
        for i in range(3):
            await buck.build(
                "-c",
                f"cbp_race.seed={base_seed + i}",
                *targets,
            )
            await buck.clean()
