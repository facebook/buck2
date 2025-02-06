# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

# pyre-strict


import hashlib
import typing
from pathlib import Path
from typing import Any, Dict, List

import pytest

from buck2.tests.e2e_util.api.buck import Buck
from buck2.tests.e2e_util.api.buck_result import BuckException
from buck2.tests.e2e_util.asserts import expect_failure
from buck2.tests.e2e_util.buck_workspace import buck_test, env
from buck2.tests.e2e_util.helper.utils import (
    expect_exec_count,
    filter_events,
    random_string,
    read_invocation_record,
    read_what_ran,
)

# Taken from data.proto
ACTION_EXECUTION_KIND_LOCAL = 1
ACTION_EXECUTION_KIND_ACTION_CACHE = 3
ACTION_EXECUTION_KIND_SIMPLE = 4
ACTION_EXECUTION_KIND_LOCAL_DEP_FILE = 7
ACTION_EXECUTION_KIND_REMOTE_DEP_FILE_CACHE = 9
ACTION_EXECUTION_KIND_LOCAL_ACTION_CACHE = 10

CACHE_UPLOAD_REASON_LOCAL_EXECUTION = 0
CACHE_UPLOAD_REASON_DEP_FILE = 1


async def check_execution_kind(
    buck: Buck,
    expecteds: typing.List[int],
    ignored: typing.Optional[typing.List[int]] = None,
) -> None:
    ignored = ignored or []
    execution_kinds = await filter_events(
        buck,
        "Event",
        "data",
        "SpanEnd",
        "data",
        "ActionExecution",
        "execution_kind",
    )
    execution_kinds = [kind for kind in execution_kinds if kind not in ignored]
    assert len(execution_kinds) == len(expecteds)
    for actual, expected in zip(execution_kinds, expecteds):
        assert actual == expected


async def check_match_dep_files(
    buck: Buck,
    expected: typing.List[
        typing.Tuple[bool, bool]  # (checking_filtered_inputs, remote_cache)
    ],
) -> None:
    match_dep_files = await filter_events(
        buck, "Event", "data", "SpanStart", "data", "MatchDepFiles"
    )
    assert len(match_dep_files) == len(expected)

    for match, (checking_filtered_inputs, remote_cache) in zip(
        match_dep_files, expected
    ):
        assert bool(match["remote_cache"]) == remote_cache
        assert bool(match["checking_filtered_inputs"]) == checking_filtered_inputs


async def _get_execution_kind(buck: Buck) -> int:
    execution_kinds = await filter_events(
        buck,
        "Event",
        "data",
        "SpanEnd",
        "data",
        "ActionExecution",
        "execution_kind",
    )
    return execution_kinds[0]


def touch(buck: Buck, name: str) -> None:
    """
    Append a random string to the marker in the file
    """
    with open(buck.cwd / name, "r", encoding="utf-8") as f:
        text = f.read()

    with open(buck.cwd / name, "w", encoding="utf-8") as f:
        f.write(text.replace("__MARKER__", "__MARKER__{}".format(random_string())))


# Flaky because of watchman on mac (and maybe windows)
# Skipping on windows due to gcc dependency
@buck_test(data_dir="dep_files", skip_for_os=["darwin", "windows"])
async def test_dep_files(buck: Buck) -> None:
    # We query cache before we query dep file. Disable remote cache to make
    # sure that for the last build what-ran doesn't return cached entry.
    args = ["app:app", "--no-remote-cache"]
    await buck.build(*args)
    await expect_exec_count(buck, 1)

    touch(buck, "app/app.h")
    await buck.build(*args)
    await expect_exec_count(buck, 1)

    touch(buck, "app/app.c")
    await buck.build(*args)
    await expect_exec_count(buck, 1)

    # //app:app doesn't use other.h and
    # using dep file this should build nothing.
    touch(buck, "app/other.h")
    await buck.build(*args)
    await expect_exec_count(buck, 0)
    await check_execution_kind(
        buck,
        [ACTION_EXECUTION_KIND_LOCAL_DEP_FILE],
        # A symlinked_dir command was re-run because app/other.h was changed
        ignored=[ACTION_EXECUTION_KIND_SIMPLE],
    )


async def get_cache_queries(buck: Buck) -> List[Dict[str, Any]]:
    return await filter_events(
        buck,
        "Event",
        "data",
        "SpanStart",
        "data",
        "ExecutorStage",
        "stage",
        "CacheQuery",
    )


async def check_no_cache_query(buck: Buck) -> None:
    cache_queries = await get_cache_queries(buck)
    assert len(cache_queries) == 0


async def check_cache_query(buck: Buck) -> None:
    cache_queries = await get_cache_queries(buck)
    assert len(cache_queries) == 1


# Skipping on windows due to gcc dependency
@buck_test(
    # test uses symlinks that mess up with eden symlink redirection on MacOS
    setup_eden=False,
    data_dir="dep_files",
    skip_for_os=["windows"],
)
async def test_dep_file_hit_identical_action(buck: Buck) -> None:
    # For actions that have dep files, buck will query the local dep file cache to see
    # if an identical action is stored there. Otherwise, it will fall back to an action cache
    # look up (if enabled) and then to the full dep file query.
    # This test builds a target to build up a dep file cache, then builds the target again
    # with a no-op configuration change so that we hit the initial dep file lookup hit case.
    dummy1 = "dummy1"
    await buck.build(
        "app:app_with_dummy_config",
        "--local-only",
        "--no-remote-cache",  # Turn off remote cache query so we execute locally
        "-c",
        f"test.dummy_config={dummy1}",
    )
    await check_execution_kind(
        buck, [ACTION_EXECUTION_KIND_SIMPLE, ACTION_EXECUTION_KIND_LOCAL]
    )

    dummy2 = "dummy2"
    await buck.build(
        "app:app_with_dummy_config",
        "--local-only",
        "-c",
        f"test.dummy_config={dummy2}",
    )
    # The result should be served by the local dep file cache BEFORE an action cache lookup
    await check_no_cache_query(buck)
    # Ignoring any simple actions because there can be either one or two symlink dir actions,
    # with the same dice key,
    # Not sure why but this feels like a DICE bug triggered by the buckconfig change.
    await check_execution_kind(
        buck,
        [ACTION_EXECUTION_KIND_LOCAL_ACTION_CACHE],
        ignored=[ACTION_EXECUTION_KIND_SIMPLE],
    )
    # The MatchDepFilesStart span should indicate we only checked the depfile cache once
    await check_match_dep_files(buck, [(False, False)])


# Flaky because of watchman on mac (and maybe windows)
# Skipping on windows due to gcc dependency
# This test tombstones the hash of the dep file produced by this action.
@buck_test(data_dir="dep_files", skip_for_os=["darwin", "windows"])
@env(
    "BUCK2_TEST_TOMBSTONED_DIGESTS",
    "ed34019d42934db589d9678e6e2d0cdff739e7e2:78",
)
async def test_dep_files_ignore_missing_digests(buck: Buck, tmp_path: Path) -> None:
    await buck.build("app:app")
    await expect_exec_count(buck, 1)

    with pytest.raises(BuckException):  # noqa B908
        dep_file_path = tmp_path / "dep_file"
        await buck.build("app:app[dep_file]", f"--out={dep_file_path}")

        # If we get here, that means materialization did not fail.
        with open(dep_file_path, "rb") as f:
            dep_file = f.read()
            dep_file_hash = hashlib.sha1(dep_file).hexdigest()
            dep_file_len = len(dep_file)
            raise Exception(
                f"Misconfigured test, BUCK2_TEST_TOMBSTONED_DIGESTS to {dep_file_hash}:{dep_file_len}",
            )

    touch(buck, "app/other.h")
    await buck.build("app:app")

    await expect_exec_count(buck, 1)


@buck_test(data_dir="invalid_dep_files")
async def test_invalid_dep_files(buck: Buck) -> None:
    await buck.build(
        "//:lazy",
    )
    # Disable remote cache lookup so we actually check for local dep files
    await expect_failure(
        buck.build(
            "//:lazy",
            "-c",
            "test.seed=123",
            "--no-remote-cache",
        ),
        stderr_regex="Invalid line encountered in dep file",
    )

    await buck.debug("flush-dep-files")
    await buck.build("//:lazy")

    # Disable remote cache lookup so we actually check for local dep files
    await expect_failure(
        buck.build(
            "//:eager",
            "--eager-dep-files",
            "--no-remote-cache",
        ),
        stderr_regex="Invalid line encountered in dep file",
    )


@buck_test(data_dir="mismatched_outputs_dep_files")
async def test_mismatched_outputs_dep_files(buck: Buck) -> None:
    await buck.build("//:test", "-c", "test.prefix=foo", "-c", "test.suffix=bar")
    # Different output now, even though the command has not changed.
    await buck.build("//:test", "-c", "test.prefix=foo/bar", "-c", "test.suffix=")


async def _dep_file_uploads(buck: Buck) -> List[Dict[str, Any]]:
    return await filter_events(
        buck, "Event", "data", "SpanEnd", "data", "DepFileUpload"
    )


async def _action_executions(buck: Buck) -> List[Dict[str, Any]]:
    return await filter_events(
        buck, "Event", "data", "SpanEnd", "data", "ActionExecution"
    )


async def _dep_file_key_from_executions(buck: Buck) -> str:
    execs = await _action_executions(buck)
    assert len(execs) == 1
    return execs[0]["dep_file_key"]


async def _check_uploaded_dep_file_key(buck: Buck, dep_file_key: str) -> None:
    # BUCK2_TEST_SKIP_ACTION_CACHE_WRITE causes action result writes for dep files to always pass.
    # This is to allow testing without action cache write permission.
    dep_file_uploads = [
        upload for upload in await _dep_file_uploads(buck) if upload["success"]
    ]
    assert len(dep_file_uploads) == 1
    uploaded_key = dep_file_uploads[0]["remote_dep_file_key"]
    assert dep_file_key == uploaded_key


@buck_test(data_dir="upload_dep_files")
@env("BUCK_LOG", "buck2_execute_impl::executors::caching=debug")
@env("BUCK2_TEST_SKIP_ACTION_CACHE_WRITE", "true")
async def test_re_dep_file_uploads_same_key(buck: Buck) -> None:
    # Test all the cases where the remote dep file key should stay the same
    target = "root//:dep_files1"
    tagged_used_file1 = buck.cwd / "used.1"  # Used for depfile 0
    tagged_used_file3 = buck.cwd / "used.3"  # Used for depfile 1
    assert tagged_used_file1.exists()
    assert tagged_used_file3.exists()

    target = [
        target,
        "-c",
        "test.allow_dep_file_cache_upload=true",
        "-c",
        f"test.cache_buster={random_string()}",
        "--local-only",
    ]

    # Check that building this target results in a dep file cache upload
    await buck.build(*target)

    key = await _dep_file_key_from_executions(buck)
    await _check_uploaded_dep_file_key(buck, key)

    # Changing a tagged (associated with a dep file) input should not change the key
    # The remote dep file key only tracks the untagged inputs. The dep file cache is for checking whether
    # the output is the same despite a tagged file changing.
    tagged_used_file1.write_text("CHANGE")
    tagged_used_file3.write_text("CHANGE")
    await buck.build(*target)
    key_tagged_input_change = await _dep_file_key_from_executions(buck)
    await _check_uploaded_dep_file_key(buck, key_tagged_input_change)
    assert key == key_tagged_input_change


@buck_test(data_dir="upload_dep_files")
@env("BUCK_LOG", "buck2_execute_impl::executors::caching=debug")
@env("BUCK2_TEST_SKIP_ACTION_CACHE_WRITE", "true")
async def test_re_dep_file_uploads_different_key(buck: Buck) -> None:
    # TODO: Mergebase is currently not set in this test.
    # Include it so we can test for the case where the mergebase differs

    keys_seen = []
    target = "root//:dep_files1"
    untagged_file1 = buck.cwd / "untagged.1"
    assert untagged_file1.exists()
    targets_file = buck.cwd / "TARGETS.fixture"
    assert targets_file.exists()

    target = [
        target,
        "-c",
        "test.allow_dep_file_cache_upload=true",
        "-c",
        f"test.cache_buster={random_string()}",
        "--local-only",
    ]

    # Check that building this target results in a dep file cache upload
    await buck.build(*target)
    key = await _dep_file_key_from_executions(buck)
    await _check_uploaded_dep_file_key(buck, key)
    keys_seen.append(key)

    # Modify the depfile name and check the new key is different
    targets_file.write_text(
        targets_file.read_text().replace(
            '"dep_file_name1",', '"dep_file_name1_modified",'
        )
    )
    await buck.build(*target)

    key_different_depfile_name = await _dep_file_key_from_executions(buck)
    await _check_uploaded_dep_file_key(buck, key_different_depfile_name)
    assert key_different_depfile_name not in keys_seen
    keys_seen.append(key_different_depfile_name)

    # Modify the output name and check the new key is different
    targets_file.write_text(
        targets_file.read_text().replace('out_name = "out"', 'out_name = "out_changed"')
    )
    await buck.build(*target)
    key_different_out_name = await _dep_file_key_from_executions(buck)
    await _check_uploaded_dep_file_key(buck, key_different_out_name)
    assert key_different_out_name not in keys_seen
    keys_seen.append(key_different_out_name)

    # Modify an untagged input and check the new key is different
    untagged_file1.write_text("CHANGE")
    await buck.build(*target)
    key_untagged_input_change = await _dep_file_key_from_executions(buck)
    await _check_uploaded_dep_file_key(buck, key_untagged_input_change)
    assert key_untagged_input_change not in keys_seen
    keys_seen.append(key_untagged_input_change)


@buck_test(data_dir="upload_dep_files")
@env("BUCK_LOG", "buck2_execute_impl::executors::caching=debug")
@env("BUCK2_TEST_SKIP_ACTION_CACHE_WRITE", "true")
async def test_dep_file_does_not_upload_when_allow_cache_upload_is_true(
    buck: Buck,
) -> None:
    target = [
        "root//:dep_files1",
        "-c",
        "test.allow_dep_file_cache_upload=false",
        "-c",
        "test.allow_cache_upload=true",
        "-c",
        f"test.cache_buster={random_string()}",
        "--remote-only",
    ]

    # Check that we don't do a dep file cache upload when allow_dep_file_cache_upload is false,
    # even though allow_cache_upload is true
    await buck.build(*target)
    uploads = await _dep_file_uploads(buck)
    assert len(uploads) == 0


@buck_test(data_dir="upload_dep_files")
@env("BUCK_LOG", "buck2_execute_impl::executors::caching=debug")
@env("BUCK2_TEST_SKIP_ACTION_CACHE_WRITE", "true")
@env("BUCK2_TEST_ONLY_REMOTE_DEP_FILE_CACHE", "true")
async def test_only_do_cache_lookup_when_dep_file_upload_is_enabled(
    buck: Buck,
) -> None:
    target = [
        "root//:dep_files1",
        "-c",
        "test.allow_dep_file_cache_upload=false",
        "-c",
        "test.allow_cache_upload=true",
        "-c",
        f"test.cache_buster={random_string()}",
        "--remote-only",
    ]

    # Check that we don't do a dep file cache lookup when allow_dep_file_cache_upload is false
    await buck.build(*target)
    await check_no_cache_query(buck)

    target = [
        "root//:dep_files1",
        "-c",
        "test.allow_dep_file_cache_upload=true",
        "-c",
        "test.allow_cache_upload=true",
        "-c",
        f"test.cache_buster={random_string()}",
        "--remote-only",
    ]

    # Check that we do a dep file cache lookup when allow_dep_file_cache_upload is true
    await buck.build(*target)
    await check_cache_query(buck)


@buck_test(data_dir="upload_dep_files")
@env("BUCK_LOG", "buck2_execute_impl::executors::caching=debug")
@env("BUCK2_TEST_SKIP_ACTION_CACHE_WRITE", "true")
async def test_re_dep_file_remote_upload(buck: Buck) -> None:
    target = [
        "root//:dep_files1",
        "-c",
        "test.allow_dep_file_cache_upload=true",
        "-c",
        f"test.cache_buster={random_string()}",
        "--remote-only",
    ]

    # Check that building on RE results in a dep file cache upload
    await buck.build(*target)
    key = await _dep_file_key_from_executions(buck)
    await _check_uploaded_dep_file_key(buck, key)


@buck_test(data_dir="upload_dep_files")
@env("BUCK_LOG", "buck2_action_impl=debug,buck2_execute_impl::executors::caching=debug")
@env("BUCK2_TEST_SKIP_ACTION_CACHE_WRITE", "true")
async def test_re_dep_file_cache_hit_upload(buck: Buck, tmpdir: Path) -> None:
    target = [
        "root//:dep_files1",
        "--remote-only",
        "-c",
        # Ensure we don't get a dep file cache hit
        "test.remote_dep_file_cache_enabled=false",
    ]

    # Build on RE to make sure action cache is populated
    await buck.build(*target)
    await buck.kill()

    record = tmpdir / "record.json"
    # Check for action cache hit and dep file cache upload
    await buck.build(
        *target,
        "-c",
        "test.allow_dep_file_cache_upload=true",
        "--unstable-write-invocation-record",
        str(record),
    )
    what_ran = await read_what_ran(buck)
    assert what_ran[0]["reproducer"]["executor"] == "Cache"
    assert len(what_ran) == 1
    key = await _dep_file_key_from_executions(buck)
    await _check_uploaded_dep_file_key(buck, key)

    invocation_record = read_invocation_record(record)

    assert invocation_record["dep_file_upload_count"] == 1
    assert (
        invocation_record["dep_file_upload_count"]
        == invocation_record["dep_file_upload_attempt_count"]
    )

    # Simulate 'user' build, with action cache hit from previous build and dep file cache checking enabled.
    await buck.clean()
    await buck.build(
        "root//:dep_files1",
        "--remote-only",
        "-c",
        "test.remote_dep_file_cache_enabled=true",
        "-c",
        "test.allow_dep_file_cache_upload=false",
    )
    await check_execution_kind(buck, [ACTION_EXECUTION_KIND_ACTION_CACHE])
    uploads = await _dep_file_uploads(buck)
    # Ensure no dep file uploads are attempted for cache hits with dep file cache checking enabled, but dep file uploads disabled.
    assert len(uploads) == 0


@buck_test(data_dir="upload_dep_files")
async def test_re_dep_file_uploads_failed_action(buck: Buck) -> None:
    # If the action failed, we should not attempt to upload to cache even if it's configured to
    target = [
        "root//:dep_files_fail",
        "-c",
        "test.allow_dep_file_cache_upload=true",
    ]
    await expect_failure(
        buck.build(
            *target,
            "--no-remote-cache",
            "--local-only",
        ),
        stderr_regex="Failing on purpose",
    )
    # Assert cache upload was not attempted
    what_ran = await read_what_ran(buck, "--emit-cache-queries")
    for what in what_ran:
        assert "CacheQuery" != what["reproducer"]["executor"]


async def check_remote_dep_file_cache_query_took_place(buck: Buck) -> str:
    what_ran = await read_what_ran(buck, "--emit-cache-queries")
    assert "CacheQuery" == what_ran[0]["reproducer"]["executor"]
    return what_ran[0]["reproducer"]["details"]["digest"]


@buck_test(data_dir="upload_dep_files")
@env(
    "BUCK_LOG",
    "buck2_execute_impl::executors::caching=debug,buck2_execute_impl::executors::action_cache=debug,buck2_action_impl=debug",
)
# Disable the regular action cache query so that we actually hit the remote dep file cache query.
@env("BUCK2_TEST_ONLY_REMOTE_DEP_FILE_CACHE", "true")
async def test_re_dep_file_query_change_tagged_unused_file(buck: Buck) -> None:
    target = "root//:dep_files1"
    # Tagged for depfile0, and exists in depfile0
    tagged_used_file1 = buck.cwd / "used.1"
    # Tagged for depfile0, but does NOT exist in depfile0
    tagged_unused = buck.cwd / "unused.1"
    assert tagged_used_file1.exists()
    assert tagged_unused.exists()

    target_upload_enabled = [
        target,
        "-c",
        "test.allow_dep_file_cache_upload=true",
        "--local-only",
    ]

    target_upload_enabled_with_action_definition_change = [
        target,
        "-c",
        "test.allow_dep_file_cache_upload=true",
        "-c",
        "test.allow_cache_upload=true",
        "--local-only",
    ]

    # Build it once with cache upload (cache upload will fail locally)
    result = await buck.build(*target_upload_enabled)
    output = result.get_build_report().output_for_target(target).read_text()
    assert output == "used1\nused2\nused3\n"

    # Build the target again. This will either result in one of
    # 1. A remote dep file cache hit and a subsequent dep file validation
    # 2. A remote dep file cache miss, fall back to local execution (local dep file cache is flushed)
    await buck.debug("flush-dep-files")
    result = await buck.build(*target_upload_enabled_with_action_definition_change)
    output = result.get_build_report().output_for_target(target).read_text()
    assert output == "used1\nused2\nused3\n"

    await check_remote_dep_file_cache_query_took_place(buck)
    execution_kind = await _get_execution_kind(buck)
    was_cache_hit = "Cache hits: 100%" in result.stderr
    assert (
        was_cache_hit and execution_kind == ACTION_EXECUTION_KIND_REMOTE_DEP_FILE_CACHE
    ) or (not was_cache_hit and execution_kind == ACTION_EXECUTION_KIND_LOCAL)
    expected_dep_file_match = [
        (False, False),  # Initial local dep file cache lookup for an identical action
        (True, True),  # Remote dep file cache verification
    ]
    if execution_kind == ACTION_EXECUTION_KIND_REMOTE_DEP_FILE_CACHE:
        # Check the MatchDepFiles events
        await check_match_dep_files(buck, expected_dep_file_match)

    # Change a file that is tracked by a dep file but shows up as unused, this will again result in one of
    # 1. A remote dep file cache hit and a subsequent dep file validation
    # 2. A remote dep file cache miss, fall back to local execution (local dep file cache is flushed)
    tagged_unused.write_text("CHANGE")
    result = await buck.build(*target_upload_enabled)
    output = result.get_build_report().output_for_target(target).read_text()
    assert output == "used1\nused2\nused3\n"

    await check_remote_dep_file_cache_query_took_place(buck)
    execution_kind = await _get_execution_kind(buck)
    was_cache_hit = "Cache hits: 100%" in result.stderr
    assert (
        was_cache_hit and execution_kind == ACTION_EXECUTION_KIND_REMOTE_DEP_FILE_CACHE
    ) or (not was_cache_hit and execution_kind == ACTION_EXECUTION_KIND_LOCAL)

    if execution_kind == ACTION_EXECUTION_KIND_REMOTE_DEP_FILE_CACHE:
        # Check the MatchDepFiles events
        await check_match_dep_files(buck, expected_dep_file_match)


@buck_test(data_dir="upload_dep_files")
@env(
    "BUCK_LOG",
    "buck2_execute_impl::executors::caching=debug,buck2_execute_impl::executors::action_cache=debug,buck2_action_impl=debug",
)
# Disable the regular action cache query so that we actually hit the remote dep file cache query.
@env("BUCK2_TEST_ONLY_REMOTE_DEP_FILE_CACHE", "true")
async def test_re_dep_file_query_change_tagged_used_file(buck: Buck) -> None:
    target = "root//:dep_files2"
    # Tagged for depfile0, and exists in depfile0
    tagged_used_file1 = buck.cwd / "used.1"
    # Tagged for depfile0, but does NOT exist in depfile0
    tagged_unused = buck.cwd / "unused.1"
    assert tagged_used_file1.exists()
    assert tagged_unused.exists()

    target_upload_enabled = [
        target,
        "-c",
        "test.allow_dep_file_cache_upload=true",
        "--local-only",
    ]

    # Build it once with cache upload (cache upload will fail locally)
    result = await buck.build(*target_upload_enabled)
    output = result.get_build_report().output_for_target(target).read_text()
    assert output == "used1\nused2\nused3\n"

    # Change a file that is tracked by a dep file and shows up as used (ends up listed in the dep file).
    # Build the target again. This will either result in one of
    # 1. A remote dep file cache hit and a subsequent dep file validation (which fails)
    # 2. A remote dep file cache miss, fall back to local execution (local dep file cache is flushed)
    # Either way, it should be executed locally
    await buck.debug("flush-dep-files")
    tagged_used_file1.write_text("used1(MODIFIED)\n")
    result = await buck.build(*target_upload_enabled)
    await check_remote_dep_file_cache_query_took_place(buck)
    await check_execution_kind(buck, [ACTION_EXECUTION_KIND_LOCAL])
    output = result.get_build_report().output_for_target(target).read_text()
    assert output == "used1(MODIFIED)\nused2\nused3\n"


# Flaky because of watchman on mac (and maybe windows)
# Skipping on windows due to gcc dependency
@buck_test(data_dir="dep_files", skip_for_os=["darwin", "windows"])
async def test_flush_dep_files(buck: Buck) -> None:
    # Make sure that we build locally
    args = ["app:app", "--no-remote-cache", "--local-only"]
    await buck.build(*args)
    await expect_exec_count(buck, 1)

    await buck.debug("flush-dep-files", "--retain-local")

    # //app:app doesn't use other.h and
    # dep file should still be present
    # since we retained local dep files
    touch(buck, "app/other.h")
    await buck.build(*args)
    await expect_exec_count(buck, 0)

    await buck.debug("flush-dep-files")

    # all dep files are gone, so we have
    # to rebuild.
    touch(buck, "app/other.h")
    await buck.build(*args)
    await expect_exec_count(buck, 1)
