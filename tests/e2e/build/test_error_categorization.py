# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

# pyre-strict

from pathlib import Path

from buck2.tests.e2e_util.api.buck import Buck
from buck2.tests.e2e_util.asserts import expect_failure
from buck2.tests.e2e_util.buck_workspace import buck_test, env

from buck2.tests.e2e_util.helper.utils import (
    filter_events,
    is_running_on_linux,
    is_running_on_windows,
    read_invocation_record,
)

# From `buck2_data`
USER_ERROR = 2
ENVIRONMENT_ERROR = 3
ACTION_COMMAND_FAILURE = 2

STARLARK_FAIL_TAG = 1
ANY_STARKLARK_EVALUATION_TAG = 2001


@buck_test(inplace=False)
async def test_action_error(buck: Buck) -> None:
    await expect_failure(
        buck.build("//:action_fail"), stderr_regex="Failed to build 'root//:action_fail"
    )
    errors_events = await filter_events(
        buck,
        "Result",
        "result",
        "build_response",
        "errors",
    )
    assert len(errors_events) == 1
    errors = errors_events[0]
    assert len(errors) == 1
    assert errors[0]["tier"] == USER_ERROR
    assert errors[0]["typ"] == ACTION_COMMAND_FAILURE
    # This test is unfortunately liable to break as a result of refactorings, since this is not
    # stable. Feel free to delete it if it becomes a problem.
    assert (
        errors[0]["source_location"]
        == "buck2_build_api/src/actions/error.rs::ActionError"
    )


@buck_test(inplace=False)
async def test_missing_outputs(buck: Buck) -> None:
    # FIXME(JakobDegen): This doesn't work with non-local-only actions
    await expect_failure(
        buck.build("//:missing_outputs", "--local-only"),
        stderr_regex="Failed to build 'root//:missing_outputs",
    )
    errors_events = await filter_events(
        buck,
        "Result",
        "result",
        "build_response",
        "errors",
    )
    assert len(errors_events) == 1
    errors = errors_events[0]
    assert len(errors) == 1
    assert errors[0]["tier"] == USER_ERROR
    assert errors[0]["typ"] is None


@buck_test(inplace=False)
async def test_bad_url(buck: Buck) -> None:
    await expect_failure(
        buck.build("//:bad_url"),
        stderr_regex="Failed to build 'root//:bad_url",
    )
    errors_events = await filter_events(
        buck,
        "Result",
        "result",
        "build_response",
        "errors",
    )
    assert len(errors_events) == 1
    errors = errors_events[0]
    assert len(errors) == 1
    # Also liable to break as a result of refactorings, feel free to update
    assert (
        errors[0]["source_location"] == "buck2_http/src/lib.rs::HttpError::SendRequest"
    )


@buck_test(inplace=False)
async def test_attr_coercion(buck: Buck) -> None:
    await expect_failure(
        buck.build("//attr_coercion:int_rule"),
        stderr_regex="evaluating build file: `root//attr_coercion:TARGETS.fixture",
    )
    errors_events = await filter_events(
        buck,
        "Result",
        "result",
        "build_response",
        "errors",
    )
    assert len(errors_events) == 1
    errors = errors_events[0]
    assert len(errors) == 1
    # Just make sure there's some kind of error metadata
    assert "CoercionError::TypeError" in errors[0]["source_location"]


@buck_test(inplace=False)
async def test_buck2_fail(buck: Buck) -> None:
    await expect_failure(
        buck.build("//buck2_fail:foobar"),
        stderr_regex="evaluating build file: `root//buck2_fail:TARGETS.fixture`",
    )
    errors_events = await filter_events(
        buck,
        "Result",
        "result",
        "build_response",
        "errors",
    )
    assert len(errors_events) == 1
    errors = errors_events[0]
    assert len(errors) == 1
    # Just make sure that despite there being no context on the error, we still report the right
    # metadata
    assert (
        errors[0]["source_location"]
        == "buck2_interpreter_for_build/src/interpreter/functions/internals.rs::BuckFail"
    )


@buck_test(inplace=False)
async def test_starlark_error_categorization(buck: Buck) -> None:
    await expect_failure(
        buck.build("//starlark_fail:foobar"),
        stderr_regex="evaluating build file: `root//starlark_fail:TARGETS.fixture`",
    )
    errors_events = await filter_events(
        buck,
        "Result",
        "result",
        "build_response",
        "errors",
    )
    assert len(errors_events) == 1
    errors = errors_events[0]
    assert len(errors) == 1
    assert errors[0]["source_location"].endswith("BuckStarlarkError::Fail")
    assert errors[0]["tier"] == USER_ERROR


@buck_test(inplace=False)
async def test_targets_error_categorization(buck: Buck) -> None:
    await expect_failure(
        buck.targets("//starlark_fail:foobar"),
        stderr_regex="evaluating build file: `root//starlark_fail:TARGETS.fixture`",
    )
    errors_events = await filter_events(
        buck,
        "Result",
        "result",
        "error",
    )
    assert len(errors_events) == 1
    errors = errors_events[0]["errors"]
    assert len(errors) == 1
    assert errors[0]["tags"] == [ANY_STARKLARK_EVALUATION_TAG, STARLARK_FAIL_TAG]
    assert errors[0]["tier"] == USER_ERROR


@buck_test(inplace=False)
async def test_daemon_crash(buck: Buck, tmp_path: Path) -> None:
    await buck.build()

    record = tmp_path / "record.json"
    await buck.debug(
        "crash", "panic", "--unstable-write-invocation-record", str(record)
    )
    invocation_record = read_invocation_record(record)

    errors = invocation_record["errors"]

    assert len(errors) == 1
    [error] = errors
    if is_running_on_windows():
        assert "transport error" in error["message"]
    else:
        assert "stream closed because of a broken pipe" in error["message"]

    assert error["tags"] == ["CLIENT_GRPC", "SERVER_PANICKED"]
    assert "buckd stderr:\n" in error["message"]
    assert "panicked at" in error["message"]

    assert invocation_record["best_error_tag"] == "SERVER_PANICKED"
    category_key = invocation_record["best_error_category_key"].split(":")
    category_key[0:2] = [
        "buck2_client_ctx/src/daemon/client.rs",
        "CLIENT_GRPC",
        "SERVER_PANICKED",
    ]
    # TODO dump stack trace on windows
    if not is_running_on_windows():
        assert category_key[4].startswith("crash("), category_key[4]


@buck_test(inplace=False)
@env("BUCKD_STARTUP_TIMEOUT", "0")
async def test_connection_timeout(buck: Buck, tmp_path: Path) -> None:
    record_path = tmp_path / "record.json"
    res = await expect_failure(
        buck.targets(":", "--unstable-write-invocation-record", str(record_path))
    )
    assert "timed out before establishing connection to Buck daemon" in res.stderr

    record = read_invocation_record(record_path)

    assert record["command_end"] is None
    assert record["has_command_result"] is False
    assert record["has_end_of_stream"] is False
    assert record["daemon_connection_failure"] is True
    assert record["daemon_was_started"] is None

    assert record["best_error_tag"] == "DAEMON_CONNECT"


@buck_test(inplace=False)
async def test_daemon_abort(buck: Buck, tmp_path: Path) -> None:
    await buck.build()

    record = tmp_path / "record.json"
    await buck.debug(
        "crash", "abort", "--unstable-write-invocation-record", str(record)
    )
    invocation_record = read_invocation_record(record)

    errors = invocation_record["errors"]
    assert len(errors) == 1
    [error] = errors

    if is_running_on_windows():
        # TODO get windows to dump a stack trace
        assert "buckd stderr is empty" in error["message"]
    else:
        # Messages from folly's signal handler.
        assert "*** Aborted at" in error["message"]
        assert "*** Signal 6 (SIGABRT)" in error["message"]

    assert invocation_record["best_error_tag"] == "CLIENT_GRPC"
    category_key = invocation_record["best_error_category_key"].split(":")
    category_key[0:2] = [
        "buck2_client_ctx/src/daemon/client.rs",
        "CLIENT_GRPC",
        "SERVER_PANICKED",
    ]
    # TODO dump stack trace on mac and windows
    if is_running_on_linux():
        assert category_key[4].startswith("crash("), category_key[4]


@buck_test(inplace=False)
async def test_build_file_race(buck: Buck, tmp_path: Path) -> None:
    target = "//file_busy:file"
    # first build
    file_path = (await buck.build(target)).get_build_report().output_for_target(target)

    # Open the file for writing and keep it open
    f = open(file_path, "w")
    # build again, source code has changed, binary must be rebuilt
    record = tmp_path / "record.json"
    build = buck.build(
        target,
        "--show-output",
        "-c",
        "test.cache_buster=2",
        "--unstable-write-invocation-record",
        str(record),
    )

    if is_running_on_windows():
        await expect_failure(build)

        invocation_record = read_invocation_record(record)
        assert invocation_record["best_error_tag"] == "IO_MATERIALIZER_FILE_BUSY"
        assert invocation_record["errors"][0]["tier"] == ENVIRONMENT_ERROR
    else:
        await build

    f.close()
