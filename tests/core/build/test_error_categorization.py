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
from buck2.tests.e2e_util.helper.golden import golden, sanitize_stderr

from buck2.tests.e2e_util.helper.utils import (
    is_running_on_linux,
    is_running_on_windows,
    read_invocation_record,
)


@buck_test()
async def test_action_error(buck: Buck, tmp_path: Path) -> None:
    record_path = tmp_path / "record.json"
    await expect_failure(
        buck.build(
            "//:action_fail", "--unstable-write-invocation-record", str(record_path)
        ),
        stderr_regex="Failed to build 'root//:action_fail",
    )
    record = read_invocation_record(record_path)
    errors = record["errors"]

    assert len(errors) == 1
    assert errors[0]["category"] == "USER"
    # This test is unfortunately liable to break as a result of refactorings, since this is not
    # stable. Feel free to delete it if it becomes a problem.
    assert (
        errors[0]["source_location"]
        == "buck2_build_api/src/actions/error.rs::ActionError"
    )


@buck_test()
async def test_missing_outputs(buck: Buck, tmp_path: Path) -> None:
    record_path = tmp_path / "record.json"
    # FIXME(JakobDegen): This doesn't work with non-local-only actions
    await expect_failure(
        buck.build(
            "//:missing_outputs",
            "--local-only",
            "--unstable-write-invocation-record",
            str(record_path),
        ),
        stderr_regex="Failed to build 'root//:missing_outputs",
    )
    record = read_invocation_record(record_path)
    errors = record["errors"]
    assert len(errors) == 1
    assert errors[0]["category"] == "USER"


@buck_test()
async def test_bad_url(buck: Buck, tmp_path: Path) -> None:
    record_path = tmp_path / "record.json"
    await expect_failure(
        buck.build(
            "//:bad_url", "--unstable-write-invocation-record", str(record_path)
        ),
        stderr_regex="Failed to build 'root//:bad_url",
    )
    record = read_invocation_record(record_path)
    errors = record["errors"]
    assert len(errors) == 1
    # Also liable to break as a result of refactorings, feel free to update
    # FIXME(minglunli): This is a regression from before, the commented line is better and we should fix this
    assert "buck2_http/src/lib.rs" in errors[0]["source_location"]
    # assert errors[0]["source_location"] == "buck2_http/src/lib.rs::HttpError::SendRequest"


@buck_test()
async def test_attr_coercion(buck: Buck, tmp_path: Path) -> None:
    record_path = tmp_path / "record.json"
    await expect_failure(
        buck.build(
            "//attr_coercion:int_rule",
            "--unstable-write-invocation-record",
            str(record_path),
        ),
        stderr_regex="evaluating build file: `root//attr_coercion:TARGETS.fixture",
    )
    record = read_invocation_record(record_path)
    errors = record["errors"]
    assert len(errors) == 1
    # Just make sure there's some kind of error metadata
    assert "StarlarkError::Value" in errors[0]["source_location"]


@buck_test()
async def test_buck2_fail(buck: Buck, tmp_path: Path) -> None:
    record_path = tmp_path / "record.json"
    await expect_failure(
        buck.build(
            "//buck2_fail:foobar",
            "--unstable-write-invocation-record",
            str(record_path),
        ),
        stderr_regex="evaluating build file: `root//buck2_fail:TARGETS.fixture`",
    )
    record = read_invocation_record(record_path)
    errors = record["errors"]
    assert len(errors) == 1
    # Just make sure that despite there being no context on the error, we still report the right
    # metadata
    assert (
        errors[0]["source_location"]
        == "buck2_interpreter_for_build/src/interpreter/functions/internals.rs::BuckFail"
    )


@buck_test()
async def test_starlark_fail_error_categorization(buck: Buck, tmp_path: Path) -> None:
    record_path = tmp_path / "record.json"
    await expect_failure(
        buck.build(
            "//starlark_fail:foobar",
            "--unstable-write-invocation-record",
            str(record_path),
        ),
        stderr_regex="evaluating build file: `root//starlark_fail:TARGETS.fixture`",
    )
    record = read_invocation_record(record_path)
    errors = record["errors"]
    assert len(errors) == 1
    assert errors[0]["source_location"].endswith("StarlarkError::Fail")
    assert errors[0]["source_area"] == "BUCK2"
    assert errors[0]["category"] == "USER"


@buck_test()
async def test_starlark_parse_error_categorization(buck: Buck, tmp_path: Path) -> None:
    record_path = tmp_path / "record.json"
    await expect_failure(
        buck.build(
            "//starlark_parse_error:starlark_parse_error",
            "--unstable-write-invocation-record",
            str(record_path),
        ),
        stderr_regex=".*Parse error:.*",
    )
    record = read_invocation_record(record_path)
    errors = record["errors"]

    assert len(errors) == 1
    assert errors[0]["source_location"].endswith("StarlarkError::Parser")
    assert errors[0]["tags"] == ["STARLARK_PARSER"]
    assert errors[0]["source_area"] == "BUCK2"
    assert errors[0]["category"] == "USER"


@buck_test()
async def test_starlark_scope_error_categorization(buck: Buck, tmp_path: Path) -> None:
    record_path = tmp_path / "record.json"
    await expect_failure(
        buck.build(
            "//starlark_scope_error:value_err",
            "--unstable-write-invocation-record",
            str(record_path),
        ),
        stderr_regex="evaluating build file: .* not found",
    )
    record = read_invocation_record(record_path)
    errors = record["errors"]

    assert len(errors) == 1
    assert errors[0]["source_location"].endswith("StarlarkError::Scope")
    assert errors[0]["tags"] == ["STARLARK_SCOPE"]
    assert errors[0]["source_area"] == "BUCK2"
    assert errors[0]["category"] == "USER"


@buck_test()
async def test_targets_error_categorization(buck: Buck, tmp_path: Path) -> None:
    record_path = tmp_path / "record.json"
    res = await expect_failure(
        buck.targets(
            "//starlark_fail:foobar",
            "--unstable-write-invocation-record",
            str(record_path),
        ),
        stderr_regex="evaluating build file: `root//starlark_fail:TARGETS.fixture`",
    )
    record = read_invocation_record(record_path)
    errors = record["errors"]
    assert len(errors) == 1
    assert errors[0]["tags"] == ["INPUT", "STARLARK_FAIL"]
    assert errors[0]["category"] == "USER"

    golden(
        output=sanitize_stderr(res.stderr),
        rel_path="fixtures/test_targets_error_categorization.golden.txt",
    )


@buck_test()
async def test_daemon_crash(buck: Buck, tmp_path: Path) -> None:
    await buck.build()

    record = tmp_path / "record.json"
    res = await expect_failure(
        buck.debug("crash", "panic", "--unstable-write-invocation-record", str(record)),
    )
    invocation_record = read_invocation_record(record)

    errors = invocation_record["errors"]

    assert len(errors) == 1
    [error] = errors
    if is_running_on_windows():
        assert "transport error" in error["message"]
    else:
        assert "stream closed because of a broken pipe" in error["message"]

    assert error["tags"][0:3] == ["CLIENT_GRPC", "SERVER_PANICKED", "TONIC"]
    assert error["tags"][3].startswith("crash")
    assert "buckd stderr:\n" in error["message"]
    assert "panicked at" in error["message"]

    assert error["best_tag"] == "SERVER_PANICKED"
    category_key = error["category_key"]
    assert category_key.startswith("SERVER_PANICKED")

    # TODO dump stack trace on windows
    if not is_running_on_windows():
        assert "crash(" in category_key

    if not is_running_on_windows():
        golden(
            output=sanitize_stderr(res.stderr),
            rel_path="fixtures/test_daemon_crash.golden.txt",
        )


@buck_test()
@env("BUCKD_STARTUP_TIMEOUT", "0")
@env("BUCKD_STARTUP_INIT_TIMEOUT", "0")
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

    assert record["errors"][0]["best_tag"] == "SERVER_STDERR_UNKNOWN"

    if not is_running_on_windows():
        golden(
            output=sanitize_stderr(res.stderr),
            rel_path="fixtures/test_connection_timeout.golden.txt",
        )


@buck_test()
async def test_daemon_abort(buck: Buck, tmp_path: Path) -> None:
    await buck.build()

    record = tmp_path / "record.json"
    await expect_failure(
        buck.debug("crash", "abort", "--unstable-write-invocation-record", str(record))
    )
    invocation_record = read_invocation_record(record)

    errors = invocation_record["errors"]
    assert len(errors) == 1
    [error] = errors

    category_key = error["category_key"]

    if is_running_on_windows():
        # TODO get windows to dump a stack trace / detect signals
        assert "buckd stderr is empty" in error["message"]
        assert category_key == "SERVER_STDERR_EMPTY:TONIC"
    else:
        # Messages from folly's signal handler.
        assert "*** Aborted at" in error["message"]
        assert "*** Signal 6 (SIGABRT)" in error["message"]
        assert category_key.startswith("SERVER_SIGABRT")
        assert error["best_tag"] == "SERVER_SIGABRT"

    # TODO dump stack trace on mac and windows
    if is_running_on_linux():
        assert "crash(" in category_key
        # test string tags are in error_tags
        assert error["tags"][-1].startswith("crash(")


@buck_test()
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
        best_error = invocation_record["errors"][0]
        assert best_error["best_tag"] == "IO_MATERIALIZER_FILE_BUSY"
        assert best_error["category"] == "ENVIRONMENT"
    else:
        await build

    f.close()


@buck_test()
async def test_download_failure(buck: Buck, tmp_path: Path) -> None:
    # Upload action if necessary
    await buck.build("//:run_action", "--remote-only")
    await buck.clean()
    record_path = tmp_path / "record.json"
    res = await expect_failure(
        buck.build(
            "//:run_action",
            "--unstable-write-invocation-record",
            str(record_path),
            env={"BUCK2_TEST_FAIL_RE_DOWNLOADS": "true"},
        )
    )
    record = read_invocation_record(record_path)
    category_key = record["errors"][0]["category_key"]
    assert category_key == "RE_NOT_FOUND:UNKNOWN"
    assert (
        "Your build requires materializing an artifact that has expired in the RE CAS and Buck does not have it. This likely happened because your Buck daemon has been online for a long time. This error is currently unrecoverable. To proceed, you should restart Buck using `buck2 killall`."
        in res.stderr
    )


@buck_test()
async def test_re_execute_failure(buck: Buck, tmp_path: Path) -> None:
    # Upload action if necessary
    await buck.build("//:run_action", "--remote-only")
    await buck.clean()
    record_path = tmp_path / "record.json"
    await expect_failure(
        buck.build(
            "//:run_action",
            "--unstable-write-invocation-record",
            str(record_path),
            "--no-remote-cache",
            env={"BUCK2_TEST_FAIL_RE_EXECUTE": "true"},
        )
    )
    record = read_invocation_record(record_path)
    category_key = record["errors"][0]["category_key"]
    assert category_key == "RE_FAILED_PRECONDITION:UNKNOWN"


@buck_test()
async def test_local_incompatible(buck: Buck, tmp_path: Path) -> None:
    record_path = tmp_path / "record.json"
    res = await expect_failure(
        buck.build(
            "//:local_run_action",
            "--remote-only",
            "--no-remote-cache",
            "--unstable-write-invocation-record",
            str(record_path),
        )
    )

    assert "Incompatible executor preferences" in res.stderr

    record = read_invocation_record(record_path)
    best_error = record["errors"][0]
    assert best_error["category"] == "USER"
    assert (
        best_error["category_key"]
        == "IncompatibleExecutorPreferences:ANY_ACTION_EXECUTION"
    )


@buck_test()
@env("BUCK2_TEST_INIT_DAEMON_ERROR", "true")
async def test_daemon_startup_error(buck: Buck, tmp_path: Path) -> None:
    record_path = tmp_path / "record.json"
    res = await expect_failure(
        buck.targets(":", "--unstable-write-invocation-record", str(record_path))
    )
    assert "Injected init daemon error" in res.stderr
    assert "Error initializing DaemonStateData" in res.stderr

    record = read_invocation_record(record_path)
    errors = record["errors"]
    assert len(errors) == 1
    [error] = errors

    assert "DAEMON_CONNECT" in error["tags"]
    assert "DAEMON_STATE_INIT_FAILED" in error["tags"]

    golden(
        output=sanitize_stderr(res.stderr),
        rel_path="fixtures/test_daemon_startup_error.golden.txt",
    )


@buck_test(
    setup_eden=True,
    extra_buck_config={
        "buck2": {
            "allow_eden_io": "false",
        }
    },
    skip_for_os=["windows"],
)
async def test_eden_io_error_tagging(buck: Buck, tmp_path: Path) -> None:
    targets_file = buck.cwd / "TARGETS.fixture"

    # remove file read permissions during test execution, test setup will fail if permissions are set on any fixture earlier
    targets_file.chmod(0o000)

    # triggers file read IO error
    record_path = tmp_path / "record.json"
    await expect_failure(
        buck.targets(
            ":",
            "--unstable-write-invocation-record",
            str(record_path),
        )
    )
    record = read_invocation_record(record_path)
    errors = record["errors"]
    assert len(errors) == 1
    [error] = errors

    assert "IO_EDEN" in error["tags"]
    assert error["category_key"] == "IO_EDEN:IO_PERMISSION_DENIED"


@buck_test()
@env("BUCK2_TEST_FAIL_STREAMING", "true")
async def test_client_streaming_error(buck: Buck, tmp_path: Path) -> None:
    record_path = tmp_path / "record.json"
    res = await expect_failure(
        buck.targets(":", "--unstable-write-invocation-record", str(record_path))
    )
    assert "Injected client streaming error" in res.stderr

    record = read_invocation_record(record_path)
    errors = record["errors"]

    assert len(errors) == 1
    [error] = errors

    assert "Injected client streaming error" in error["message"]

    golden(
        output=sanitize_stderr(res.stderr),
        rel_path="fixtures/test_client_streaming_error.golden.txt",
    )


@buck_test()
async def test_action_error_has_categorization(buck: Buck, tmp_path: Path) -> None:
    record_path = tmp_path / "record.json"
    await expect_failure(
        buck.build(
            "//fail_action:error_handler_produced_error_categories",
            "--unstable-write-invocation-record",
            str(record_path),
        ),
        stderr_regex="Action sub-errors produced by error handlers",
    )

    record = read_invocation_record(record_path)
    errors = record["errors"]
    assert len(errors) == 1
    [error] = errors

    assert "ACTION_COMMAND_FAILURE" in error["tags"]
    assert error["category_key"] == "ACTION_COMMAND_FAILURE:FirstError"
