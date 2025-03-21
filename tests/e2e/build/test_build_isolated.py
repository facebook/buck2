# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

# pyre-strict


import json
import os
import platform
import re
import sys
from pathlib import Path
from typing import List

import pytest

from buck2.tests.e2e_util.api.buck import Buck
from buck2.tests.e2e_util.api.buck_result import BuckException
from buck2.tests.e2e_util.asserts import expect_failure
from buck2.tests.e2e_util.buck_workspace import buck_test, env

from buck2.tests.e2e_util.helper.assert_occurrences import (
    assert_occurrences,
    assert_occurrences_regex,
)
from buck2.tests.e2e_util.helper.utils import json_get, random_string, read_what_ran


# Eden materializer only available on Linux
def eden_linux_only() -> bool:
    return sys.platform == "linux"


############################################################################################
### NOTE(JakobDegen): Do not add new tests to this file. Instead:
###  1. Use or make a file in `tests/core` with a name that explains what you're testing.
###  2. Give that file its own data directory, not one shared with other tests.
############################################################################################


@buck_test(inplace=False, data_dir="pass")
async def test_pass(buck: Buck) -> None:
    results = await buck.build("//:abc")
    assert "does not have any outputs" not in results.stderr


@buck_test(inplace=False, data_dir="pass")
async def test_missing_target(buck: Buck) -> None:
    await expect_failure(buck.build("//:not_a_target_name"))


@buck_test(inplace=False, data_dir="pass")
async def test_success_message_printed(buck: Buck) -> None:
    results = await buck.build("//:abc", "--console=simplenotty")

    assert "BUILD SUCCEEDED" in results.stderr

    results = await buck.build("//:abc", "--console=simpletty")

    assert_occurrences("\x1b[38;5;10mBUILD SUCCEEDED\x1b[39m", results.stderr, 1)

    results = await buck.build("//:abc", "--console=super")

    assert_occurrences("\x1b[38;5;10mBUILD SUCCEEDED\x1b[39m", results.stderr, 1)


@buck_test(inplace=False, data_dir="failing")
async def test_multiple_errors_print_with_simple_console(buck: Buck) -> None:
    e = await expect_failure(
        buck.build(
            "--console=simple",
            "//:foo",
            "//:bar",
            "//:using_dir",
        )
    )

    # Make sure that streamed events come back
    assert_occurrences_regex("(Build ID|Buck UI):", e.stderr, 1)
    assert_occurrences("RE Session: ", e.stderr, 1)
    assert_occurrences_regex("^BUILD FAILED", e.stderr, 1)

    execution_error = "Action failed: {} (<unspecified>) (bin_false)"
    assert_occurrences(execution_error.format("root//:foo"), e.stderr, 2)
    assert_occurrences(execution_error.format("root//:bar"), e.stderr, 2)

    # TODO: Windows handle wrong binary path as internal error, maybe we should fix it.
    if platform.system() != "Windows":
        exit_code = "(Local|Remote) command returned non-zero exit code 1"
        assert_occurrences_regex(exit_code, e.stderr, 6)

    build_error = "Failed to build '{} (<unspecified>)'"
    assert_occurrences(build_error.format("root//:foo"), e.stderr, 1)
    assert_occurrences(build_error.format("root//:bar"), e.stderr, 1)
    # TODO(nmj): Remove this comment
    # assert_occurrences_regex("getting metadata for.*a_dir`", e.stderr, 1)

    e = await expect_failure(buck.build("--console=simple", "//:non_existent"))

    assert_occurrences_regex("^BUILD FAILED", e.stderr, 1)
    assert_occurrences(
        "Unknown target `non_existent` from package `root//`", e.stderr, 1
    )


@buck_test(inplace=False, data_dir="failing")
async def test_multiple_errors_print_with_super_console(buck: Buck) -> None:
    e = await expect_failure(
        buck.build(
            "--console=super",
            "//:foo",
            "//:bar",
            "//:using_dir",
        )
    )

    # See https://en.wikipedia.org/wiki/ANSI_escape_code#8-bit for how to decode this
    DARK_RED = re.escape("\033[38;5;1m")

    execution_error = "Action failed: "
    assert_occurrences_regex(execution_error, e.stderr, 3)

    # TODO: Windows handle wrong binary path as internal error, maybe we should fix it.
    if platform.system() != "Windows":
        exit_code = "(Local|Remote) command returned non-zero exit code 1"
        assert_occurrences_regex(exit_code, e.stderr, 3)

    # These will eventually be red.
    build_error = "Failed to build '{} (<unspecified>)'"
    assert_occurrences(build_error.format("root//:foo"), e.stderr, 1)
    assert_occurrences(build_error.format("root//:bar"), e.stderr, 1)
    # TODO(nmj): Remove this comment
    # assert_occurrences_regex("getting metadata for.*a_dir`", e.stderr, 1)

    assert_occurrences("\x1b[38;5;1mBUILD FAILED\x1b[39m", e.stderr, 1)

    e = await expect_failure(buck.build("--console=super", "//:non_existent"))

    target_error = f"{DARK_RED}Unknown target `non_existent` from package `root//`"
    assert_occurrences("\x1b[38;5;1mBUILD FAILED\x1b[39m", e.stderr, 1)
    assert_occurrences_regex(target_error, e.stderr, 1)


@buck_test(inplace=False, data_dir="pass")
async def test_stderr_is_printed_for_successful_actions(buck: Buck) -> None:
    no_color_text = "warning on stderr no color"
    # Support '\r\n' which is printed on Windows.
    simple_color_stripped = "\\] warning on stderr with color\\r?$"
    # Make sure we reset the terminal after printing.
    simple_color = "\x1b[33mwarning on stderr with color\x1b[0m"
    # Make sure we reset the terminal after printing. Color is represented
    # slightly differently (33m vs 38;5;3m) because of how parsed / sanitized
    # colors are stored. Those are the same color, though.
    super_color = "\x1b[38;5;3mwarning on stderr with color\x1b[39m"

    superconsole_stderr_line = "stderr for {} (printer writing_stderr)"
    simpleconsole_stderr_line = "stderr:"

    # By default stderr should not be relayed on success w/o the -v2 or higher flag.

    res = await buck.build("--console=simplenotty", "//:print_stderr_simple_notty")

    assert_occurrences(no_color_text, res.stderr, 0)
    assert_occurrences_regex(simple_color_stripped, res.stderr, 0)
    assert_occurrences(simpleconsole_stderr_line, res.stderr, 0)

    res = await buck.build("--console=simpletty", "//:print_stderr_simple_tty")

    assert_occurrences(no_color_text, res.stderr, 0)
    assert_occurrences(simple_color, res.stderr, 0)
    assert_occurrences(simpleconsole_stderr_line, res.stderr, 0)

    res = await buck.build("--console=super", "//:print_stderr_super")

    assert_occurrences(no_color_text, res.stderr, 0)
    assert_occurrences(super_color, res.stderr, 0)
    assert_occurrences(
        superconsole_stderr_line.format("root//:print_stderr_super"), res.stderr, 0
    )

    res = await buck.build(
        "-v5", "--console=simplenotty", "//:v_print_stderr_simple_notty"
    )

    assert_occurrences(no_color_text, res.stderr, 1)
    assert_occurrences_regex(simple_color_stripped, res.stderr, 1)

    res = await buck.build("-v5", "--console=simpletty", "//:v_print_stderr_simple_tty")

    assert_occurrences(no_color_text, res.stderr, 1)
    assert_occurrences(simple_color, res.stderr, 1)
    assert_occurrences(simpleconsole_stderr_line, res.stderr, 1)

    res = await buck.build("-v5", "--console=super", "//:v_print_stderr_super")

    assert_occurrences(no_color_text, res.stderr, 1)
    assert_occurrences(super_color, res.stderr, 1)
    assert_occurrences(
        superconsole_stderr_line.format("root//:v_print_stderr_super"), res.stderr, 1
    )

    # Things that print all the time should print w/o the verbosity flag, or with it

    res = await buck.build(
        "--console=simplenotty", "//:always_print_stderr_simple_notty"
    )

    assert_occurrences(no_color_text, res.stderr, 1)
    assert_occurrences_regex(simple_color_stripped, res.stderr, 1)
    assert_occurrences(simpleconsole_stderr_line, res.stderr, 1)

    res = await buck.build("--console=simpletty", "//:always_print_stderr_simple_tty")

    assert_occurrences(no_color_text, res.stderr, 1)
    assert_occurrences(simple_color, res.stderr, 1)
    assert_occurrences(simpleconsole_stderr_line, res.stderr, 1)

    res = await buck.build("--console=super", "//:always_print_stderr_super")

    assert_occurrences(no_color_text, res.stderr, 1)
    assert_occurrences(super_color, res.stderr, 1)
    assert_occurrences(
        superconsole_stderr_line.format("root//:always_print_stderr_super"),
        res.stderr,
        1,
    )

    res = await buck.build(
        "-v5", "--console=simplenotty", "//:v_always_print_stderr_simple_notty"
    )

    assert_occurrences(no_color_text, res.stderr, 1)
    assert_occurrences_regex(simple_color_stripped, res.stderr, 1)
    assert_occurrences(simpleconsole_stderr_line, res.stderr, 1)

    res = await buck.build(
        "-v5", "--console=simpletty", "//:v_always_print_stderr_simple_tty"
    )

    assert_occurrences(no_color_text, res.stderr, 1)
    assert_occurrences(simple_color, res.stderr, 1)
    assert_occurrences(simpleconsole_stderr_line, res.stderr, 1)

    res = await buck.build("-v5", "--console=super", "//:v_always_print_stderr_super")

    assert_occurrences(no_color_text, res.stderr, 1)
    assert_occurrences(super_color, res.stderr, 1)
    assert_occurrences(
        superconsole_stderr_line.format("root//:v_always_print_stderr_super"),
        res.stderr,
        1,
    )


@buck_test(inplace=False, data_dir="flagfiles")
async def test_flagfiles_are_located_correctly(buck: Buck) -> None:
    out = await buck.build("@//mode/dev", "cell//subdir:simple", rel_cwd=Path("cell"))

    build_report = out.get_build_report()
    output = build_report.output_for_target("cell//subdir:simple")
    assert output.read_text().rstrip() == "overridden"

    out = await buck.build("@//mode/dev", "cell//subdir:simple", rel_cwd=Path("cell"))

    build_report = out.get_build_report()
    output = build_report.output_for_target("cell//subdir:simple")
    assert output.read_text().rstrip() == "overridden"

    # Make sure that relative paths are resolved against the cell root
    # (determined from project root + cwd) if they're not found relative
    # to cwd
    out = await buck.build(
        "@mode/dev", "cell//subdir:simple", rel_cwd=Path("cell/subdir")
    )

    build_report = out.get_build_report()
    output = build_report.output_for_target("cell//subdir:simple")
    assert output.read_text().rstrip() == "overridden"
    assert (
        "`@mode/dev` was specified, but not found. Using file at `//mode/dev`.\n"
        'This behavior is being deprecated. Please use `"@//mode/dev"` instead'
    ) in out.stderr

    out = await buck.build("@cell//mode/dev", "cell//subdir:simple")

    build_report = out.get_build_report()
    output = build_report.output_for_target("cell//subdir:simple")
    assert output.read_text().rstrip() == "overridden"

    await expect_failure(
        buck.build("@cell/mode/missing", "cell//subdir:simple"),
        stderr_regex="Unable to read flag file at `cell/mode/missing`",
    )


@buck_test(inplace=False, data_dir="early_action_cutoff")
async def test_early_action_cutoff(buck: Buck, tmp_path: Path) -> None:
    sentinel = tmp_path / "sentinel"
    sentinel.touch()

    # This action is going to check that the file pointed at by "sentinel"
    # exists. Point it at a valid file.

    with open(buck.cwd / "sentinel", "w", encoding="utf-8") as f:
        f.write(str(sentinel))

    await buck.build("//:check")

    ## Now, invalidate the action, and remove the underlying sentinel file. If
    # :check executes now, it'll fail.

    with open(buck.cwd / "src", "w", encoding="utf-8") as f:
        f.write("TEXT2")

    sentinel.unlink()

    # Run it to find out

    await buck.build("//:check")


@buck_test(inplace=False, data_dir="execution_platforms")
async def test_hybrid_executor_threshold(buck: Buck) -> None:
    await buck.build(
        "root//executor_threshold_tests/...",
        "-c",
        f"test.cache_buster={random_string()}",
    )
    out = await read_what_ran(buck)

    executors = {line["identity"]: line["reproducer"]["executor"] for line in out}
    expected = {
        "root//executor_threshold_tests:big (<unspecified>) (head)": "Local",
        "root//executor_threshold_tests:cp_big (<unspecified>) (cp)": "Local",
        "root//executor_threshold_tests:small (<unspecified>) (head)": "Local",
        "root//executor_threshold_tests:cp_small (<unspecified>) (cp)": "Re",
    }
    assert executors == expected


@buck_test(inplace=False, data_dir="execution_platforms")
@pytest.mark.parametrize(
    "low_pass_filter",
    [
        "true",
        "false",
    ],
)
async def test_hybrid_executor_fallbacks(buck: Buck, low_pass_filter: str) -> None:
    opts = [
        "-c",
        f"test.cache_buster={random_string()}",
        "-c",
        f"test.experimental_low_pass_filter={low_pass_filter}",
    ]

    # Those work as they are allowed to fallback:
    await buck.build(
        "root//executor_fallback_tests:local_only",
        "root//executor_fallback_tests:local_only_full_hybrid",
        "root//executor_fallback_tests:remote_only_prefer_local",
        *opts,
    )

    # This one doesn't:
    await expect_failure(
        buck.build(
            "root//executor_fallback_tests:local_only_no_fallback",
            *opts,
        )
    )


@buck_test(inplace=False, data_dir="execution_platforms")
async def test_hybrid_executor_fallback_preferred_error(buck: Buck) -> None:
    opts = [
        "-c",
        f"test.cache_buster={random_string()}",
    ]

    await expect_failure(
        buck.build(
            "root//executor_fallback_tests:fails_both",
            *opts,
        ),
        stderr_regex="Failed on local",
    )

    await expect_failure(
        buck.build(
            "root//executor_fallback_tests:fails_both_prefer_local",
            *opts,
        ),
        stderr_regex="Failed on local",
    )


@buck_test(inplace=False, data_dir="execution_platforms")
@pytest.mark.parametrize(
    "target",
    [
        "slower_locally",
        "slower_locally_force_full_hybrid",
    ],
)
async def test_hybrid_executor_cancels_local_execution(buck: Buck, target: str) -> None:
    await buck.build(
        f"root//executor_race_tests:{target}",
        "-c",
        f"test.cache_buster={random_string()}",
    )

    log = (await buck.log("show")).stdout.strip().splitlines()
    commands = None

    for line in log:
        commands = commands or json_get(
            line,
            "Event",
            "data",
            "SpanEnd",
            "data",
            "ActionExecution",
            "commands",
        )

    assert commands is not None
    assert len(commands) == 2
    assert commands[0]["status"] == {"Cancelled": {}}
    assert commands[1]["status"] == {"Success": {}}


@buck_test(inplace=False, data_dir="execution_platforms")
async def test_hybrid_executor_logging(buck: Buck) -> None:
    await buck.build(
        "root//executor_fallback_tests:local_only",
        "-c",
        f"test.cache_buster={random_string()}",
    )

    log = (await buck.log("show")).stdout.strip().splitlines()
    commands = None

    for line in log:
        commands = commands or json_get(
            line,
            "Event",
            "data",
            "SpanEnd",
            "data",
            "ActionExecution",
            "commands",
        )

    assert commands is not None
    assert len(commands) == 2
    assert commands[0]["details"]["signed_exit_code"] != 0
    assert commands[0]["status"] == {"Failure": {}}
    assert commands[1]["details"]["signed_exit_code"] == 0
    assert commands[1]["status"] == {"Success": {}}


@buck_test(inplace=False, data_dir="execution_platforms")
@pytest.mark.parametrize(
    "low_pass_filter",
    [
        "true",
        "false",
    ],
)
async def test_hybrid_executor_prefer_local(buck: Buck, low_pass_filter: str) -> None:
    opts = [
        "-c",
        f"test.cache_buster={random_string()}",
        "-c",
        f"test.experimental_low_pass_filter={low_pass_filter}",
    ]

    # heavyweight_works_only_locally will only succeed if it runs locally, but
    # its weight would normally prevent that from happening. It has
    # prefer_local, so it only works if that results in local execution being
    # attempted.
    #
    # slower_and_works_only_locally will only work locally but it'll fail
    # faster on RE. This means it must not be attempted at al on RE.
    await buck.build(
        "root//executor_race_tests:heavyweight_works_only_locally",
        "root//executor_race_tests:slower_and_works_only_locally",
        *opts,
    )

    # Same as above, but with prefer-local on the build command line instead of the command.
    await buck.build(
        "root//executor_race_tests:heavyweight_works_only_locally_local_not_preferred",
        "root//executor_race_tests:slower_and_works_only_locally_local_not_preferred",
        "--prefer-local",
        *opts,
    )


@buck_test(inplace=False, data_dir="execution_platforms")
async def test_hybrid_executor_prefer_remote_local_fallback(buck: Buck) -> None:
    opts = [
        "-c",
        f"test.cache_buster={random_string()}",
    ]
    # Local only command that fails with --remote-only, passes with --prefer-remote
    await expect_failure(
        buck.build(
            "root//executor_fallback_tests:local_only_full_hybrid",
            "--remote-only",
            *opts,
        ),
        stderr_regex="Failed to build .*local_only_full_hybrid",
    )

    await buck.build(
        "root//executor_fallback_tests:local_only_full_hybrid",
        "--prefer-remote",
        *opts,
    )


@buck_test(inplace=False, data_dir="execution_platforms")
async def test_hybrid_executor_prefer_remote(buck: Buck) -> None:
    opts = [
        "-c",
        f"test.cache_buster={random_string()}",
    ]
    # Build execution is sequential and remote first with --prefer-remote
    # using an action that succeeds slowly on RE and fails fast locally
    # that would fail if run concurrently
    await buck.build(
        "root//executor_race_tests:slower_remotely",
        "--prefer-remote",
        *opts,
    )


@buck_test(inplace=False, data_dir="execution_platforms")
async def test_executor_preference_priority(buck: Buck) -> None:
    opts = [
        "-c",
        f"test.cache_buster={random_string()}",
    ]

    await buck.build(
        "root//executor_preference_tests:",
        "--prefer-remote",
        *opts,
    )


@buck_test(inplace=False, data_dir="execution_platforms")
async def test_executor_preference_with_remote_args(buck: Buck) -> None:
    opts = [
        "-c",
        f"test.cache_buster={random_string()}",
    ]

    await buck.build(
        "root//executor_preference_prefer_remote_arg_tests:",
        *opts,
    )


@buck_test(inplace=False, data_dir="execution_platforms")
async def test_executor_preference_with_remote_args_and_cli_override(
    buck: Buck,
) -> None:
    opts = [
        "-c",
        f"test.cache_buster={random_string()}",
    ]

    await expect_failure(
        buck.build(
            "root//executor_preference_prefer_remote_arg_tests:",
            # `--prefer-local` takes priority over any `ctx.actions.run()`
            "--prefer-local",
            *opts,
        )
    )


@buck_test(inplace=False, data_dir="execution_platforms")
async def test_prefer_local(buck: Buck) -> None:
    await expect_failure(
        buck.build(
            "root//executor_fallback_tests:local_only_no_fallback",
            "-c",
            f"test.cache_buster={random_string()}",
        )
    )

    await buck.build(
        "root//executor_fallback_tests:local_only_no_fallback", "--prefer-local"
    )


@buck_test(inplace=False, data_dir="execution_platforms")
async def test_local_only(buck: Buck) -> None:
    args = [
        "root//executor_fallback_tests:local_only_no_fallback",
        "-c",
        f"test.cache_buster={random_string()}",
    ]

    await expect_failure(buck.build(*args))

    await buck.build(
        *args,
        "--local-only",
    )


@buck_test(inplace=False, data_dir="execution_platforms")
async def test_remote_only(buck: Buck) -> None:
    args = [
        "root//executor_fallback_tests:remote_only_no_fallback",
        "root//executor_fallback_tests:remote_only_full_hybrid",
        "-c",
        f"test.cache_buster={random_string()}",
    ]

    await expect_failure(buck.build(*args))

    await buck.build(
        *args,
        "--remote-only",
    )


async def _assert_locally_executed_upload_attempted(buck: Buck, count: int = 1) -> None:
    await _assert_upload_attempted(buck, count)


async def _assert_upload_attempted(buck: Buck, count: int) -> None:
    log = (await buck.log("show")).stdout.strip().splitlines()
    uploads = []
    excluded_uploads = []

    for line in log:
        e = json_get(
            line,
            "Event",
            "data",
            "SpanEnd",
            "data",
            "CacheUpload",
        )
        if e is None:
            continue
        if e["success"] or e["re_error_code"] == "PERMISSION_DENIED":
            # Tolerate permission denied errors because we don't have a choice on CI :(
            uploads.append(e)
        else:
            excluded_uploads.append(e)

    if len(uploads) == count:
        return
    else:
        print(f"Expected {count} uploads", file=sys.stderr)
        print(f"Actual uploads: {uploads}", file=sys.stderr)
        print(f"Excluded uploads: {excluded_uploads}", file=sys.stderr)
        raise AssertionError("Wrong number of uploads, see above")


@buck_test(inplace=False, data_dir="execution_platforms")
@env("BUCK_LOG", "buck2_execute_impl::executors::caching=debug")
async def test_re_uploads(buck: Buck) -> None:
    args = ["-c", f"write.text={random_string()}"]
    await buck.build("root//upload_tests:write", *args)
    await _assert_locally_executed_upload_attempted(buck, 1)


@buck_test(inplace=False, data_dir="execution_platforms")
@env("BUCK_LOG", "buck2_execute_impl::executors::caching=debug")
async def test_re_uploads_dir(buck: Buck) -> None:
    args = ["-c", f"write.text={random_string()}"]
    await buck.build("root//upload_tests:write_in_dir", *args)
    await _assert_locally_executed_upload_attempted(buck, 1)


@buck_test(inplace=False, data_dir="execution_platforms")
@env("BUCK_LOG", "buck2_execute_impl::executors::caching=debug")
async def test_re_uploads_limit(buck: Buck) -> None:
    args = ["-c", f"write.text={random_string()}"]
    await buck.build("root//upload_tests:write_xxl", *args)
    await _assert_locally_executed_upload_attempted(buck, 0)


@buck_test(inplace=False, data_dir="toolchain_deps")
async def test_toolchain_deps(buck: Buck) -> None:
    # This test builds two targets, both with the same `default_target_platform` platform
    # but which should resolve to different execution platforms because of toolchain deps.
    # Both targets still get configured with the `default_target_platform` of release.
    #
    # The Python toolchain works on Windows/Linux, but we prefer Linux as an exec platform.
    # The ASIC toolchain only works on Windows, so `python_and_asic` (which does both) must
    # pick Windows for Python as well.
    result = await buck.build("root//tests:python_and_asic", "root//tests:python_only")
    python_and_asic = (
        result.get_build_report()
        .output_for_target("root//tests:python_and_asic")
        .read_text()
    )
    python_only = (
        result.get_build_report()
        .output_for_target("root//tests:python_only")
        .read_text()
    )

    # If any of the selects get resolved incorrectly, the toolchain binaries below will change.
    assert python_and_asic == "python_release_windows\nasic\n"
    assert python_only == "python_release_linux\n"

    await buck.build("root//...", "--target-platforms=root//config:platform_windows")

    # Check we get the toolchain dependencies in uquery and cquery
    result = await buck.uquery("deps(//toolchains:python)")
    assert "//toolchains:compile_python_release_linux\n" in result.stdout
    assert "//toolchains:python_debug\n" in result.stdout
    result = await buck.cquery(
        "deps(//toolchains:python)", "--target-platforms=root//config:platform_linux"
    )
    assert "//toolchains:compile_python_release_linux " in result.stdout
    assert "//toolchains:python_debug " not in result.stdout


@buck_test(inplace=False, data_dir="http_deferral")
@pytest.mark.parametrize(
    "digest_algorithm",
    [
        "SHA1",
        "SHA256",
    ],
)
async def test_http_deferral(buck: Buck, digest_algorithm: str) -> None:
    with open(buck.cwd / ".buckconfig", "a") as f:
        f.write("[buck2]\n")
        f.write(f"digest_algorithms = {digest_algorithm}\n")

    target = "//:download"

    # Check it was deferred
    res = await buck.build(target, "--materializations=none")
    output = res.get_build_report().output_for_target(target)
    assert not os.path.exists(output)

    # Check it can be materialized
    res = await buck.build(target)
    assert os.path.exists(output)


@buck_test(inplace=False, data_dir="http_deferral")
@env(
    "BUCK2_TEST_INJECTED_MISSING_DIGESTS",
    "1a45666759704bf08fc670aa96118a0415c470fc:221",
)
async def test_http_deferral_uploads(buck: Buck) -> None:
    await buck.build("//:target", "--no-remote-cache")


@buck_test(inplace=False, data_dir="no_output")
async def test_no_output(buck: Buck) -> None:
    results = await buck.build("//:none")

    assert "BUILD SUCCEEDED" in results.stderr
    assert "does not have any outputs" in results.stderr


@buck_test(inplace=False, data_dir="no_output")
async def test_no_output_wildcard(buck: Buck) -> None:
    results = await buck.build("//...")

    assert "BUILD SUCCEEDED" in results.stderr
    assert "does not have any outputs" not in results.stderr


@buck_test(inplace=False, data_dir="executor_caching")
async def test_executor_caching_disabled(buck: Buck) -> None:
    async def read_executors() -> List[str]:
        out = await read_what_ran(buck)
        return [line["reproducer"]["executor"] for line in out]

    seed = random_string()
    # Run on RE with cache lookup and writes disabled
    await buck.build(
        ":test",
        "-c",
        f"test.seed={seed}",
        "-c",
        "test.remote_enabled=true",
        "--no-remote-cache",
    )
    assert await read_executors() == ["Re"]

    await buck.kill()

    # Run on RE, should not get any cache hit as the cache writes were disabled
    await buck.build(
        ":test",
        "-c",
        f"test.seed={seed}",
        "-c",
        "test.remote_enabled=true",
    )

    executors = await read_executors()
    assert executors == ["Re"] or executors == []


@buck_test(inplace=False, data_dir="executor_caching")
async def test_executor_cache_writes_enabled(buck: Buck) -> None:
    async def read_executors() -> List[str]:
        out = await read_what_ran(buck)
        return [line["reproducer"]["executor"] for line in out]

    seed = random_string()
    # Run on RE with cache lookup disabled and cache writes enabled
    await buck.build(
        ":test",
        "-c",
        f"test.seed={seed}",
        "-c",
        "test.remote_enabled=true",
        "--no-remote-cache",
        "--write-to-cache-anyway",
    )
    assert await read_executors() == ["Re"]

    await buck.kill()

    # Run on RE, should not get cache hits as writes were enabled
    await buck.build(
        ":test",
        "-c",
        f"test.seed={seed}",
        "-c",
        "test.remote_enabled=true",
    )
    assert await read_executors() == ["Cache"]


@buck_test(inplace=False, data_dir="executor_caching")
async def test_executor_caching(buck: Buck) -> None:
    async def read_executors() -> List[str]:
        out = await read_what_ran(buck)
        return [line["reproducer"]["executor"] for line in out]

    seed = random_string()

    # Run on RE
    await buck.build(
        ":test", "-c", f"test.seed={seed}", "-c", "test.remote_enabled=true"
    )
    assert (await read_executors()) == ["Re"]

    # Run on RE, with caching (the default)
    await buck.kill()
    await buck.build(
        ":test", "-c", f"test.seed={seed}", "-c", "test.remote_enabled=true"
    )
    assert (await read_executors()) == ["Cache"]

    # Kill, run locally, no caching.
    await buck.kill()
    await buck.build(
        ":test",
        "-c",
        f"test.seed={seed}",
        "-c",
        "test.local_enabled=true",
        "-c",
        "test.remote_enabled=false",
        "-c",
        "test.remote_cache_enabled=false",
    )
    assert (await read_executors()) == ["Local"]

    # Kill, run locally, with caching (explicitly).
    await buck.kill()
    await buck.build(
        ":test",
        "-c",
        f"test.seed={seed}",
        "-c",
        "test.remote_enabled=false",
        "-c",
        "test.local_enabled=true",
        "-c",
        "test.remote_cache_enabled=true",
    )
    assert (await read_executors()) == ["Cache"]

    # Kill, run locally, with caching (the default).
    await buck.kill()
    await buck.build(
        ":test",
        "-c",
        f"test.seed={seed}",
        "-c",
        "test.remote_enabled=false",
        "-c",
        "test.local_enabled=true",
    )
    assert (await read_executors()) == ["Cache"]


@buck_test(inplace=False, data_dir="pass")
async def test_sandcastle_id_check(buck: Buck) -> None:
    async def pid() -> int:
        res = await buck.status()
        return json.loads(res.stdout)["process_info"]["pid"]

    await buck.build()
    pid1 = await pid()
    await buck.build(env={"SANDCASTLE_ID": "foo"})
    pid2 = await pid()
    await buck.build(env={"SANDCASTLE_ID": "foo"})
    pid3 = await pid()

    assert pid1 != pid2
    assert pid2 == pid3


@buck_test(inplace=False, data_dir="execution_platforms")
async def test_build_fails_with_mutually_exclusive_executors(buck: Buck) -> None:
    with pytest.raises(BuckException):
        await buck.build(
            "--local-only", "--remote-only", "root//executor_threshold_tests/..."
        )


@buck_test(inplace=False, data_dir="execution_platforms")
async def test_enforce_unique_inodes(buck: Buck) -> None:
    await buck.build(
        "root//executor_unique_inode_tests/...",
        "-c",
        f"test.cache_buster={random_string()}",
    )


@buck_test(inplace=False, data_dir="execution_platforms", skip_for_os=["windows"])
async def test_executable_bit(buck: Buck) -> None:
    await buck.build(
        "root//executor_exec_bit_tests/...",
        "-c",
        f"test.cache_buster={random_string()}",
    )


@buck_test(inplace=False, data_dir="execution_platforms")
@env("BUCK_OFFLINE_BUILD", "1")
async def test_build_offline(buck: Buck) -> None:
    await buck.build("root//executor_threshold_tests/...")
    out = await read_what_ran(buck)

    executors = {line["identity"]: line["reproducer"]["executor"] for line in out}
    expected = {
        "root//executor_threshold_tests:big (<unspecified>) (head)": "Local",
        "root//executor_threshold_tests:cp_big (<unspecified>) (cp)": "Local",
        "root//executor_threshold_tests:small (<unspecified>) (head)": "Local",
        "root//executor_threshold_tests:cp_small (<unspecified>) (cp)": "Local",
    }
    assert executors == expected


@buck_test(inplace=False, data_dir="execution_platforms")
async def test_symlink_output(buck: Buck) -> None:
    with open(buck.cwd / ".buckconfig.local", "w") as f:
        f.write("[buck2_re_client]\n")
        f.write("respect_file_symlinks = false\n")
    await buck.build(
        "root//executor_symlink_tests:check_not_symlink",
        "-c",
        f"test.cache_buster={random_string()}",
    )
    await buck.kill()
    with open(buck.cwd / ".buckconfig.local", "w") as f:
        f.write("[buck2_re_client]\n")
        f.write("respect_file_symlinks = true\n")
    await buck.build(
        "root//executor_symlink_tests:check_symlink",
        "-c",
        f"test.cache_buster={random_string()}",
    )


############################################################################################
### NOTE(JakobDegen): Do not add new tests to this file. Instead:
###  1. Use or make a different test file with a name that explains what you're testing.
###  2. Give that file its own data directory, not one shared with other tests.
############################################################################################
