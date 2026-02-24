# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

# pyre-strict


import asyncio
import json
import os
import re
import signal
from pathlib import Path

import pytest
from buck2.tests.e2e_util.api.buck import Buck
from buck2.tests.e2e_util.api.buck_result import BuckException, ExitCodeV2
from buck2.tests.e2e_util.asserts import expect_failure
from buck2.tests.e2e_util.buck_workspace import (
    buck_test,
    env,
    get_mode_from_platform,
    is_deployed_buck2,
)

MAC_AND_WINDOWS = ["darwin", "windows"]


def remove_ansi_escape_sequences(ansi_str: str) -> str:
    """convert ansi_str to str"""
    ansi_escape = re.compile(r"\x1B(?:[@-Z\\-_]|\[[0-?]*[ -/]*[@-~])")
    return ansi_escape.sub("", ansi_str)


# TODO(marwhal): Fix and enable on Windows
@buck_test(inplace=True, skip_for_os=["windows"])
async def test_sh_test(buck: Buck) -> None:
    await buck.test(
        "fbcode//buck2/tests/targets/rules/sh_test:test",
    )

    await expect_failure(
        buck.test(
            "fbcode//buck2/tests/targets/rules/sh_test:test_fail",
        ),
        stderr_regex=r"1 TESTS FAILED\n(\s)+✗ fbcode\/\/buck2\/tests\/targets\/rules\/sh_test:test_fail - main",
    )


# TODO(marwhal): Fix and enable on Windows
@buck_test(inplace=True, skip_for_os=["windows"])
async def test_sh_test_remote_checks(buck: Buck) -> None:
    await expect_failure(
        buck.test(
            "fbcode//buck2/tests/targets/rules/sh_test:test",
            "--remote-only",
        ),
        stderr_regex="Incompatible executor preferences: `RemoteRequired` & `LocalRequired`",
    )
    await buck.test(
        "fbcode//buck2/tests/targets/rules/sh_test:test_remote_implicit",
        "--local-only",
    )
    await buck.test(
        "fbcode//buck2/tests/targets/rules/sh_test:test_remote_implicit",
        "--remote-only",
    )
    await expect_failure(
        buck.test(
            "fbcode//buck2/tests/targets/rules/sh_test:test_remote_explicit",
            "--local-only",
        ),
        stderr_regex="LocalOnly.*is incompatible",
    )
    await buck.test(
        "fbcode//buck2/tests/targets/rules/sh_test:test_remote_explicit",
        "--remote-only",
    )


# TODO(marwhal): Fix and enable on Windows
@buck_test(inplace=True, skip_for_os=["windows"])
async def test_test_build_fail(buck: Buck) -> None:
    await expect_failure(
        buck.test(
            "hewwo_buck",
        ),
        stderr_regex="does not exist",
    )

    await buck.test("fbcode//buck2/tests/targets/rules/sh_test:test")


@buck_test(inplace=True, skip_for_os=["darwin"])
async def test_cpp_test(buck: Buck) -> None:
    mode = get_mode_from_platform()
    await buck.test("fbcode//buck2/tests/targets/rules/cxx:cpp_test_pass", mode)

    await expect_failure(
        buck.test("fbcode//buck2/tests/targets/rules/cxx:cpp_test_fail", mode),
        stderr_regex=r"1 TESTS FAILED\n(\s)+✗ fbcode\/\/buck2\/tests\/targets\/rules\/cxx:cpp_test_fail - Simple\.Fail",
    )

    await buck.test("fbcode//buck2/tests/targets/rules/cxx:cpp_test_local_only", mode)

    await expect_failure(
        buck.test(
            "fbcode//buck2/tests/targets/rules/cxx:cpp_test_local_only",
            mode,
            "--remote-only",
        ),
        stderr_regex=r"The desired execution strategy \(.RemoteOnly.\) is incompatible with the executor config that was selected",
    )


@buck_test(inplace=True, skip_for_os=["windows"])
async def test_cpp_stress_runs(buck: Buck) -> None:
    mode = get_mode_from_platform()
    res = await buck.test(
        "fbcode//buck2/tests/targets/rules/cxx:cpp_test_pass",
        mode,
        "--",
        "--stress-runs=10",
    )

    assert "Pass 10" in res.stderr, "Expected stress runs to be run"


@buck_test(inplace=True, skip_for_os=["darwin"])
async def test_cpp_test_fdb_message(buck: Buck) -> None:
    await expect_failure(
        buck.test(
            "fbcode//buck2/tests/targets/rules/cxx:cpp_test_fail",
            get_mode_from_platform(),
            "--",
            "--color",
            "off",
        ),
        stderr_regex=r"Run \$ fdb buck test \<args\> to debug",
    )


@buck_test(inplace=True, skip_for_os=MAC_AND_WINDOWS)
async def test_python_test(buck: Buck) -> None:
    await buck.test("fbcode//buck2/tests/targets/rules/python/test:test")

    await buck.test("fbcode//buck2/tests/targets/rules/python/test:test_env")

    await expect_failure(
        buck.test("fbcode//buck2/tests/targets/rules/python/test:test_fail"),
        stderr_regex=r"1 TESTS FAILED\n(\s)+✗ fbcode\/\/buck2\/tests\/targets\/rules\/python\/test:test_fail - test",
    )

    await expect_failure(
        buck.test("fbcode//buck2/tests/targets/rules/python/test:test_fatal"),
        stderr_regex=r"1 TESTS FATALS\n(\s)+⚠ fbcode\/\/buck2\/tests\/targets\/rules\/python\/test:test_fatal - test",
    )


@buck_test(inplace=True, skip_for_os=MAC_AND_WINDOWS)
async def test_python_test_with_remote_execution(buck: Buck) -> None:
    await buck.test(
        "fbcode//buck2/tests/targets/rules/python/test:test_remote_execution",
    )
    await expect_failure(
        buck.test(
            "fbcode//buck2/tests/targets/rules/python/test:test_remote_execution_fail",
        ),
        stderr_regex=r"1 TESTS FAILED\n(\s)+✗ fbcode\/\/buck2\/tests\/targets\/rules\/python\/test:test_remote_execution_fail - test",
    )
    await expect_failure(
        buck.test(
            "fbcode//buck2/tests/targets/rules/python/test:test_remote_execution_fatal",
        ),
        stderr_regex=r"1 TESTS FATALS\n(\s)+⚠ fbcode\/\/buck2\/tests\/targets\/rules\/python\/test:test_remote_execution_fatal - test",
    )


@buck_test(inplace=True, skip_for_os=MAC_AND_WINDOWS)
async def test_python_needed_coverage(buck: Buck) -> None:
    await buck.test(
        "fbcode//buck2/tests/targets/rules/python/needed_coverage:test_pass",
        "fbcode//buck2/tests/targets/rules/python/needed_coverage:test_pass_specific_file",
    )
    await expect_failure(
        buck.test("fbcode//buck2/tests/targets/rules/python/needed_coverage:test_fail"),
        stderr_regex="ERROR: Actual coverage [0-9.]*% is smaller than expected 100.% for file",
    )
    await expect_failure(
        buck.test(
            "fbcode//buck2/tests/targets/rules/python/needed_coverage:test_fail_fractional"
        ),
        stderr_regex="ERROR: Actual coverage [0-9.]*% is smaller than expected [0-9.]*% for file",
    )


@buck_test(inplace=True, skip_for_os=MAC_AND_WINDOWS)
async def test_tests_attribute(buck: Buck) -> None:
    lib_tests = await buck.test("fbcode//buck2/tests/targets/rules/python/test:lib")
    assert "Pass 1" in remove_ansi_escape_sequences(lib_tests.stderr)


@buck_test(inplace=True, skip_for_os=MAC_AND_WINDOWS)
async def test_tests_attribute_ignore(buck: Buck) -> None:
    lib_tests = await buck.test(
        "fbcode//buck2/tests/targets/rules/python/test:lib",
        "--ignore-tests-attribute",
    )
    assert "NO TESTS RAN" in remove_ansi_escape_sequences(lib_tests.stderr)


@buck_test(inplace=True)
async def test_listing_failure(buck: Buck) -> None:
    output = await expect_failure(
        buck.test(
            "fbcode//buck2/tests/targets/rules/python/broken:broken",
            get_mode_from_platform(),
        ),
    )
    assert re.search(r"Listing Fail 1", output.stderr)
    assert re.search(
        r"1 LISTINGS FAILED\n(\s)+⚠ fbcode\/\/buck2\/tests\/targets\/rules\/python\/broken:broken\n",
        output.stderr,
        re.DOTALL,
    )


@buck_test(inplace=True, skip_for_os=["windows"])
async def test_python_import_error_with_static_listing_builtin_runner(
    buck: Buck,
) -> None:
    output = await expect_failure(
        buck.test(
            "fbcode//buck2/tests/targets/rules/python/broken:broken_with_static_listing_builtin_runner",
            get_mode_from_platform(),
        ),
    )

    assert re.search("2 TESTS FATALS", output.stderr, re.DOTALL)
    assert re.search(
        r"test_\d \(buck2.tests.targets.rules.python.broken.broken_import.TestCase\)",
        output.stderr,
        re.DOTALL,
    )
    assert not re.search("unittest.loader._FailedTest", output.stderr, re.DOTALL)


@buck_test(inplace=True, skip_for_os=["windows"])
async def test_python_import_error_with_static_listing_new_provider(buck: Buck) -> None:
    output = await expect_failure(
        buck.test(
            "fbcode//buck2/tests/targets/rules/python/broken:broken_with_static_listing_new_adapter",
            get_mode_from_platform(),
        ),
    )
    assert re.search("2 TESTS FATALS", output.stderr, re.DOTALL)
    assert not re.search("unittest.loader._FailedTest", output.stderr, re.DOTALL)
    assert re.search(
        r"test_\d \(buck2.tests.targets.rules.python.broken.broken_import.TestCase\)",
        output.stderr,
        re.DOTALL,
    )


@buck_test(inplace=True, skip_for_os=["windows"])
async def test_python_import_error_with_static_listing_new_provider_bundle(
    buck: Buck,
) -> None:
    output = await expect_failure(
        buck.test(
            "fbcode//buck2/tests/targets/rules/python/broken:broken_with_static_listing_new_adapter_bundle",
            get_mode_from_platform(),
        ),
    )
    assert re.search("1 TESTS FATALS", output.stderr, re.DOTALL)
    assert re.search(
        r"buck2\/tests\/targets\/rules\/python\/broken:broken_with_static_listing_new_adapter_bundle - main",
        output.stderr,
        re.DOTALL,
    )


@buck_test(inplace=True)
async def test_tests_dedupe(buck: Buck) -> None:
    lib_tests = await buck.test(
        "fbcode//buck2/tests/targets/rules/python/test:lib",
        "fbcode//buck2/tests/targets/rules/python/test:tests_for_lib",
        get_mode_from_platform(),
    )
    assert "Pass 1" in remove_ansi_escape_sequences(lib_tests.stderr)


@pytest.mark.parametrize("build_filtered", [(True), (False)])
@buck_test(
    inplace=True,
    skip_for_os=["windows"],  # TODO(marwhal): Fix and enable on Windows
)
async def test_label_filtering(buck: Buck, build_filtered: bool) -> None:
    cmd = ["fbcode//buck2/tests/targets/rules/label_test_filtering:"]
    if build_filtered:
        cmd.append("--build-filtered")

    await expect_failure(buck.test(*cmd), stderr_regex="1 TESTS FAILED")

    await expect_failure(
        buck.test(*cmd, "--exclude", "label-pass"), stderr_regex="1 TESTS FAILED"
    )

    await expect_failure(
        buck.test(*cmd, "--include", "label-fail"), stderr_regex="1 TESTS FAILED"
    )

    await expect_failure(
        buck.test(*cmd, "--include", "label-fail", "--exclude", "label-pass"),
        stderr_regex="1 TESTS FAILED",
    )

    await expect_failure(
        buck.test(
            *cmd,
        ),
        stderr_regex="1 TESTS FAILED",
    )

    await buck.test(*cmd, "--include", "label-pass")

    await buck.test(*cmd, "--exclude", "label-fail")

    await buck.test(*cmd, "--include", "!label-fail")

    await buck.test(
        *cmd, "--include", "label-fail", "--exclude", "label-fail", "--always-exclude"
    )

    await buck.test(*cmd, "--include", "!label-fail", "label-fail")


@buck_test(inplace=True, skip_for_os=MAC_AND_WINDOWS)
async def test_name_filtering(buck: Buck) -> None:
    await buck.test(
        "fbcode//buck2/tests/targets/rules/python/test/...", "--", "test_env"
    )

    await expect_failure(
        buck.test(
            "fbcode//buck2/tests/targets/rules/python/test/...", "--", "test_fail"
        ),
        stderr_regex="1 TESTS FAILED",
    )


@buck_test(inplace=True)
async def test_compile_error(buck: Buck) -> None:
    await expect_failure(
        buck.test(
            "fbcode//buck2/tests/targets/compile_error:cpp_test_compile_error",
            get_mode_from_platform(),
        ),
        stderr_regex="#error Compile error.*1 BUILDS FAILED.*NO TESTS RAN",
    )


@buck_test(
    inplace=True,
    skip_for_os=["windows"],  # TODO(marwhal): Fix and enable on Windows
)
async def test_cwd(buck: Buck) -> None:
    await buck.test(
        "fbcode//buck2/tests/targets/rules/sh_test:test_cwd",
    )


@buck_test(
    inplace=True,
    skip_for_os=["windows"],  # TODO(marwhal): Fix and enable on Windows
)
async def test_default_label_filtering(buck: Buck) -> None:
    await expect_failure(
        buck.test(
            "fbcode//buck2/tests/targets/rules/sh_test:test_fail_extended",
            "--",
            "--extended-tests",
        ),
        stderr_regex="1 TESTS FAILED",
    )

    # Ignores it by default
    await buck.test(
        "fbcode//buck2/tests/targets/rules/sh_test:test_fail_extended",
    )


@buck_test(
    inplace=True,
    skip_for_os=["windows"],  # TODO(marwhal): Fix and enable on Windows
)
async def test_stress_runs(buck: Buck) -> None:
    await expect_failure(
        buck.test(
            "fbcode//buck2/tests/targets/rules/sh_test:test_fail",
            "--",
            "--stress-runs",
            "10",
        ),
        stderr_regex="10 TESTS FAILED",
    )


# Not-in-place tests cannot run with deployed buck2
if not is_deployed_buck2():

    @buck_test(inplace=False, data_dir="testsof")
    @env("BUCK_LOG", "buck2_test::command=debug")
    async def test_target_compatibility(buck: Buck) -> None:
        out = await buck.test(
            "//...",
            "--target-platforms",
            "//:platform_default_tests",
        )

        assert "target incompatible node" in out.stderr

        await expect_failure(
            buck.test(
                "//:foo_extra_test",
                "--target-platforms",
                "//:platform_default_tests",
            ),
            stderr_regex="incompatible",
        )


# TODO(marwhal): Fix and enable on Windows
@buck_test(inplace=True, skip_for_os=["windows"])
async def test_external_runner_test_info_options(buck: Buck) -> None:
    await buck.test(
        "fbcode//buck2/tests/targets/rules/external_runner_test_info/...",
    )


# TODO(marwhal): Fix and enable on Windows
@buck_test(inplace=True, skip_for_os=["windows"])
async def test_allow_tests_on_re(buck: Buck) -> None:
    await buck.test(
        "fbcode//buck2/tests/targets/rules/external_runner_test_info/...",
        "--unstable-allow-tests-on-re",
    )


@buck_test(inplace=True)
async def test_incompatible_tests_do_not_run_on_re(buck: Buck) -> None:
    await expect_failure(
        buck.test(
            "fbcode//buck2/tests/targets/rules/external_runner_test_info:invalid_test",
            "-c",
            "external_runner_test_info.declare_invalid_test=1",
        ),
        stderr_regex="Trying to execute a `local_only = True` action on remote executor",
    )


@buck_test(inplace=True)
@env("TEST_MAKE_IT_FAIL", "1")
async def test_env_var_filtering(buck: Buck) -> None:
    await buck.test(
        "fbcode//buck2/tests/targets/rules/python/test:test",
        get_mode_from_platform(),
    )

    await expect_failure(
        buck.test(
            "fbcode//buck2/tests/targets/rules/python/test:test",
            get_mode_from_platform(),
            "--",
            "--env",
            "TEST_MAKE_IT_FAIL=1",
        ),
        stderr_regex="1 TESTS FAILED",
    )


@buck_test(inplace=True, skip_for_os=["windows"])
async def test_prepare_for_local_execution_env_with_env_cli_parameter(
    buck: Buck, tmp_path: Path
) -> None:
    out = tmp_path / "out"
    await buck.test(
        "fbcode//buck2/tests/targets/rules/python/test:test",
        "--",
        "--env",
        "EXTRA_VAR=foo",
        "--no-run-output-test-commands-for-fdb",
        str(out),
    )

    with open(out) as f:
        config = json.load(f)

    # Expect python/test:test target to support debugging. Executable field is populated only when debugging is supported.
    assert "debuggers" in config
    assert len(config["debuggers"]) > 0
    assert "executable" in config
    env = config["executable"]["env"]
    assert "PWD" in env
    assert "EXTRA_VAR" in env


# TODO(marwhal): Fix and enable on Windows
@buck_test(inplace=True, skip_for_os=["windows"])
@env("EXTRA_VAR", "foo")
async def test_prepare_for_local_execution_env(buck: Buck, tmp_path: Path) -> None:
    out = tmp_path / "out"
    await buck.test(
        "fbcode//buck2/tests/targets/rules/python/test:test",
        "--",
        "--no-run-output-test-commands-for-fdb",
        str(out),
    )

    with open(out) as f:
        config = json.load(f)

    # Expect python/test:test target to support debugging. Executable field is populated only when debugging is supported.
    assert "debuggers" in config
    assert len(config["debuggers"]) > 0
    assert "executable" in config
    env = config["executable"]["env"]
    assert "PWD" in env
    assert "EXTRA_VAR" not in env


@buck_test(inplace=True)
@env("BUCK2_TEST_TPX_USE_TCP", "true")
async def test_tcp(buck: Buck) -> None:
    await buck.test(
        "fbcode//buck2/tests/targets/rules/python/test:test",
        get_mode_from_platform(),
    )


@buck_test(inplace=True)
async def test_passing_test_names_are_not_shown(buck: Buck) -> None:
    # Passing test headers are not shown unless we pass --print-passing-details explicitly.
    tests = await buck.test(
        "fbcode//buck2/tests/targets/rules/python/test:test",
        get_mode_from_platform(),
    )
    assert (
        "Pass: fbcode//buck2/tests/targets/rules/python/test:test - test"
        not in tests.stderr
    )


@buck_test(inplace=True)
async def test_failing_test_names_are_shown(buck: Buck) -> None:
    await expect_failure(
        buck.test(
            "fbcode//buck2/tests/targets/rules/python/test:test",
            get_mode_from_platform(),
            "--",
            "--env",
            "TEST_ENV=fail",
        ),
        stderr_regex="Fail: fbcode//buck2/tests/targets/rules/python/test:test - test",
    )


@buck_test(inplace=True)
async def test_no_print_passing_details(buck: Buck) -> None:
    # Without --print-passing-details, test headers and stdout is NOT displayed.
    tests = await buck.test(
        "fbcode//buck2/tests/targets/rules/python/test:test",
        get_mode_from_platform(),
    )
    assert (
        "Pass: fbcode//buck2/tests/targets/rules/python/test:test - test"
        not in tests.stderr
    )
    assert "TESTED!" not in tests.stderr


@buck_test(inplace=True)
async def test_print_passing_details(buck: Buck) -> None:
    # With --print-passing-details, test headers and stdout is displayed.
    tests = await buck.test(
        "fbcode//buck2/tests/targets/rules/python/test:test",
        get_mode_from_platform(),
        "--",
        "--print-passing-details",
    )
    assert (
        "Pass: fbcode//buck2/tests/targets/rules/python/test:test - test"
        in tests.stderr
    )
    assert "TESTED!" in tests.stderr


@buck_test(inplace=True)
async def test_no_no_print_details(buck: Buck) -> None:
    # Without --no-print-details the stack trace is displayed.
    await expect_failure(
        buck.test(
            "fbcode//buck2/tests/targets/rules/python/test:test",
            get_mode_from_platform(),
            "--",
            "--env",
            "TEST_ENV=fail",
        ),
        stderr_regex="AssertionError: 41 != 42",
    )


@buck_test(inplace=True)
async def test_no_print_details(buck: Buck) -> None:
    # With --no-print-details the stack trace is not displayed.
    tests = await expect_failure(
        buck.test(
            "fbcode//buck2/tests/targets/rules/python/test:test",
            "--",
            "--env",
            "TEST_ENV=fail",
            "--no-print-details",
        ),
    )
    assert "AssertionError: 41 != 42" not in tests.stderr


@buck_test(inplace=True)
async def test_bundle_sharding(buck: Buck) -> None:
    tests = await buck.test(
        "fbcode//buck2/tests/targets/rules/python/test:multi_tests",
        get_mode_from_platform(),
    )
    assert "Pass 4" in tests.stderr


# TODO(marwhal): Fix and enable on Windows
@buck_test(inplace=True, skip_for_os=["windows"])
async def test_cancellation(buck: Buck, tmp_path: Path) -> None:
    """
    This test starts a test that writes its PID to a file then runs for 60
    seconds. We test cancellation by sending a CTRL+C as soon as a test
    starts. We then check that the process exited, and that nothing else
    started (or if anything did, that they stopped).
    """

    # Make sure we are ready to go
    await buck.build(
        "fbcode//buck2/tests/targets/rules/python/test:cancellation",
        "--build-test-info",
    )

    tests = buck.test(
        "fbcode//buck2/tests/targets/rules/python/test:cancellation",
        "--",
        "--stress-runs",
        "10",
        "--env",
        "SLOW_DURATION=60",
        "--env",
        f"PIDS={tmp_path}",
    )

    tests = await tests.start()

    for _i in range(30):
        await asyncio.sleep(1)
        pids = os.listdir(tmp_path)
        if pids:
            break
    else:
        raise Exception("Tests never started")

    tests.send_signal(signal.SIGINT)
    await tests.communicate()  # Wait for the command to exit

    # Give stuff time to settle, PIDS don't necessarily disappear
    # instantly. Also, verify that we are not starting more tests.
    await asyncio.sleep(5)

    # At this point, nothing should be alive.
    pids = os.listdir(tmp_path)
    for pid in pids:
        try:
            os.kill(int(pid), 0)
        except OSError:
            pass
        else:
            raise Exception(f"PID existed: {pid}")


@buck_test(inplace=True, skip_for_os=["windows"])
async def test_cancellation_on_re(buck: Buck) -> None:
    """
    This test starts a test on RE, waits for it to start, cancels, then starts
    again and verifies we don't wait for the test to finish.
    """

    # Make sure we are ready to go
    await buck.build(
        "fbcode//buck2/tests/targets/rules/python/test:cancellation",
        "--build-test-info",
    )

    tests = buck.test(
        "fbcode//buck2/tests/targets/rules/python/test:cancellation",
        "--unstable-force-tests-on-re",
        "--remote-only",
        "--no-remote-cache",
        "--",
        "--env",
        "SLOW_DURATION=60",
        "--env",
        "PIDS=/tmp",
    )

    tests = await tests.start()

    async def has_started() -> bool:
        try:
            stdout = (await buck.log("what-ran")).stdout
        except BuckException as e:
            # The log is truncated here so this can exit non-zero.
            stdout = e.stdout

        # what-ran returns things that started
        return "test.run" in stdout

    for _i in range(30):
        await asyncio.sleep(1)
        if await has_started():
            break
    else:
        raise Exception("Tests never started")

    tests.send_signal(signal.SIGINT)
    await tests.communicate()  # Wait for the command to exit

    # Run a command that cannot execute concurrerntly and check it does not
    # take 60 seconds to run, which means we went idle.
    await asyncio.wait_for(buck.audit_config("-c", "foo.bar=True"), timeout=10)


@buck_test(inplace=True, skip_for_os=["windows"])
async def test_timeout_local(buck: Buck) -> None:
    await expect_failure(
        buck.test(
            "fbcode//buck2/tests/targets/rules/python/test:timeout",
            "--local-only",
            "--no-remote-cache",
            "--",
            "--env",
            "SLOW_DURATION=60",
            "--timeout=5",
        ),
        stderr_regex="Timeout: fbcode//buck2/tests/targets/rules/python/test:timeout",
    )


@buck_test(inplace=True, skip_for_os=["windows"])
async def test_timeout_re(buck: Buck) -> None:
    await expect_failure(
        buck.test(
            "fbcode//buck2/tests/targets/rules/python/test:timeout",
            "--unstable-allow-all-tests-on-re",
            "--remote-only",
            "--no-remote-cache",
            "--",
            "--env",
            "SLOW_DURATION=60",
            "--timeout=5",
        ),
        stderr_regex="Timeout: fbcode//buck2/tests/targets/rules/python/test:timeout",
    )


if not is_deployed_buck2():

    @buck_test(inplace=True, skip_for_os=["windows"])
    async def test_overall_timeout(buck: Buck) -> None:
        """
        If an overall timeout is set, we expect that to result in OMITs
        reported in Tpx, and Tpx does not set an error status for that.

        We're OK with that, we will report how many OMITs there were.
        The caller is expected to be aware of how this feature works.
        """
        buck.test(
            "fbcode//buck2/tests/targets/rules/python/test:timeout",
            "--local-only",
            "--no-remote-cache",
            "--overall-timeout",
            "5s",
            "--",
            "--env",
            "SLOW_DURATION=60",
        )


@buck_test(inplace=True, skip_for_os=["windows"])
@pytest.mark.parametrize(
    "test",
    ["requires_env", "requires_env_location"],
)
async def test_test_env(buck: Buck, test: str) -> None:
    test = f"fbcode//buck2/tests/targets/rules/sh_test:{test}"

    await buck.test(test)

    # Check run also works. Note that those tests run from `fbcode` by default
    # so no chdir needed here.
    await buck.run(test)


@buck_test(inplace=True, skip_for_os=["windows"])
async def test_exit_code(buck: Buck) -> None:
    result = await expect_failure(
        buck.test("fbcode//buck2/tests/targets/rules/sh_test:test_fail")
    )
    assert result.process.returncode == 32
    result = await expect_failure(buck.test("not//a/real:target"))
    assert result.process.returncode == ExitCodeV2.USER_ERROR.value


@buck_test(inplace=True, skip_for_os=["windows"])
async def test_skip_missing_targets(buck: Buck) -> None:
    await expect_failure(
        buck.test("fbcode//buck2/tests/targets/rules/python/test:not_a_thing"),
        stderr_regex="Unknown target `not_a_thing`",
    )

    res = await buck.test(
        "fbcode//buck2/tests/targets/rules/python/test:not_a_thing",
        "--skip-missing-targets",
    )

    assert "Skipped 1 missing targets:" in res.stderr


@buck_test(inplace=True, skip_for_os=["darwin", "windows"])
async def test_test_worker(buck: Buck) -> None:
    worker_args = [
        "-c",
        "build.use_persistent_workers=True",
        "--local-only",
        "--no-remote-cache",
    ]
    await buck.test(
        *worker_args, "fbcode//buck2/tests/targets/rules/worker_grpc:worker_test"
    )


@buck_test(inplace=True, write_invocation_record=True)
@env("TEST_MAKE_IT_FAIL", "1")
async def test_failed_tests_has_error_category(buck: Buck) -> None:
    res = await expect_failure(
        buck.test(
            "fbcode//buck2/tests/targets/rules/python/test:test",
            get_mode_from_platform(),
            "--",
            "--env",
            "TEST_MAKE_IT_FAIL=1",
        ),
        stderr_regex="1 TESTS FAILED",
    )

    record = res.invocation_record()
    errors = record["errors"]

    assert len(errors) == 1
    assert errors[0]["category"] == "USER"
    assert "TestExecutor" in errors[0]["category_key"]
