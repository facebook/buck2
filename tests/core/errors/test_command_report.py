# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

# pyre-strict


import asyncio
import json
from pathlib import Path
from typing import List

from buck2.tests.e2e_util.api.buck import Buck
from buck2.tests.e2e_util.asserts import expect_failure
from buck2.tests.e2e_util.buck_workspace import buck_test, env
from buck2.tests.e2e_util.helper.golden import golden

# From `exit_result.rs/ExitCode`
Success = 0
UnknownFailure = 1
InfraError = 2
UserError = 3
ConnectError = 11


def command_report_test(name: str, command: List[str]) -> None:
    async def impl(buck: Buck, tmp_path: Path) -> None:
        report = tmp_path / "command_report.json"
        await expect_failure(buck.build("--command-report-path", str(report), *command))

        with open(report) as f:
            report = json.loads(f.read())
        del report["trace_id"]

        golden(
            output=json.dumps(report, indent=2, sort_keys=True),
            rel_path="fixtures/" + name + ".golden.json",
        )
        pass

    globals()[name] = impl

    return buck_test()(impl)


# Test build with a couple of build errors
command_report_test("test_command_report_build_errors", [":fail1", ":fail2"])


# Set Watchman timeout to 0 to mimic a Watchman Timeout error.
@buck_test(extra_buck_config={"buck2": {"file_watcher": "watchman"}})
@env("BUCK2_WATCHMAN_TIMEOUT", "0")
async def test_command_report_watchman_error(buck: Buck, tmp_path: Path) -> None:
    report = tmp_path / "command_report.json"
    await expect_failure(
        buck.build("--command-report-path", str(report), ":build_success")
    )

    with open(report) as f:
        report = json.loads(f.read())

    assert report["exit_code"] == UserError
    assert "SyncableQueryHandler returned an error" in report["error_messages"][0]


# Early client error that doesn't show up in invocation records
@buck_test()
@env("BUCK2_TEST_INIT_DAEMON_ERROR", "true")
async def test_command_report_init_daemon_error(buck: Buck, tmp_path: Path) -> None:
    report = tmp_path / "command_report.json"
    await expect_failure(
        buck.build("--command-report-path", str(report), ":build_success")
    )

    with open(report) as f:
        report = json.loads(f.read())

    assert report["exit_code"] == ConnectError
    assert "Injected init daemon error" in report["error_messages"][0]


# Deliberately cause a daemon connection failure.
@buck_test()
@env("BUCK2_TEST_FAIL_BUCKD_AUTH", "true")
# This test case spawns a loose daemon that we can't connect to. On windows
# this loose daemon will keep holding onto buck-out files after test case finishes
# and prevent other processes from changing them, so set a termination timeout
# of 20 seconds so that this loose daemon gets killed before test case finishes.
@env("BUCK2_TERMINATE_AFTER", "15")
async def test_exit_result_connection_error(buck: Buck, tmp_path: Path) -> None:
    report = tmp_path / "command_report.json"
    await expect_failure(
        buck.build("--command-report-path", str(report), ":build_success")
    )

    with open(report) as f:
        report = json.loads(f.read())

    assert report["exit_code"] == ConnectError
    assert "injected auth error" in report["error_messages"][0]

    await asyncio.sleep(
        20
    )  # Makes sure the daemon terminates before test case finishes


# Late client error takes precedence over action errors
@buck_test()
@env("BUCK2_TEST_BUILD_ERROR", "true")
# Ideally both action error and client error should both be included in the exit result,
# but this is difficult to do due to the current design, it might be easier to just rewrite
# and merge exit result stuff into invocation record so everything is logged to scuba.
async def test_command_report_post_build_client_error(
    buck: Buck, tmp_path: Path
) -> None:
    report = tmp_path / "command_report.json"
    # Failed build that should have some action errors
    await expect_failure(buck.build("--command-report-path", str(report), ":fail1"))

    with open(report) as f:
        report = json.loads(f.read())

    # There's only 1 error message and it's the late client error that's injected
    assert len(report["error_messages"]) == 1
    assert report["exit_code"] == UnknownFailure
    assert "Injected Build Response Error" in report["error_messages"][0]


@buck_test()
async def test_cleanup_timeout(buck: Buck, tmp_path: Path) -> None:
    report = tmp_path / "command_report.json"
    await buck.targets("--command-report-path", str(report), ":")

    with open(report) as f:
        report = json.loads(f.read())

    # test commands have scribe logging disabled, which is reported as a finalizing error
    finalizing_errors = report["finalizing_error_messages"]
    assert len(finalizing_errors) == 1
    assert "'invocation recorder' failed to finalize" in finalizing_errors[0]
    assert "Scribe sink not enabled" in finalizing_errors[0]
