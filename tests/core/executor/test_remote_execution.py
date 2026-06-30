# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

# pyre-strict

import os
import tempfile

from buck2.tests.e2e_util.api.buck import Buck
from buck2.tests.e2e_util.asserts import expect_failure
from buck2.tests.e2e_util.buck_workspace import buck_test, env
from buck2.tests.e2e_util.helper.utils import filter_events, random_string


@buck_test()
@env("BUCK2_TEST_FAIL_CONNECT", "true")
async def test_re_connection_failure_no_retry(buck: Buck) -> None:
    out = await expect_failure(
        buck.build(
            "root//:simple",
            "--remote-only",
            "--no-remote-cache",
        ),
    )

    assert "Injected RE Connection error" in out.stderr
    assert "retrying after sleeping" not in out.stderr


RE_USE_CASE_STAGES = ("Queue", "WorkerDownload", "Execute", "WorkerUpload")


async def assert_re_use_case(buck: Buck, expected_use_case: str) -> None:
    for action in RE_USE_CASE_STAGES:
        use_cases = await filter_events(
            buck,
            "Event",
            "data",
            "SpanStart",
            "data",
            "ExecutorStage",
            "stage",
            "Re",
            "stage",
            action,
            "use_case",
        )
        assert use_cases, f"No RE `{action}` stages found"
        assert all(use_case == expected_use_case for use_case in use_cases), use_cases


@buck_test()
async def test_re_use_case_override_with_arg(buck: Buck) -> None:
    # Make sure action is not cached
    with open(buck.cwd / "input.txt", "w") as f:
        f.write(random_string())
    await buck.build(
        "root//:simple",
        "--remote-only",
        "--no-remote-cache",
    )
    await assert_re_use_case(buck, "buck2-testing")
    # Change the target input
    with open(buck.cwd / "input.txt", "w") as f:
        f.write(random_string())
    await buck.build(
        "root//:simple",
        "--remote-only",
        "--no-remote-cache",
        "--config",
        "buck2_re_client.override_use_case=buck2-user",
    )
    await assert_re_use_case(buck, "buck2-user")


@buck_test()
async def test_re_use_case_override_with_config(buck: Buck) -> None:
    # Make sure action is not cached
    with open(buck.cwd / "input.txt", "w") as f:
        f.write(random_string())
    await buck.build(
        "root//:simple",
        "--remote-only",
        "--no-remote-cache",
    )
    await assert_re_use_case(buck, "buck2-testing")
    # Change the target input
    with open(buck.cwd / "input.txt", "w") as f:
        f.write(random_string())
    with open(buck.cwd / ".buckconfig.local", "w") as f:
        f.write("[buck2_re_client]\n")
        f.write("override_use_case = buck2-user\n")
    await buck.build(
        "root//:simple",
        "--remote-only",
        "--no-remote-cache",
    )
    await assert_re_use_case(buck, "buck2-user")


@buck_test()
async def test_re_use_case_override_with_external_config(buck: Buck) -> None:
    # Make sure action is not cached
    with open(buck.cwd / "input.txt", "w") as f:
        f.write(random_string())
    await buck.build(
        "root//:simple",
        "--remote-only",
        "--no-remote-cache",
    )
    await assert_re_use_case(buck, "buck2-testing")
    # Change the target input
    with open(buck.cwd / "input.txt", "w") as f:
        f.write(random_string())
    with tempfile.NamedTemporaryFile("w", delete=False) as f:
        f.write("[buck2_re_client]\n")
        f.write("override_use_case = buck2-user\n")
        f.close()
        await buck.build(
            "root//:simple",
            "--remote-only",
            "--no-remote-cache",
            "--config-file",
            f.name,
        )
    await assert_re_use_case(buck, "buck2-user")


@buck_test()
async def test_re_use_case_override_with_external_config_source(buck: Buck) -> None:
    with tempfile.NamedTemporaryFile("w", delete=False) as temp:
        env = os.environ.copy()
        env["BUCK2_TEST_EXTRA_EXTERNAL_CONFIG"] = temp.name
        # Make sure action is not cached
        with open(buck.cwd / "input.txt", "w") as f:
            f.write(random_string())
        await buck.build(
            "root//:simple",
            "--remote-only",
            "--no-remote-cache",
            env=env,
        )
        await assert_re_use_case(buck, "buck2-default")
        # Change the target input
        with open(buck.cwd / "input.txt", "w") as f:
            f.write(random_string())
        temp.write("[buck2_re_client]\n")
        temp.write("override_use_case = buck2-user\n")
        temp.flush()
        await buck.build(
            "root//:simple",
            "--remote-only",
            "--no-remote-cache",
            env=env,
        )
        await assert_re_use_case(buck, "buck2-user")
