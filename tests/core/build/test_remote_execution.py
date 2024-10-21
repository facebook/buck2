# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

# pyre-strict

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


async def filter_re_use_case(buck: Buck) -> list[str]:
    use_cases = []
    for action in ["Queue", "WorkerDownload", "Execute", "WorkerUpload"]:
        use_cases.extend(
            await filter_events(
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
        )
    return use_cases


@buck_test()
async def test_re_use_case_override(buck: Buck) -> None:
    # Make sure action is not cached
    with open(buck.cwd / "input.txt", "w") as f:
        f.write(random_string())
    await buck.build(
        "root//:simple",
        "--remote-only",
        "--no-remote-cache",
    )
    use_cases = await filter_re_use_case(buck)
    assert len(use_cases) == 4
    assert all(use_case == "buck2-default" for use_case in use_cases)
    # Change the target input
    with open(buck.cwd / "input.txt", "w") as f:
        f.write(random_string())
    await buck.build(
        "root//:simple",
        "--remote-only",
        "--no-remote-cache",
        "--config",
        "buck2_re_client.override_use_case=buck2-testing",
    )
    use_cases = await filter_re_use_case(buck)
    assert len(use_cases) == 4
    assert all(use_case == "buck2-testing" for use_case in use_cases)
