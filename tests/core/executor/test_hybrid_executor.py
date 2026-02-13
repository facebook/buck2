# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

# pyre-strict


from typing import Optional

import pytest
from buck2.tests.e2e_util.api.buck import Buck
from buck2.tests.e2e_util.api.buck_result import BuckException, InvocationRecord
from buck2.tests.e2e_util.asserts import expect_failure
from buck2.tests.e2e_util.buck_workspace import buck_test, env
from buck2.tests.e2e_util.helper.utils import json_get, random_string, read_what_ran


@buck_test()
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


@buck_test()
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


@buck_test()
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


@buck_test()
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


@buck_test()
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


@buck_test()
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


@buck_test()
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


@buck_test()
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


@buck_test()
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


@buck_test()
async def test_executor_preference_with_remote_args(buck: Buck) -> None:
    opts = [
        "-c",
        f"test.cache_buster={random_string()}",
    ]

    await buck.build(
        "root//executor_preference_prefer_remote_arg_tests:",
        *opts,
    )


@buck_test()
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


@buck_test()
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


@buck_test()
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


@buck_test()
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


@buck_test()
async def test_build_fails_with_mutually_exclusive_executors(buck: Buck) -> None:
    with pytest.raises(BuckException):
        await buck.build(
            "--local-only", "--remote-only", "root//executor_threshold_tests/..."
        )


@buck_test()
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


@buck_test(write_invocation_record=True)
async def test_hybrid_executor_remote_queuing_fallback(buck: Buck) -> None:
    async def build(
        target: str, *opts: str, env: Optional[dict[str, str]] = None
    ) -> InvocationRecord:
        # kill to update env
        await buck.kill()
        res = await buck.build(
            f"root//executor_race_tests:{target}",
            "-c",
            f"test.cache_buster={random_string()}",
            *opts,
            env=env,
        )
        return res.invocation_record()

    async def scheduling_mode(buck: Buck) -> int:
        actions = await read_what_ran(buck)
        return actions[0]["scheduling_mode"]

    record = await build("slower_remotely_and_works_on_both_full_hybrid")
    assert record["run_local_count"] == 1
    assert record["run_remote_count"] == 0
    assert record["run_fallback_count"] == 0
    assert await scheduling_mode(buck) == "FullHybrid"

    record = await build(
        "slower_remotely_and_works_on_both_fallback_only",
        env={"BUCK2_TEST_RE_QUEUE_ESTIMATE_S": "0"},
    )
    assert record["run_local_count"] == 0
    assert record["run_remote_count"] == 1
    assert record["run_fallback_count"] == 0
    assert await scheduling_mode(buck) == "Fallback"

    record = await build(
        "slower_remotely_and_works_on_both_fallback_only",
        "-c",
        "build.remote_execution_fallback_on_estimated_queue_time_exceeds_s=10",
        env={"BUCK2_TEST_RE_QUEUE_ESTIMATE_S": "100"},
    )
    assert record["run_local_count"] == 1
    assert record["run_remote_count"] == 0
    assert record["run_fallback_count"] == 1
    assert await scheduling_mode(buck) == "FallbackReQueueEstimate"
