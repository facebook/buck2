# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

# pyre-strict


import glob
import json
from typing import Any, Dict, List

from buck2.tests.e2e_util.api.buck import Buck
from buck2.tests.e2e_util.asserts import expect_failure
from buck2.tests.e2e_util.buck_workspace import buck_test
from buck2.tests.e2e_util.helper.utils import read_what_ran


package = "fbcode//buck2/tests/targets/rules/worker_grpc"

worker_args = [
    "-c",
    "build.require_persistent_workers=True",
    "--no-remote-cache",
]


async def _read_what_ran_for_executor(
    buck: Buck, executor: str
) -> List[Dict[str, Any]]:
    return [
        entry
        for entry in (await read_what_ran(buck))
        if entry["reproducer"]["executor"] == executor
    ]


# disabled on mac due to Python GRPC issues
@buck_test(inplace=True, skip_for_os=["darwin", "windows"])
async def test_worker(buck: Buck) -> None:
    res = await buck.build(*worker_args, package + ":gen_worker_run_out")
    output = res.get_build_report().output_for_target(package + ":gen_worker_run_out")
    assert output.read_text() == "hello worker"
    assert len(await _read_what_ran_for_executor(buck, "Worker")) == 1


@buck_test(inplace=True, skip_for_os=["darwin", "windows"])
async def test_worker_disabled(buck: Buck) -> None:
    # Check non-worker exe runs if workers are disabled.
    fallback_args = ["-c", "build.use_persistent_workers=false", "--local-only"]
    res = await buck.build(*fallback_args, package + ":gen_worker_run_fallback")
    output = res.get_build_report().output_for_target(
        package + ":gen_worker_run_fallback"
    )
    assert "hello fallback" in output.read_text()
    assert len(await _read_what_ran_for_executor(buck, "Worker")) == 0


@buck_test(inplace=True, skip_for_os=["darwin", "windows"])
async def test_worker_shared(buck: Buck) -> None:
    # Check worker is shared between multiple actions.
    res = await buck.build(*worker_args, package + ":gen_worker_deps")
    trace_id = json.loads(res.stdout)["trace_id"]
    worker_dirs = glob.glob(f"/tmp/buck2_worker/{trace_id}*/stderr")
    num_workers = len(worker_dirs)
    assert num_workers == 1, f"expected 1 worker, found {worker_dirs} for {trace_id}"
    assert len(await _read_what_ran_for_executor(buck, "WorkerInit")) == 1


@buck_test(inplace=True, skip_for_os=["darwin", "windows"])
async def test_error_thrown_during_execution(buck: Buck) -> None:
    # Check error thrown during worker command execution.
    await expect_failure(
        buck.build(*worker_args, package + ":gen_worker_run_error"),
        stderr_regex="compile error",
    )


@buck_test(inplace=True, skip_for_os=["darwin", "windows"])
async def test_error_thrown_during_startup(buck: Buck) -> None:
    # Check error thrown on worker start up.
    await expect_failure(
        buck.build(*worker_args, package + ":gen_worker_init_fail"),
        stderr_regex="init error",
    )


@buck_test(inplace=True, skip_for_os=["darwin", "windows"])
async def test_error_thrown_missing_worker(buck: Buck) -> None:
    # Check error thrown by missing worker executable.
    await expect_failure(
        buck.build(*worker_args, package + ":gen_worker_missing"),
        stderr_regex="Worker failed to spawn",
    )


@buck_test(inplace=True, skip_for_os=["darwin", "windows"])
async def test_connection_error(buck: Buck) -> None:
    # Check connection error if worker server can't be connected to.
    await expect_failure(
        buck.build(*worker_args, package + ":gen_worker_init_deadlock"),
        stderr_regex="Worker failed to connect",
    )


# Streaming is stuck in "initialize_worker" state
# @buck_test(inplace=True, skip_for_os=["darwin", "windows"])
# async def test_streaming(buck: Buck) -> None:
#     # Streaming execution demo
#     res = await buck.build(*worker_args, package + ":gen_worker_run_out_streaming")
#     output = res.get_build_report().output_for_target(
#         package + ":gen_worker_run_out_streaming"
#     )
#     assert output.read_text() == "hello worker"
#     assert len(await _read_what_ran_for_executor(buck, "Worker")) == 1


# TODO(ctolliday) re-enable once cancellation is in place
# def assert_executed(
#     what_ran: List[Dict[str, Any]], expected: List[Tuple[str, str]]
# ) -> None:
#     def target_name(identity: str) -> str:
#         return identity.split()[0].split(":")[1]
#     expected_target_names = [entry[1] for entry in expected]
#     what_ran_matching = [
#         (entry["reproducer"]["executor"], target_name(entry["identity"]))
#         for entry in what_ran
#         if target_name(entry["identity"]) in expected_target_names
#     ]
#     assert what_ran_matching == expected, what_ran
#
# assert_executed(
#     await read_what_ran(buck),
#     [
#         # TODO(ctolliday) ideally this should use --no-remote-cache and check for "Re" not "Cache".
#         # Will fail if not cached because RE execution will block on local execution.
#         # Making workers cancellable fixes this.
#         ("Cache", "gen_slow_worker_fast_fallback"),
#         ("WorkerInit", "gen_fast_worker_slow_fallback"),
#         ("Worker", "gen_fast_worker_slow_fallback"),
#     ],
# )


@buck_test(inplace=True, skip_for_os=["darwin", "windows"])
async def test_worker_exit_handled(buck: Buck) -> None:
    # Check connection error if worker server dies mid-request
    await expect_failure(
        buck.build(
            *worker_args,
            package + ":gen_worker_init_self_destruct",
        ),
        stderr_regex="Worker exited while running command",
    )


@buck_test(inplace=True, skip_for_os=["darwin", "windows"])
async def test_hybrid_execution(buck: Buck) -> None:
    # 1. Check that building `:gen_slow_worker_fast_fallback` first (as dependency) causes remote to succeed and worker to be cancelled.
    # 2. Check that `:gen_fast_worker_slow_fallback` worker execution succeeds, using same worker initialized by 1.
    hybrid_args = [
        "-c",
        "build.use_persistent_workers=True",
        "-c",
        "build.use_limited_hybrid=False",
    ]
    await buck.build(*hybrid_args, package + ":gen_fast_worker_slow_fallback")
    assert len(await _read_what_ran_for_executor(buck, "WorkerInit")) == 1


@buck_test(inplace=True, skip_for_os=["darwin", "windows"])
async def test_worker_thread_limit(buck: Buck) -> None:
    worker_args = [
        "-c",
        "build.use_persistent_workers=True",
        "--local-only",
        "--no-remote-cache",
    ]
    await expect_failure(
        buck.build(*worker_args, package + ":gen_worker_concurrent_fail"),
        stderr_regex="Concurrency check failed",
    )
    await buck.build(*worker_args, package + ":gen_worker_concurrent_pass")


@buck_test(inplace=True)
async def test_dummy() -> None:
    # None of the tests in this file pass on Windows or mac and that upsets CI.
    pass
