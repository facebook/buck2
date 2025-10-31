# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

# pyre-strict

import json
import typing

from buck2.tests.e2e_util.api.buck import Buck
from buck2.tests.e2e_util.buck_workspace import buck_test


@buck_test(skip_for_os=["darwin", "windows"])
async def test_default_output(buck: Buck) -> None:
    status = await start_daemon_and_get_status(buck)
    assert "tokio_runtime_metrics" not in status


@buck_test(skip_for_os=["darwin", "windows"])
async def test_includes_tokio(buck: Buck) -> None:
    status = await start_daemon_and_get_status(buck, include_tokio_runtime_metrics=True)
    assert "tokio_runtime_metrics" in status
    metrics = status["tokio_runtime_metrics"]
    assert "global_queue_depth" in metrics
    assert "num_alive_tasks" in metrics
    assert "num_workers" in metrics


@buck_test(skip_for_os=["darwin", "windows"])
async def test_notices_tokio_worker_override(buck: Buck) -> None:
    append_tokio_workers_config(buck)
    status = await start_daemon_and_get_status(buck, include_tokio_runtime_metrics=True)
    assert "tokio_runtime_metrics" in status
    metrics = status["tokio_runtime_metrics"]
    assert "num_workers" in metrics
    assert metrics["num_workers"] == 42  # from modification below


# Placeholder for tests to be listed successfully on non-Linux platforms.
async def test_noop() -> None:
    pass


def append_tokio_workers_config(buck: Buck) -> None:
    with open(buck.cwd / ".buckconfig", "a") as buckconfig:
        buckconfig.write("[build]\n")
        buckconfig.write("num_tokio_workers = 42")


async def start_daemon_and_get_status(
    buck: Buck, include_tokio_runtime_metrics: bool = False
) -> dict[str, typing.Any]:
    # Start the daemon
    await buck.targets(":")

    status_result = await (
        buck.status("--include-tokio-runtime-metrics")
        if include_tokio_runtime_metrics
        else buck.status()
    )
    status_data = json.loads(status_result.stdout)
    return status_data
