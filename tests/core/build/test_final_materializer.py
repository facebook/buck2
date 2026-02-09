# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

# pyre-strict

"""
Tests for final materialization performance, focusing on the no-op case
where artifacts are already materialized.

These tests can be run in two modes:
1. Regular test mode: Uses small N values to verify correctness
2. Performance mode: Uses larger N values to measure materialization overhead

To run performance tests, use the perf_* test functions or run this file
directly with appropriate arguments.
"""

import json
import time
import typing

from buck2.tests.e2e_util.api.buck import Buck
from buck2.tests.e2e_util.buck_workspace import buck_test


async def get_materialization_duration_from_critical_path(
    buck: Buck,
) -> typing.Optional[float]:
    """Extract materialization duration from critical path log."""
    try:
        result = await buck.log("critical-path", "--format=json")
        for line in result.stdout.strip().splitlines():
            entry = json.loads(line)
            if entry.get("kind") == "materialization":
                # Duration is in microseconds in the log
                return entry.get("total_duration_us", 0) / 1_000_000.0
    except Exception:
        pass
    return None


async def run_build_and_measure(
    buck: Buck,
    target: str,
    extra_args: typing.Optional[list[str]] = None,
) -> tuple[float, typing.Optional[float]]:
    """
    Run a build and measure wall-clock time.

    Returns:
        Tuple of (wall_clock_seconds, materialization_seconds_from_log)
    """
    args = [target, "--no-remote-cache"]
    if extra_args:
        args.extend(extra_args)

    start = time.monotonic()
    await buck.build(*args)
    wall_clock = time.monotonic() - start

    mat_duration = await get_materialization_duration_from_critical_path(buck)

    return wall_clock, mat_duration


@buck_test()
async def test_small_flat_build(buck: Buck) -> None:
    """Test that building a small flat tset (10 artifacts) works correctly."""
    result = await buck.build("//:small_flat")
    build_report = result.get_build_report()
    output = build_report.output_for_target("root//:small_flat")
    assert output.exists()
    content = output.read_text()
    assert "10 artifacts" in content


@buck_test()
async def test_medium_balanced_build(buck: Buck) -> None:
    """Test that building a medium balanced tset (100 artifacts) works correctly."""
    result = await buck.build("//:medium_balanced")
    build_report = result.get_build_report()
    output = build_report.output_for_target("root//:medium_balanced")
    assert output.exists()
    content = output.read_text()
    assert "100 artifacts" in content


@buck_test()
async def test_medium_flat_build(buck: Buck) -> None:
    """Test that building a medium flat tset (100 artifacts) works correctly."""
    result = await buck.build("//:medium_flat")
    build_report = result.get_build_report()
    output = build_report.output_for_target("root//:medium_flat")
    assert output.exists()
    content = output.read_text()
    assert "100 artifacts" in content


@buck_test()
async def test_medium_deep_build(buck: Buck) -> None:
    """Test that building a medium deep tset (100 artifacts) works correctly."""
    result = await buck.build("//:medium_deep")
    build_report = result.get_build_report()
    output = build_report.output_for_target("root//:medium_deep")
    assert output.exists()
    content = output.read_text()
    assert "100 artifacts" in content


@buck_test()
async def test_noop_materialization_small_flat(buck: Buck) -> None:
    """
    Test no-op materialization for small flat tset.

    Builds the target twice - the second build should have minimal
    materialization overhead since artifacts are already present.
    """
    target = "//:small_flat"

    # First build - materialize everything
    await buck.build(target, "--no-remote-cache")

    # Second build - should be a no-op for materialization
    wall_clock, mat_duration = await run_build_and_measure(buck, target)

    # Basic sanity check - second build should complete reasonably fast
    assert wall_clock < 30.0, f"No-op build took too long: {wall_clock}s"


@buck_test()
async def test_noop_materialization_medium_balanced(buck: Buck) -> None:
    """
    Test no-op materialization for medium balanced tset.

    Builds the target twice - the second build should have minimal
    materialization overhead since artifacts are already present.
    """
    target = "//:medium_balanced"

    # First build - materialize everything
    await buck.build(target, "--no-remote-cache")

    # Second build - should be a no-op for materialization
    wall_clock, mat_duration = await run_build_and_measure(buck, target)

    # Basic sanity check - second build should complete reasonably fast
    assert wall_clock < 30.0, f"No-op build took too long: {wall_clock}s"


@buck_test()
async def test_configurable_target(buck: Buck) -> None:
    """
    Test the configurable target with custom parameters.
    """
    result = await buck.build(
        "//:configurable",
        "-c",
        "test.artifacts_per_node=2",
        "-c",
        "test.nodes_per_tset=5",
        "-c",
        "test.number_of_tsets=3",
    )
    build_report = result.get_build_report()
    output = build_report.output_for_target("root//:configurable")
    assert output.exists()
    content = output.read_text()
    # 2 * 5 * 3 = 30 artifacts
    assert "30 artifacts" in content


# Performance test functions - these can be called directly for benchmarking
# or integrated with a performance testing framework


async def perf_noop_materialization(
    buck: Buck,
    artifacts_per_node: int = 1,
    nodes_per_tset: int = 100,
    number_of_tsets: int = 10,
    iterations: int = 3,
) -> dict[str, typing.Any]:
    """
    Performance test for no-op materialization.

    Args:
        buck: Buck instance
        artifacts_per_node: Artifacts per logical node
        nodes_per_tset: Nodes per sub-tset
        number_of_tsets: Number of sub-tsets
        iterations: Number of no-op builds to measure

    Returns:
        Dictionary with performance metrics
    """
    target = "//:configurable"
    total_artifacts = artifacts_per_node * nodes_per_tset * number_of_tsets
    config_args = [
        "-c",
        f"test.artifacts_per_node={artifacts_per_node}",
        "-c",
        f"test.nodes_per_tset={nodes_per_tset}",
        "-c",
        f"test.number_of_tsets={number_of_tsets}",
    ]

    # Initial build to populate artifacts
    initial_start = time.monotonic()
    await buck.build(target, "--no-remote-cache", *config_args)
    initial_duration = time.monotonic() - initial_start

    # Measure no-op builds
    noop_durations: list[float] = []
    mat_durations: list[float] = []

    for _ in range(iterations):
        wall_clock, mat_duration = await run_build_and_measure(
            buck, target, config_args
        )
        noop_durations.append(wall_clock)
        if mat_duration is not None:
            mat_durations.append(mat_duration)

    return {
        "artifacts_per_node": artifacts_per_node,
        "nodes_per_tset": nodes_per_tset,
        "number_of_tsets": number_of_tsets,
        "total_artifacts": total_artifacts,
        "initial_build_seconds": initial_duration,
        "noop_iterations": iterations,
        "noop_wall_clock_seconds": {
            "min": min(noop_durations),
            "max": max(noop_durations),
            "avg": sum(noop_durations) / len(noop_durations),
            "all": noop_durations,
        },
        "materialization_seconds": {
            "min": min(mat_durations) if mat_durations else None,
            "max": max(mat_durations) if mat_durations else None,
            "avg": sum(mat_durations) / len(mat_durations) if mat_durations else None,
            "all": mat_durations,
        },
    }


@buck_test()
async def test_perf_small_scale(buck: Buck) -> None:
    """
    Small-scale performance test that runs as part of the test suite.

    This validates that the performance test infrastructure works correctly
    with a small number of artifacts.
    """
    results = await perf_noop_materialization(
        buck,
        artifacts_per_node=1,
        nodes_per_tset=10,
        number_of_tsets=2,
        iterations=2,
    )

    assert results["total_artifacts"] == 20
    assert results["noop_iterations"] == 2
    assert len(results["noop_wall_clock_seconds"]["all"]) == 2
    assert results["noop_wall_clock_seconds"]["avg"] > 0
