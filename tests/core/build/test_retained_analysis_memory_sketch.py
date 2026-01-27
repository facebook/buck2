# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

# pyre-strict

import json
import os
import subprocess
from pathlib import Path

from buck2.tests.e2e_util.api.buck import Buck
from buck2.tests.e2e_util.buck_workspace import buck_test


def check_merged_cardinality(sketch_strs: list[str], expected: float) -> None:
    """
    Verify that merged sketch cardinality matches expected value within 10% tolerance.

    Args:
        sketch_strs: List of sketch strings to merge
        expected: Expected cardinality value
    """
    # Remove version prefixes
    base64_strs = []
    for sketch_str in sketch_strs:
        if not sketch_str.startswith("V1:"):
            raise ValueError(f"Unexpected sketch version: {sketch_str[:10]}")
        base64_strs.append(sketch_str[3:])

    sketch_size_bin = os.environ["SKETCH_SIZE_BIN"]
    result = subprocess.run(
        [sketch_size_bin],
        input="\n".join(base64_strs),
        capture_output=True,
        text=True,
        check=True,
    )
    cardinality = float(result.stdout.strip())

    tolerance = 0.05
    lower_bound = expected * (1 - tolerance)
    upper_bound = expected * (1 + tolerance)

    assert lower_bound <= cardinality <= upper_bound, (
        f"Sketch cardinality {cardinality:.0f} should be "
        f"between {lower_bound:.0f} and {upper_bound:.0f} (expected ~{expected:,})"
    )


async def get_target_sketches(
    buck: Buck, tmp_path: Path, targets: list[str]
) -> dict[str, str]:
    """
    Build targets and extract their sketches from the build report.

    Args:
        buck: Buck test fixture
        tmp_path: Temporary directory for build report
        targets: List of targets to build (e.g., ["//:target1", "//:target2"])

    Returns:
        Dict mapping target name (e.g., "root//:target1") to sketch string
    """
    report = tmp_path / "build-report.json"

    # Build with sketch enabled
    await buck.build(
        *targets,
        "-c",
        "buck2.log_retained_analysis_memory_sketch=true",
        "--build-report",
        str(report),
    )

    # Read and parse build report
    with open(report) as f:
        report_data = json.load(f)

    # Extract sketches for each target
    sketches = {}
    for target in targets:
        # Convert target pattern to full target name
        # "//:target1" -> "root//:target1"
        full_target = f"root{target}"
        sketch = report_data["results"][full_target]["configured"]["<unspecified>"][
            "retained_analysis_memory_sketch"
        ]
        assert sketch is not None, f"Sketch should be present for {full_target}"
        assert sketch.startswith("V1:"), (
            f"Sketch should have V1 version prefix for {full_target}"
        )
        sketches[target] = sketch

    return sketches


@buck_test()
async def test_retained_analysis_memory_sketch(buck: Buck, tmp_path: Path) -> None:
    """
    Test that retained_analysis_memory_sketch is computed correctly.

    We use a custom rule that allocates memory in analysis,
    then verify the sketch cardinality matches the expected amount.
    """
    # target1 allocates 1M + shared_dep allocates 5M = ~6M total
    sketches = await get_target_sketches(buck, tmp_path, ["//:target1"])
    check_merged_cardinality([sketches["//:target1"]], 6_000_000)


@buck_test()
async def test_retained_analysis_memory_sketch_merging(
    buck: Buck, tmp_path: Path
) -> None:
    """
    Test that sketch merging works correctly.

    Build two targets that share a dependency, then verify that merging
    their sketches gives the correct combined cardinality.
    """
    # Build both targets
    sketches = await get_target_sketches(buck, tmp_path, ["//:target1", "//:target2"])

    sketch1 = sketches["//:target1"]
    sketch2 = sketches["//:target2"]

    # Expected:
    # target1: 1M + 5M (shared) = 6M
    # target2: 1.5M + 5M (shared) = 6.5M
    # merged: 1M + 1.5M + 5M (shared counted once) = 7.5M
    check_merged_cardinality([sketch1], 6_000_000)
    check_merged_cardinality([sketch2], 6_500_000)
    check_merged_cardinality([sketch1, sketch2], 7_500_000)


@buck_test()
async def test_retained_analysis_memory_sketch_bzl_globals(
    buck: Buck, tmp_path: Path
) -> None:
    """
    Test that memory from bzl file globals is included in the sketch.

    Rules defined in bzl files can have global data that gets retained.
    This test verifies that such memory is reflected in the sketch.
    """
    sketches = await get_target_sketches(buck, tmp_path, ["//:target_with_globals"])

    # Verify global data memory is included (at least 1M from GLOBAL_DATA)
    check_merged_cardinality([sketches["//:target_with_globals"]], 1_000_000)


@buck_test()
async def test_retained_analysis_memory_sketch_daemon_restart(
    buck: Buck, tmp_path: Path
) -> None:
    """
    Test that sketches are bitwise identical across daemon restarts.

    Build a target, kill the daemon, rebuild, and verify sketches are exactly equal.
    """
    # First build
    sketches1 = await get_target_sketches(buck, tmp_path, ["//:target1"])

    # Kill daemon
    await buck.kill()

    # Second build after restart
    sketches2 = await get_target_sketches(buck, tmp_path, ["//:target1"])

    # Sketches should be bitwise identical
    assert sketches1["//:target1"] == sketches2["//:target1"], (
        "Sketches should be bitwise identical across daemon restarts"
    )


@buck_test()
async def test_retained_analysis_memory_sketch_anon_targets(
    buck: Buck, tmp_path: Path
) -> None:
    """
    Test that memory from anon targets is included in the sketch.

    Anon targets allocate their own analysis memory which should be
    reflected in the parent target's sketch.
    """
    sketches = await get_target_sketches(buck, tmp_path, ["//:target_with_anon"])

    # The anon target allocates 500K + parent allocates 100K = ~600K
    check_merged_cardinality([sketches["//:target_with_anon"]], 600_000)


@buck_test()
async def test_retained_analysis_memory_sketch_disabled(
    buck: Buck, tmp_path: Path
) -> None:
    """Test that sketch is not present when config is disabled."""
    report = tmp_path / "build-report.json"

    # Build without sketch config
    await buck.build(
        "//:target1",
        "--build-report",
        str(report),
    )

    # Read build report
    with open(report) as f:
        report_data = json.load(f)

    # Verify sketch field is absent
    configured_entry = report_data["results"]["root//:target1"]["configured"][
        "<unspecified>"
    ]
    assert "retained_analysis_memory_sketch" not in configured_entry, (
        "Sketch should not be present when config is disabled"
    )
