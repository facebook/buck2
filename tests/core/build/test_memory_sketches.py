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


def _get_sketch_cardinality(sketch_str: str) -> float:
    """Get the cardinality of a single sketch string."""
    if not sketch_str.startswith("V1:"):
        raise ValueError(f"Unexpected sketch version: {sketch_str[:10]}")
    base64_str = sketch_str[3:]
    sketch_size_bin = os.environ["SKETCH_SIZE_BIN"]
    result = subprocess.run(
        [sketch_size_bin],
        input=base64_str,
        capture_output=True,
        text=True,
        check=True,
    )
    return float(result.stdout.strip())


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


def check_approx(actual: float, expected: float, tolerance: float = 0.10) -> None:
    """
    Verify that actual value matches expected within a tolerance (default 10%).
    """
    lower_bound = expected * (1 - tolerance)
    upper_bound = expected * (1 + tolerance)

    assert lower_bound <= actual <= upper_bound, (
        f"Value {actual:.0f} should be "
        f"between {lower_bound:.0f} and {upper_bound:.0f} (expected ~{expected:,.0f})"
    )


async def _build_and_extract_sketches(
    buck: Buck,
    tmp_path: Path,
    targets: list[str],
    sketch_field: str,
    config_key: str,
) -> dict[str, str]:
    """
    Build targets with a sketch config enabled and extract sketches from the build report.

    Args:
        sketch_field: The field name in the build report (e.g., "retained_analysis_memory_sketch")
        config_key: The buckconfig key to enable (e.g., "buck2.log_retained_analysis_memory_sketch")
    """
    report = tmp_path / "build-report.json"

    # Build with sketch enabled
    await buck.build(
        *targets,
        "-c",
        f"{config_key}=true",
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
            sketch_field
        ]
        assert sketch is not None, f"Sketch should be present for {full_target}"
        assert sketch.startswith("V1:"), (
            f"Sketch should have V1 version prefix for {full_target}"
        )
        sketches[target] = sketch

    return sketches


async def _get_retained_sketches(
    buck: Buck, tmp_path: Path, targets: list[str]
) -> dict[str, str]:
    return await _build_and_extract_sketches(
        buck,
        tmp_path,
        targets,
        "retained_analysis_memory_sketch",
        "buck2.log_retained_analysis_memory_sketch",
    )


async def _get_analysis_peak_sketches(
    buck: Buck, tmp_path: Path, targets: list[str]
) -> dict[str, str]:
    return await _build_and_extract_sketches(
        buck,
        tmp_path,
        targets,
        "peak_analysis_memory_sketch",
        "buck2.log_peak_analysis_memory_sketch",
    )


async def _get_load_peak_sketches(
    buck: Buck, tmp_path: Path, targets: list[str]
) -> dict[str, str]:
    return await _build_and_extract_sketches(
        buck,
        tmp_path,
        targets,
        "peak_load_memory_sketch",
        "buck2.log_peak_load_memory_sketch",
    )


# =============================================================================
# Retained analysis memory sketch tests
# =============================================================================


@buck_test()
async def test_retained_analysis_memory_sketch(buck: Buck, tmp_path: Path) -> None:
    """
    Test that retained_analysis_memory_sketch is computed correctly.

    We use a custom rule that allocates memory in analysis,
    then verify the sketch cardinality matches the expected amount.
    """
    # target1 allocates 1M + shared_dep allocates 5M = ~6M total
    sketches = await _get_retained_sketches(buck, tmp_path, ["//:target1"])
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
    sketches = await _get_retained_sketches(
        buck, tmp_path, ["//:target1", "//:target2"]
    )

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
    sketches = await _get_retained_sketches(buck, tmp_path, ["//:target_with_globals"])

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
    sketches1 = await _get_retained_sketches(buck, tmp_path, ["//:target1"])

    # Kill daemon
    await buck.kill()

    # Second build after restart
    sketches2 = await _get_retained_sketches(buck, tmp_path, ["//:target1"])

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
    sketches = await _get_retained_sketches(buck, tmp_path, ["//:target_with_anon"])

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


# =============================================================================
# Analysis memory peak sketch tests
# =============================================================================


@buck_test()
async def test_peak_analysis_memory_sketch(buck: Buck, tmp_path: Path) -> None:
    """
    Test that peak_analysis_memory_sketch is computed correctly.

    Peak memory should be >= retained memory. For these simple targets that
    allocate and retain all memory, peak ~= retained.
    """
    # target1 allocates 1M + shared_dep allocates 5M = ~6M total
    sketches = await _get_analysis_peak_sketches(buck, tmp_path, ["//:target1"])
    check_merged_cardinality([sketches["//:target1"]], 6_000_000)


@buck_test()
async def test_peak_analysis_memory_sketch_merging(buck: Buck, tmp_path: Path) -> None:
    """
    Test that peak sketch merging works correctly.

    Build two targets that share a dependency, then verify that merging
    their sketches gives the correct combined cardinality.
    """
    sketches = await _get_analysis_peak_sketches(
        buck, tmp_path, ["//:target1", "//:target2"]
    )

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
async def test_peak_analysis_memory_sketch_disabled(buck: Buck, tmp_path: Path) -> None:
    """Test that peak sketch is not present when config is disabled."""
    report = tmp_path / "build-report.json"

    # Build without peak sketch config
    await buck.build(
        "//:target1",
        "--build-report",
        str(report),
    )

    # Read build report
    with open(report) as f:
        report_data = json.load(f)

    # Verify peak sketch field is absent
    configured_entry = report_data["results"]["root//:target1"]["configured"][
        "<unspecified>"
    ]
    assert "peak_analysis_memory_sketch" not in configured_entry, (
        "Peak sketch should not be present when config is disabled"
    )


@buck_test()
async def test_peak_analysis_memory_sketch_gte_retained(
    buck: Buck, tmp_path: Path
) -> None:
    """
    Test that peak sketch cardinality >= retained sketch cardinality.

    Peak memory includes all allocations during evaluation (including
    temporaries), while retained only includes what survives freezing.
    """
    report = tmp_path / "build-report.json"

    await buck.build(
        "//:target1",
        "-c",
        "buck2.log_peak_analysis_memory_sketch=true",
        "-c",
        "buck2.log_retained_analysis_memory_sketch=true",
        "--build-report",
        str(report),
    )

    with open(report) as f:
        report_data = json.load(f)

    entry = report_data["results"]["root//:target1"]["configured"]["<unspecified>"]
    peak_sketch = entry["peak_analysis_memory_sketch"]
    retained_sketch = entry["retained_analysis_memory_sketch"]

    assert peak_sketch is not None
    assert retained_sketch is not None

    peak_card = _get_sketch_cardinality(peak_sketch)
    retained_card = _get_sketch_cardinality(retained_sketch)

    assert peak_card >= retained_card * 0.95, (
        f"Peak cardinality ({peak_card:.0f}) should be >= "
        f"retained cardinality ({retained_card:.0f})"
    )


@buck_test()
async def test_analysis_memory_peak_captures_temporaries(
    buck: Buck, tmp_path: Path
) -> None:
    """
    Test that peak sketch captures temporary memory that is NOT retained.

    target_peak_only allocates 4MB of temporary memory (not stored in any
    provider) and retains only 1MB. The peak sketch should be significantly
    larger than the retained sketch because it includes the temporaries.
    """
    report = tmp_path / "build-report.json"

    await buck.build(
        "//:target_peak_only",
        "-c",
        "buck2.log_peak_analysis_memory_sketch=true",
        "-c",
        "buck2.log_retained_analysis_memory_sketch=true",
        "--build-report",
        str(report),
    )

    with open(report) as f:
        report_data = json.load(f)

    entry = report_data["results"]["root//:target_peak_only"]["configured"][
        "<unspecified>"
    ]
    peak_sketch = entry["peak_analysis_memory_sketch"]
    retained_sketch = entry["retained_analysis_memory_sketch"]

    assert peak_sketch is not None
    assert retained_sketch is not None

    peak_card = _get_sketch_cardinality(peak_sketch)
    retained_card = _get_sketch_cardinality(retained_sketch)

    # Peak should be significantly larger than retained because of the
    # 4MB of temporary allocations that are dropped at freeze time.
    # The temp allocation is 4x the retained allocation, so peak should
    # be at least 2x retained even accounting for overhead differences.
    assert peak_card > retained_card * 2, (
        f"Peak cardinality ({peak_card:.0f}) should be significantly larger than "
        f"retained cardinality ({retained_card:.0f}) due to temporary allocations"
    )


# =============================================================================
# Load memory peak sketch tests
# =============================================================================


@buck_test()
async def test_peak_load_memory_sketch(buck: Buck, tmp_path: Path) -> None:
    """
    Test that peak_load_memory_sketch is computed and non-empty.

    The load peak sketch captures peak memory from loading .bzl files.
    The rules.bzl file is loaded during analysis, so the sketch should
    have a non-zero cardinality.
    """
    sketches = await _get_load_peak_sketches(buck, tmp_path, ["//:target1"])
    # The bzl files are small, but loading them still allocates some memory.
    assert _get_sketch_cardinality(sketches["//:target1"]) > 0


@buck_test()
async def test_peak_load_memory_sketch_with_globals(buck: Buck, tmp_path: Path) -> None:
    """
    Test that load peak sketch reflects memory from bzl file globals.

    rules_with_globals.bzl allocates ~1MB of global data at load time.
    This should be captured in the load peak sketch.
    """
    sketches = await _get_load_peak_sketches(buck, tmp_path, ["//:target_with_globals"])
    # The bzl file allocates ~1MB of globals at load time
    assert _get_sketch_cardinality(sketches["//:target_with_globals"]) >= 500_000


@buck_test()
async def test_peak_load_memory_sketch_disabled(buck: Buck, tmp_path: Path) -> None:
    """Test that load peak sketch is not present when config is disabled."""
    report = tmp_path / "build-report.json"

    # Build without load peak sketch config
    await buck.build(
        "//:target1",
        "--build-report",
        str(report),
    )

    # Read build report
    with open(report) as f:
        report_data = json.load(f)

    # Verify load peak sketch field is absent
    configured_entry = report_data["results"]["root//:target1"]["configured"][
        "<unspecified>"
    ]
    assert "peak_load_memory_sketch" not in configured_entry, (
        "Load peak sketch should not be present when config is disabled"
    )


@buck_test()
async def test_load_and_analysis_peak_sketches(buck: Buck, tmp_path: Path) -> None:
    """Test that load and analysis peak sketches separate load/analysis memory."""
    report = tmp_path / "build-report.json"

    await buck.build(
        "//:target_with_load_memory",
        "//:target1",
        "-c",
        "buck2.log_peak_load_memory_sketch=true",
        "-c",
        "buck2.log_peak_analysis_memory_sketch=true",
        "--build-report",
        str(report),
    )

    with open(report) as f:
        report_data = json.load(f)

    # Load includes all package .bzl imports (~2M from rules_for_load_test +
    # rules_with_globals); analysis only refs rules_for_load_test (~1M). Diff ≈ 1M.
    load_entry = report_data["results"]["root//:target_with_load_memory"]["configured"][
        "<unspecified>"
    ]
    load_memory_peak = _get_sketch_cardinality(load_entry["peak_load_memory_sketch"])
    analysis_memory_peak = _get_sketch_cardinality(
        load_entry["peak_analysis_memory_sketch"]
    )
    check_approx(load_memory_peak - analysis_memory_peak, 1_000_000)

    # Analysis ≈ 6M (1M target1 + 5M shared_dep). Load includes ~2M from
    # .bzl globals not referenced by target1's analysis. Diff ≈ 4M.
    analysis_entry = report_data["results"]["root//:target1"]["configured"][
        "<unspecified>"
    ]
    target1_analysis_memory_peak = _get_sketch_cardinality(
        analysis_entry["peak_analysis_memory_sketch"]
    )
    target1_load_memory_peak = _get_sketch_cardinality(
        analysis_entry["peak_load_memory_sketch"]
    )
    check_approx(target1_analysis_memory_peak - target1_load_memory_peak, 4_000_000)
