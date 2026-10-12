# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

# pyre-strict


import json
from pathlib import Path
from typing import Any, TypeVar

from buck2.tests.e2e_util.api.buck import Buck
from buck2.tests.e2e_util.asserts import expect_failure
from buck2.tests.e2e_util.buck_workspace import buck_test
from buck2.tests.e2e_util.helper.golden import golden
from buck2.tests.e2e_util.helper.utils import (
    read_streaming_report,
    replace_digest,
    replace_hash,
    replay_streaming_report,
)

K = TypeVar("K")


def _sanitize_timing_fields(obj: Any) -> None:
    """Replace timing-dependent fields with a placeholder to avoid flaky tests."""
    if isinstance(obj, dict):
        for key in list(obj.keys()):
            if key == "compute_time_ms":
                obj[key] = "<COMPUTE_TIME_MS>"
            elif key == "wall_clock_completion_ms":
                obj[key] = "<WALL_CLOCK_COMPLETION_MS>"
            else:
                _sanitize_timing_fields(obj[key])
    elif isinstance(obj, list):
        for item in obj:
            _sanitize_timing_fields(item)


def _collect_output_paths(report: dict[str, Any]) -> set[str]:
    output_paths = set()
    for target_result in report["results"].values():
        for configured_result in target_result["configured"].values():
            for provider_outputs in configured_result.get("outputs", {}).values():
                output_paths.update(provider_outputs)
            for provider_outputs in configured_result.get("other_outputs", {}).values():
                output_paths.update(provider_outputs)
    return output_paths


def _sorted_outputs(entry: dict[str, Any]) -> dict[str, list[str]]:
    return {provider: sorted(paths) for provider, paths in entry["outputs"].items()}


def _first_success(statuses: list[tuple[int, K, str]]) -> dict[K, int]:
    """Line at which each entry first reads `SUCCESS`, checking it never goes back."""
    finished_at: dict[K, int] = {}
    for i, key, status in statuses:
        assert status in ("IN_PROGRESS", "SUCCESS"), (key, status)
        if status == "SUCCESS":
            finished_at.setdefault(key, i)
        else:
            assert key not in finished_at, f"{key} went back to IN_PROGRESS at line {i}"
    return finished_at


def build_report_test(name: str, command: list[str]) -> None:
    async def impl(buck: Buck, tmp_path: Path) -> None:
        report = tmp_path / "build-report.json"
        await buck.build("--build-report", str(report), *command)

        with open(report) as file:
            report = json.loads(file.read())
        del report["trace_id"]
        del report["project_root"]
        _sanitize_timing_fields(report)

        # Build report errors can change based on minor test changes such as
        # 1. Adding a target in TARGETS.fixture
        # 2. Line number changing due to code moving around
        # Sanitize so that we only check the important bits of the error message
        golden(
            output=replace_digest(
                replace_hash(json.dumps(report, indent=2, sort_keys=True))
            ),
            rel_path="fixtures/" + name + ".golden.json",
        )

    globals()[name] = impl

    return buck_test()(impl)


build_report_test(
    "test_build_report_format",
    [
        "//:rule1",
        "//:rule2",
        "//:rule2[out1]",
        "-c",
        "buck2.log_configured_graph_size=true",
    ],
)

build_report_test(
    "test_build_report_format_skip_unconfigured",
    [
        "//:rule1",
        "-c",
        "build_report.print_unconfigured_section=false",
    ],
)

build_report_test(
    "test_build_report_format_package_relative_paths",
    [
        "//:rule1",
        "//subdir:rule",
        "--build-report-options",
        "package-project-relative-paths",
    ],
)


build_report_test(
    "test_build_report_format_artifact_hash_information",
    [
        "//:rule1",
        "//:dir1",
        "//subdir:rule",
        "--build-report-options",
        "include-artifact-hash-information",
    ],
)

build_report_test(
    "test_build_report_format_configured_graph_sketch",
    [
        "//:rule1",
        "//:dir1",
        "//subdir:rule",
        "-c",
        "buck2.log_configured_graph_sketch=true",
    ],
)

build_report_test(
    "test_build_report_format_total_configured_graph_sketch",
    [
        "//:rule1",
        "//:dir1",
        "//subdir:rule",
        "-c",
        "buck2.log_total_configured_graph_sketch=true",
    ],
)

build_report_test(
    "test_build_report_format_action_graph_sketch",
    [
        "//:rule1",
        "//:dir1",
        "//subdir:rule",
        "-c",
        "buck2.log_action_graph_sketch=true",
    ],
)

build_report_test(
    "test_build_report_format_all_sketches",
    [
        "//:rule1",
        "//:dir1",
        "//subdir:rule",
        # Let's look at something more interesting than unspecified platform
        "--target-platforms=root//:platform",
        "-c",
        "buck2.log_configured_graph_sketch=true",
        "-c",
        "buck2.log_total_configured_graph_sketch=true",
        "-c",
        "buck2.log_action_graph_sketch=true",
    ],
)


@buck_test()
async def test_build_report_non_existent_directory(buck: Buck) -> None:
    build_report = "non_existent_dir/report"

    await buck.build(
        "//:rule1",
        "--build-report",
        build_report,
    )

    with open(buck.cwd / build_report) as file:
        report = json.load(file)
        assert report["success"]


@buck_test()
async def test_build_report_contains_metrics(buck: Buck, tmp_path: Path) -> None:
    report = tmp_path / "build-report.json"

    await buck.build(
        "//:rule1",
        "-c",
        "buck2.detailed_aggregated_metrics=true",
        "--build-report",
        str(report),
    )

    with open(report) as file:
        report = json.load(file)
        # Cannot create a golden test since the values may change across runs.
        # Assert that some of the fields are present to ensure that the values are being populated.
        assert report["build_metrics"]
        assert report["build_metrics"]["action_graph_size"] == 1
        assert report["build_metrics"]["metrics"]["declared_actions"] == 2


@buck_test()
async def test_build_report_contains_per_target_build_metrics(
    buck: Buck, tmp_path: Path
) -> None:
    report = tmp_path / "build-report.json"

    await buck.build(
        "//:rule1",
        "//:rule2",
        "-c",
        "buck2.detailed_aggregated_metrics=true",
        "--build-report",
        str(report),
    )

    with open(report) as file:
        report = json.load(file)
        # Cannot create a golden test since the values may change across runs.
        # Assert that some of the fields are present to ensure that the values are being populated.
        rule1_metrics = report["results"]["root//:rule1"]["configured"][
            "<unspecified>"
        ]["build_metrics"]
        assert rule1_metrics["action_graph_size"] == 1
        assert rule1_metrics["metrics"]["declared_actions"] == 2
        assert rule1_metrics["amortized_metrics"]["declared_actions"] == 1

        # re_platform_names should be absent for local-only actions (skip_serializing_if empty)
        assert "re_platform_names" not in rule1_metrics

        # wall_clock_completion_ms should be present for completed targets
        assert "wall_clock_completion_ms" in rule1_metrics
        assert rule1_metrics["wall_clock_completion_ms"] > 0

        rule2_metrics = report["results"]["root//:rule2"]["configured"][
            "<unspecified>"
        ]["build_metrics"]
        assert rule2_metrics["action_graph_size"] == 1
        assert rule2_metrics["metrics"]["declared_actions"] == 4
        assert rule2_metrics["amortized_metrics"]["declared_actions"] == 3
        assert "re_platform_names" not in rule2_metrics
        assert report["build_metrics"]


def streaming_build_report_test(golden_name: str, command: list[str]) -> None:
    async def impl(buck: Buck, tmp_path: Path) -> None:
        base_report = tmp_path / "build-report.json"
        report = tmp_path / "streaming-build-report.json"

        await buck.build(
            "--build-report",
            str(base_report),
            "--streaming-build-report",
            str(report),
            *command,
        )

        reports = read_streaming_report(report)
        # Each line carries one target, so the whole stream is what adds up to the report.
        report = reports[-1]
        report["results"] = replay_streaming_report(reports)
        report["strings"] = {
            key: string for line in reports for key, string in line["strings"].items()
        }

        del report["trace_id"]
        del report["project_root"]
        _sanitize_timing_fields(report)

        # Build report errors can change based on minor test changes such as
        # 1. Adding a target in TARGETS.fixture
        # 2. Line number changing due to code moving around
        # Sanitize so that we only check the important bits of the error message
        golden(
            output=replace_digest(
                replace_hash(json.dumps(report, indent=2, sort_keys=True))
            ),
            rel_path="fixtures/" + golden_name + ".golden.json",
        )

    # The golden belongs to the `build_report_test` of the same name. Registering this test
    # under that name would replace it.
    name = golden_name.replace("test_build_report", "test_streaming_build_report", 1)
    globals()[name] = impl

    return buck_test()(impl)


streaming_build_report_test(
    "test_build_report_format_skip_unconfigured",
    [
        "//:rule1",
        "-c",
        "build_report.print_unconfigured_section=false",
    ],
)

streaming_build_report_test(
    "test_build_report_format_package_relative_paths",
    [
        "//:rule1",
        "//subdir:rule",
        "--build-report-options",
        "package-project-relative-paths",
    ],
)

streaming_build_report_test(
    "test_build_report_format_artifact_hash_information",
    [
        "//:rule1",
        "//:dir1",
        "//subdir:rule",
        "--build-report-options",
        "include-artifact-hash-information",
    ],
)


@buck_test()
async def test_streaming_build_report(buck: Buck, tmp_path: Path) -> None:
    """Test that --streaming-build-report creates a streaming report file with JSON lines."""
    streaming_report = tmp_path / "streaming-report.jsonl"

    await buck.build(
        "//:rule1",
        "//:rule2",
        "//:rule2[out2]",
        "--streaming-build-report",
        str(streaming_report),
    )

    reports = read_streaming_report(streaming_report)
    for report in reports:
        assert {"trace_id", "success", "results", "project_root"} <= report.keys()
        assert len(report["results"]) == 1, (
            f"Expected one target per line, got {sorted(report['results'])}"
        )
    # One line per output, plus one each when a configured target and a target finish.
    # `//:rule2` and `//:rule2[out2]` are a single target.
    assert len(reports) == 7, f"Expected 7 lines, got {len(reports)}"
    results = replay_streaming_report(reports)
    assert {
        Path(path).name for path in _collect_output_paths({"results": results})
    } == {"out.txt", "out1.txt", "out2.txt"}


@buck_test()
async def test_streaming_build_report_finishes_missing_target(
    buck: Buck, tmp_path: Path
) -> None:
    """A missing target has no outputs, so only its build-finished line reports it."""
    streaming_report = tmp_path / "streaming-report.jsonl"

    await expect_failure(
        buck.build(
            "//:does_not_exist", "--streaming-build-report", str(streaming_report)
        )
    )

    reports = read_streaming_report(streaming_report)
    assert len(reports) == 1, f"Expected 1 line, got {len(reports)}"
    assert reports[0]["results"]["root//:does_not_exist"]["success"] == "FAIL"


@buck_test()
async def test_streaming_build_report_in_progress(buck: Buck, tmp_path: Path) -> None:
    """An entry reads `IN_PROGRESS` until its build has finished, then `SUCCESS` for good."""
    streaming_report = tmp_path / "streaming-report.jsonl"
    final_report = tmp_path / "build-report.json"

    await buck.build(
        "//:rule1",
        "//:rule2",
        "//:rule2[out2]",
        "--streaming-build-report",
        str(streaming_report),
        "--build-report",
        str(final_report),
    )

    reports = read_streaming_report(streaming_report)
    final_results = json.loads(final_report.read_text())["results"]

    target_statuses = [
        (i, target, entry["success"])
        for i, report in enumerate(reports)
        for target, entry in report["results"].items()
    ]
    configured_statuses = [
        (i, (target, cfg), configured["success"])
        for i, report in enumerate(reports)
        for target, entry in report["results"].items()
        for cfg, configured in entry["configured"].items()
    ]
    target_finished_at = _first_success(target_statuses)
    configured_finished_at = _first_success(configured_statuses)

    assert any(status == "IN_PROGRESS" for _, _, status in target_statuses), (
        "No target was ever reported IN_PROGRESS"
    )
    assert set(target_finished_at) == set(final_results)
    assert set(configured_finished_at) == {
        (target, cfg)
        for target, entry in final_results.items()
        for cfg in entry["configured"]
    }

    # The first line to report an entry finished already carries everything it built. For
    # `//:rule2` that includes the output of `//:rule2[out2]`, a separate provider label whose
    # build can end before or after the bare label's.
    for target, i in target_finished_at.items():
        assert _sorted_outputs(reports[i]["results"][target]) == _sorted_outputs(
            final_results[target]
        )
    for (target, cfg), i in configured_finished_at.items():
        assert _sorted_outputs(
            reports[i]["results"][target]["configured"][cfg]
        ) == _sorted_outputs(final_results[target]["configured"][cfg])

    assert "IN_PROGRESS" not in final_report.read_text()


@buck_test()
async def test_streaming_build_report_in_progress_without_unconfigured_section(
    buck: Buck, tmp_path: Path
) -> None:
    """Without the unconfigured section, only configured entries have a `success`."""
    streaming_report = tmp_path / "streaming-report.jsonl"

    await expect_failure(
        buck.build(
            "//:rule1",
            "//:does_not_exist",
            "--streaming-build-report",
            str(streaming_report),
            "-c",
            "build_report.print_unconfigured_section=false",
        )
    )

    reports = read_streaming_report(streaming_report)
    entries = [
        (key, entry) for report in reports for key, entry in report["results"].items()
    ]
    assert all("success" not in entry for _, entry in entries), entries

    rule1_history = [
        [configured["success"] for configured in entry["configured"].values()]
        for key, entry in entries
        if key == "root//:rule1"
    ]
    # The output's line, then the configured target finishing, then the target.
    assert rule1_history == [["IN_PROGRESS"], ["SUCCESS"], ["SUCCESS"]], rule1_history

    missing = [entry for key, entry in entries if key == "root//:does_not_exist"]
    assert missing, "Expected a line for the missing target"
    assert all(entry["configured"] == {} and entry["errors"] for entry in missing)


@buck_test()
async def test_streaming_build_report_non_existent_directory(buck: Buck) -> None:
    streaming_report = "non_existent_dir/report"

    await buck.build(
        "//:rule1",
        "--streaming-build-report",
        streaming_report,
    )

    reports = read_streaming_report(buck.cwd / streaming_report)
    assert reports, "Expected the streaming report to be written"
    assert all(report["success"] for report in reports)


@buck_test()
async def test_streaming_build_report_overwrites_existing_file(
    buck: Buck, tmp_path: Path
) -> None:
    streaming_report = tmp_path / "streaming-report.jsonl"

    with open(streaming_report, "w") as file:
        file.write("Not valid JSON!")

    await buck.build(
        "//:rule1",
        "--streaming-build-report",
        str(streaming_report),
    )

    # Read and validate the streaming report
    with open(streaming_report) as file:
        lines = file.read().strip().split("\n")

        # Each line should be valid JSON
        for line in lines:
            report_data = json.loads(line)
            # Verify basic structure
            assert "trace_id" in report_data
            assert "success" in report_data
            assert "results" in report_data
            assert "project_root" in report_data


@buck_test(data_dir="re_platform_names")
async def test_build_report_re_platform_names(buck: Buck, tmp_path: Path) -> None:
    report = tmp_path / "build-report.json"

    await buck.build(
        "//:run_action",
        "-c",
        "buck2.detailed_aggregated_metrics=true",
        "--build-report",
        str(report),
    )

    with open(report) as file:
        report_data = json.load(file)
        metrics = report_data["results"]["root//:run_action"]["configured"][
            "<unspecified>"
        ]["build_metrics"]
        assert "re_platform_names" in metrics
        assert "linux-remote-execution" in metrics["re_platform_names"]
