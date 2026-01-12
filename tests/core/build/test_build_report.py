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

from buck2.tests.e2e_util.api.buck import Buck
from buck2.tests.e2e_util.buck_workspace import buck_test
from buck2.tests.e2e_util.helper.golden import golden
from buck2.tests.e2e_util.helper.utils import replace_digest, replace_hash


def build_report_test(name: str, command: list[str]) -> None:
    async def impl(buck: Buck, tmp_path: Path) -> None:
        report = tmp_path / "build-report.json"
        await buck.build("--build-report", str(report), *command)

        with open(report) as file:
            report = json.loads(file.read())
        del report["trace_id"]
        del report["project_root"]

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
    "test_build_report_format_configured_graph_unconfigured_sketch",
    [
        "//:rule1",
        "//:dir1",
        "//subdir:rule",
        "-c",
        "buck2.log_configured_graph_unconfigured_sketch=true",
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
    "test_build_report_format_total_configured_graph_unconfigured_sketch",
    [
        "//:rule1",
        "//:dir1",
        "//subdir:rule",
        "-c",
        "buck2.log_total_configured_graph_unconfigured_sketch=true",
    ],
)

build_report_test(
    "test_build_report_format_per_configuration_sketch",
    [
        "//:rule1",
        "//:dir1",
        "//subdir:rule",
        # Let's look at something more interesting than unspecified platform
        "--target-platforms=root//:platform",
        "-c",
        "buck2.log_total_per_configuration_sketch=true",
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
        "buck2.log_configured_graph_unconfigured_sketch=true",
        "-c",
        "buck2.log_total_configured_graph_sketch=true",
        "-c",
        "buck2.log_total_configured_graph_unconfigured_sketch=true",
        "-c",
        "buck2.log_total_per_configuration_sketch=true",
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

        rule2_metrics = report["results"]["root//:rule2"]["configured"][
            "<unspecified>"
        ]["build_metrics"]
        assert rule2_metrics["action_graph_size"] == 1
        assert rule2_metrics["metrics"]["declared_actions"] == 4
        assert rule2_metrics["amortized_metrics"]["declared_actions"] == 3
        assert report["build_metrics"]


def streaming_build_report_test(name: str, command: list[str]) -> None:
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

        with open(report) as file:
            lines = file.read().strip().split("\n")
            report = json.loads(lines[-1])  # take the last report

        del report["trace_id"]
        del report["project_root"]

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

    # Check that the streaming report file was created
    assert streaming_report.exists(), (
        f"Streaming report file should be created at {streaming_report}"
    )

    # Read and validate the streaming report
    with open(streaming_report) as file:
        lines = file.read().strip().split("\n")
        # Should have at least one JSON line (could have multiple for streaming)
        assert len(lines) == 3, (
            "Streaming report should contain 3 lines, one for each output"
        )

        # Each line should be valid JSON
        for line in lines:
            report_data = json.loads(line)
            # Verify basic structure
            assert "trace_id" in report_data
            assert "success" in report_data
            assert "results" in report_data
            assert "project_root" in report_data


@buck_test()
async def test_streaming_build_report_non_existent_directory(buck: Buck) -> None:
    streaming_report = "non_existent_dir/report"

    await buck.build(
        "//:rule1",
        "--streaming-build-report",
        streaming_report,
    )

    with open(buck.cwd / streaming_report) as file:
        report = json.load(file)
        assert report["success"]


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
