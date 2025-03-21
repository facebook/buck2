# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

# pyre-strict


import json

from buck2.tests.e2e_util.api.buck import Buck
from buck2.tests.e2e_util.buck_workspace import buck_test
from buck2.tests.e2e_util.helper.utils import replace_hashes


@buck_test()
async def test_build_report_format(buck: Buck) -> None:
    await buck.build(
        "//:rule1",
        "//:rule2",
        "--build-report",
        "report",
        "//:rule2[out1]",
        "-c",
        "buck2.log_configured_graph_size=true",
    )
    with open(buck.cwd / "report") as file:
        report = json.load(file)

        assert report["success"]
        assert report["failures"] == {}

        results = report["results"]

        rule1 = results["root//:rule1"]
        assert replace_hashes(rule1["outputs"]["DEFAULT"]) == [
            "buck-out/v2/gen/root/<HASH>/__rule1__/out.txt"
        ]
        assert rule1["other_outputs"] == {}
        rule1_configured = rule1["configured"]["<unspecified>"]
        assert rule1_configured["success"] == "SUCCESS"
        assert replace_hashes(rule1_configured["outputs"]["DEFAULT"]) == [
            "buck-out/v2/gen/root/<HASH>/__rule1__/out.txt"
        ]
        assert rule1_configured["other_outputs"] == {}

        assert rule1["configured_graph_size"] == 2
        assert rule1_configured["configured_graph_size"] == 2

        rule2 = results["root//:rule2"]
        assert rule2["success"] == "SUCCESS"
        assert replace_hashes(rule2["outputs"]["DEFAULT"]) == [
            "buck-out/v2/gen/root/<HASH>/__rule2__/out1.txt"
        ]
        assert replace_hashes(rule2["outputs"]["out1"]) == [
            "buck-out/v2/gen/root/<HASH>/__rule2__/out1.txt"
        ]
        assert rule2["other_outputs"] == {}

        rule2_configured = rule2["configured"]["<unspecified>"]
        assert rule2_configured["success"] == "SUCCESS"
        assert replace_hashes(rule2_configured["outputs"]["DEFAULT"]) == [
            "buck-out/v2/gen/root/<HASH>/__rule2__/out1.txt"
        ]
        assert replace_hashes(rule2_configured["outputs"]["out1"]) == [
            "buck-out/v2/gen/root/<HASH>/__rule2__/out1.txt"
        ]
        assert rule2_configured["other_outputs"] == {}

        assert rule2["configured_graph_size"] == 3
        assert rule2_configured["configured_graph_size"] == 3


@buck_test()
async def test_build_report_format_skip_unconfigured(buck: Buck) -> None:
    await buck.build(
        "//:rule1",
        "--build-report",
        "report",
        "-c",
        "build_report.print_unconfigured_section=false",
    )
    with open(buck.cwd / "report") as file:
        report = json.load(file)

        assert report["success"]
        assert report["failures"] == {}

        results = report["results"]

        rule1 = results["root//:rule1"]
        assert "success" not in rule1
        assert "outputs" not in rule1
        assert "other_outputs" not in rule1
        rule1_configured = rule1["configured"]["<unspecified>"]
        assert rule1_configured["success"] == "SUCCESS"
        assert replace_hashes(rule1_configured["outputs"]["DEFAULT"]) == [
            "buck-out/v2/gen/root/<HASH>/__rule1__/out.txt"
        ]
        assert rule1_configured["other_outputs"] == {}


@buck_test()
async def test_build_report_package_project_relative_path(buck: Buck) -> None:
    await buck.build(
        "//:rule1",
        "//subdir:rule",
        "--build-report",
        "report",
    )

    with open(buck.cwd / "report") as file:
        results = json.load(file)["results"]
        assert "package_project_relative_path" not in results["root//:rule1"]
        assert "package_project_relative_path" not in results["root//subdir:rule"]

    await buck.build(
        "//:rule1",
        "//subdir:rule",
        "--build-report",
        "report",
        "--build-report-options",
        "package-project-relative-paths",
    )

    with open(buck.cwd / "report") as file:
        results = json.load(file)["results"]
        assert results["root//:rule1"]["package_project_relative_path"] == ""
        assert results["root//subdir:rule"]["package_project_relative_path"] == "subdir"


@buck_test()
async def test_build_report_include_artifact_hash_information(buck: Buck) -> None:
    await buck.build(
        "//:rule1",
        "--build-report",
        "report",
    )

    with open(buck.cwd / "report") as file:
        results = json.load(file)["results"]
        assert "artifact_info" not in results["root//:rule1"]["configured"]

    await buck.build(
        "//:rule1",
        "//:dir1",
        "--build-report",
        "report",
        "--build-report-options",
        "include-artifact-hash-information",
    )

    with open(buck.cwd / "report") as file:
        results = json.load(file)["results"]
        rule1 = results["root//:rule1"]
        artifact_info = rule1["configured"]["<unspecified>"]["artifact_info"]["DEFAULT"]
        assert artifact_info["kind"] == "file"
        assert artifact_info["digest"] == "da39a3ee5e6b4b0d3255bfef95601890afd80709:0"

        dir1 = results["root//:dir1"]
        artifact_info = dir1["configured"]["<unspecified>"]["artifact_info"]["DEFAULT"]
        assert artifact_info["kind"] == "directory"
        assert artifact_info["digest"] in [
            "b6ab192957f7876a3e8a02bb140ad221c0c087bd:55",  # unix
            "4b24c991c1b6f0461cab9da9410ece56bd129ed6:57",  # windows
        ]


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
