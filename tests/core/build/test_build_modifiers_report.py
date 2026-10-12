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
from typing import Any

from buck2.tests.e2e_util.api.buck import Buck
from buck2.tests.e2e_util.asserts import expect_failure
from buck2.tests.e2e_util.buck_workspace import buck_test
from buck2.tests.e2e_util.helper.golden import (
    golden,
    GOLDEN_DIRECTORY,
    sanitize_build_report,
    sanitize_hashes,
)
from buck2.tests.e2e_util.helper.utils import (
    read_streaming_report,
    replay_streaming_report,
    streaming_status_history,
)


def build_report_test(
    name: str,
    command: list[str],
    expect_error: bool = False,
) -> None:
    async def impl(buck: Buck, tmp_path: Path) -> None:
        report = tmp_path / "build-report.json"
        command.extend(["--build-report", str(report)])

        if expect_error:
            command.extend(["--build-report-options", "fill-out-failures"])
            await expect_failure(buck.build(*command))
        else:
            await buck.build(*command)

        with open(report) as file:
            report = json.loads(file.read())

        sanitize_build_report(report)

        golden(
            output=sanitize_hashes(json.dumps(report, indent=2, sort_keys=True)),
            rel_path=GOLDEN_DIRECTORY + name + ".golden.json",
        )

    globals()[name] = impl

    return buck_test()(impl)


build_report_test(
    "test_build_modifiers_report",
    [
        "root//:target?root//:macos+root//:arm",
        "root//:target?root//:linux",
    ],
)

build_report_test(
    "test_build_modifiers_report_error_failures_includes_modifiers",
    ["root//fail:fail?root//:macos+root//:arm"],
    expect_error=True,
)

build_report_test(
    "test_build_modifiers_report_package_pattern",
    ["root//:?root//:macos"],
)

build_report_test(
    "test_build_modifiers_report_recursive_pattern",
    ["root//...?root//:macos"],
    expect_error=True,
)

build_report_test(
    "test_build_modifiers_report_ambiguous_pattern",
    [
        "root//subdir?root//:macos",
        "root//subdir:subdir?root//:linux",
        "root//subdir:subdir?root//:macos",
    ],
)

build_report_test(
    "test_build_modifiers_report_deduplication",
    [
        "root//:?root//:macos",
        "root//:target2?root//:linux",
        "root//:target?root//:macos",
    ],
)


@buck_test()
async def test_build_modifiers_that_lead_to_same_configured(buck: Buck) -> None:
    mac_first = "root//:target?root//:macos+root//:arm"
    arm_first = "root//:target?root//:arm+root//:macos"

    result = await buck.build(
        mac_first,
        arm_first,
        "-c",
        "buck2.detailed_aggregated_metrics=true",
        "-c",
        "buck2.log_configured_graph_sketch=true",
        "-c",
        "buck2.log_configured_graph_unconfigured_sketch=true",
    )

    report = json.loads(result.stdout)

    mac_configuration = report["results"][mac_first]["configured"]
    arm_configuration = report["results"][arm_first]["configured"]

    assert mac_configuration == arm_configuration


async def _streamed_status_history(
    buck: Buck, tmp_path: Path, *targets: str
) -> dict[str, list[str]]:
    streaming_report = tmp_path / "streaming-build-report.jsonl"
    await buck.build(
        *targets,
        "--streaming-build-report",
        str(streaming_report),
    )
    return streaming_status_history(read_streaming_report(streaming_report))


@buck_test()
async def test_streaming_build_report_target_completion_with_modifiers(
    buck: Buck, tmp_path: Path
) -> None:
    """Each `?modifier` entry finishes when its own request has been built."""
    mac_arm = "root//:target?root//:macos+root//:arm"
    linux = "root//:target?root//:linux"

    assert await _streamed_status_history(buck, tmp_path, mac_arm, linux) == {
        mac_arm: ["IN_PROGRESS", "SUCCESS"],
        linux: ["IN_PROGRESS", "SUCCESS"],
    }


@buck_test()
async def test_streaming_build_report_target_completion_with_modifiers_that_lead_to_same_configured(
    buck: Buck, tmp_path: Path
) -> None:
    """Two entries for one configured target each finish when their own request has."""
    mac_first = "root//:target?root//:macos+root//:arm"
    arm_first = "root//:target?root//:arm+root//:macos"

    assert await _streamed_status_history(buck, tmp_path, mac_first, arm_first) == {
        mac_first: ["IN_PROGRESS", "SUCCESS"],
        arm_first: ["IN_PROGRESS", "SUCCESS"],
    }


def _configured_success(results: dict[str, Any], key: str) -> str | None:
    """The `success` of the entry's only configured target, if it has one yet."""
    configured = results.get(key, {}).get("configured", {})
    assert len(configured) <= 1, f"{key} has several configurations: {list(configured)}"
    return next((entry["success"] for entry in configured.values()), None)


def _has_output(results: dict[str, Any], key: str, name: str) -> bool:
    return any(
        path.endswith(name)
        for configured in results.get(key, {}).get("configured", {}).values()
        for paths in configured["outputs"].values()
        for path in paths
    )


def _first_line_with_output(reports: list[dict[str, Any]], key: str, name: str) -> int:
    lines = [i for i, r in enumerate(reports) if _has_output(r["results"], key, name)]
    assert lines, f"`{name}` for {key} is on no streaming line"
    return lines[0]


@buck_test()
async def test_streaming_build_report_configured_target_finishes_per_request(
    buck: Buck, tmp_path: Path
) -> None:
    """Requests that configure one target each finish their own configured entry."""
    fast = "root//slow:target?root//:macos+root//:arm"
    slow = "root//slow:target?root//:arm+root//:macos"
    streaming_report = tmp_path / "streaming-build-report.jsonl"

    await buck.build(
        fast,
        "root//slow:target[slow]?root//:arm+root//:macos",
        # A cached `slow.txt` could be built before `fast` finishes.
        "--no-remote-cache",
        "--streaming-build-report",
        str(streaming_report),
    )

    reports = read_streaming_report(streaming_report)
    final = replay_streaming_report(reports)
    assert final[fast]["configured"].keys() == final[slow]["configured"].keys()

    # `[slow]` also lists `fast.txt`, so a line is written while `slow.txt` builds.
    slow_built = _first_line_with_output(reports, slow, "slow.txt")
    statuses = [
        (_configured_success(results, fast), _configured_success(results, slow))
        for results in (
            replay_streaming_report(reports[: i + 1]) for i in range(slow_built)
        )
    ]
    assert all(slow_status != "SUCCESS" for _, slow_status in statuses), statuses
    assert ("SUCCESS", "IN_PROGRESS") in statuses, statuses

    assert (_configured_success(final, fast), _configured_success(final, slow)) == (
        "SUCCESS",
        "SUCCESS",
    )


@buck_test()
async def test_streaming_build_report_configured_target_waits_for_every_label(
    buck: Buck, tmp_path: Path
) -> None:
    """A request's labels for one target share a configured entry, which finishes
    only once the last of them has built."""
    key = "root//slow:target?root//:macos"
    streaming_report = tmp_path / "streaming-build-report.jsonl"

    await buck.build(
        key,
        "root//slow:target[slow]?root//:macos",
        # A cached `slow.txt` could be built before the default label finishes.
        "--no-remote-cache",
        "--streaming-build-report",
        str(streaming_report),
    )

    reports = read_streaming_report(streaming_report)
    slow_built = _first_line_with_output(reports, key, "slow.txt")
    statuses = [
        _configured_success(replay_streaming_report(reports[: i + 1]), key)
        for i in range(slow_built)
    ]
    assert statuses, "the default label's output is streamed before `slow.txt`"
    assert all(status != "SUCCESS" for status in statuses), statuses
    assert _configured_success(replay_streaming_report(reports), key) == "SUCCESS"
