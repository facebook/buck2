# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

# pyre-strict


import json
import os.path
from pathlib import Path

import pytest
from buck2.tests.e2e_util.api.buck import Buck
from buck2.tests.e2e_util.buck_workspace import buck_test
from buck2.tests.e2e_util.helper.utils import is_running_on_windows


@buck_test()
async def test_log_show_invocation_record(buck: Buck, tmp_path: Path) -> None:
    mode_file = tmp_path / "mode"
    mode_file.write_text("-c\naa.bb=cc\n-c\ndd.ee=ff\n")

    # Any simple would do.
    await buck.uquery(f"@{mode_file}", "//:EEE")

    result = await buck.log("show")
    invocation = json.loads(result.stdout.splitlines()[0])
    command_line_args = invocation["command_line_args"]
    expanded_command_line_args = invocation["expanded_command_line_args"]
    assert f"@{mode_file}" in command_line_args
    assert f"@{mode_file}" not in expanded_command_line_args
    assert "aa.bb=cc" in expanded_command_line_args
    assert "aa.bb=cc" not in command_line_args


@buck_test(write_invocation_record=True)
async def test_log_size_logging(buck: Buck) -> None:
    res = await buck.cquery(
        "//:EEE",
    )

    out = await buck.log("last")
    path = out.stdout.strip()
    with open(path, "rb") as f:
        log_size_in_disk = len(f.read())

    logged_size = res.invocation_record()["compressed_event_log_size_bytes"]

    assert logged_size == log_size_in_disk


@buck_test()
async def test_last_log(buck: Buck) -> None:
    await buck.build("//:EEE")
    out = await buck.log("last")
    path = out.stdout.strip()
    assert os.path.exists(path)
    assert "/log/" in path or "\\log\\" in path
    out2 = await buck.log("path")
    assert path == out2.stdout.strip()


@buck_test()
async def test_last_log_all(buck: Buck) -> None:
    await buck.build("//:EEE")
    out = await buck.log("last", "--all")
    paths = list(out.stdout.splitlines())
    assert len(paths) > 0
    for path in paths:
        assert os.path.exists(path)
        assert "/log/" in path or "\\log\\" in path


@buck_test()
async def test_log_command_with_trace_id(buck: Buck, tmp_path: Path) -> None:
    build_file_path = tmp_path / "b"
    await buck.uquery("//:", f"--write-build-id={build_file_path}")
    build_id = build_file_path.read_text("utf-8").strip()
    await buck.log("show", f"--trace-id={build_id}")
    log = (await buck.log("show", f"--trace-id={build_id}")).stdout.strip().splitlines()
    # Check it looks like log.
    assert len(log) >= 1
    for line in log:
        json.loads(line)


@buck_test()
async def test_what_buck(buck: Buck, tmp_path: Path) -> None:
    mode_path = tmp_path / "mode"
    mode_path.write_text("-c\nxx.yy=zz\n")

    await buck.uquery("//:", f"@{mode_path}")

    out = await buck.log("what-cmd")
    assert "uquery //: " in out.stdout
    if not is_running_on_windows():
        # Path is quoted on Windows.
        assert f"uquery //: @{mode_path}" in out.stdout

    out = await buck.log("what-cmd", "--expand")
    assert "uquery //: -c" in out.stdout


@buck_test()
@pytest.mark.parametrize(
    "snapshots,expected_re,expected_http",
    [
        pytest.param(
            [(100_000, 200_000), (100_100, 200_200), (100_300, 200_500)],
            300,
            500,
            id="nonzero_baseline",
        ),
        pytest.param([(0, 0), (300, 500)], 300, 500, id="zero_baseline"),
        pytest.param([(100_000, 200_000), (100_000, 200_000)], 0, 0, id="no_downloads"),
        pytest.param([], 0, 0, id="no_snapshots"),
        pytest.param([(100_000, 200_000)], 0, 0, id="one_snapshot"),
    ],
)
async def test_log_summary_downloads(
    buck: Buck,
    tmp_path: Path,
    snapshots: list[tuple[int, int]],
    expected_re: int,
    expected_http: int,
) -> None:
    await buck.uquery("//:EEE")
    log = [json.loads(line) for line in (await buck.log("show")).stdout.splitlines()]
    snapshot_event = next(
        event
        for event in log[1:]
        if "Snapshot"
        in event.get("Event", {}).get("data", {}).get("Instant", {}).get("data", {})
    )
    snapshot = snapshot_event["Event"]["data"]["Instant"]["data"]["Snapshot"]
    log_path = tmp_path / "summary.json-lines"
    with log_path.open("w") as output:
        print(json.dumps(log[0]), file=output)
        for index, (re_bytes, http_bytes) in enumerate(snapshots):
            snapshot_event["Event"]["timestamp"] = [index + 1, 0]
            snapshot["re_download_bytes"] = re_bytes
            snapshot["http_download_bytes"] = http_bytes
            print(json.dumps(snapshot_event), file=output)

    result = await buck.log("summary", str(log_path))

    assert [line for line in result.stdout.splitlines() if "downloaded:" in line] == [
        f"- Total downloaded: {expected_re + expected_http}B",
        f"  - RE downloaded: {expected_re}B",
        f"  - HTTP downloaded: {expected_http}B",
    ]
