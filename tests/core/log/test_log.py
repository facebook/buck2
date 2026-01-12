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

from buck2.tests.e2e_util.api.buck import Buck
from buck2.tests.e2e_util.buck_workspace import buck_test
from buck2.tests.e2e_util.helper.utils import (
    is_running_on_windows,
    read_invocation_record,
)


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


@buck_test()
async def test_log_size_logging(buck: Buck, tmp_path: Path) -> None:
    record_file = tmp_path / "record.json"
    await buck.cquery(
        "//:EEE",
        "--unstable-write-invocation-record",
        str(record_file),
    )

    out = await buck.log("last")
    path = out.stdout.strip()
    with open(path, "rb") as f:
        log_size_in_disk = len(f.read())

    logged_size = read_invocation_record(record_file)["compressed_event_log_size_bytes"]

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
