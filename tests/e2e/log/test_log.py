# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

# pyre-strict


import csv
import json
import os.path
import sys
from pathlib import Path

from buck2.tests.e2e_util.api.buck import Buck
from buck2.tests.e2e_util.asserts import expect_failure
from buck2.tests.e2e_util.buck_workspace import buck_test

from buck2.tests.e2e_util.helper.utils import is_running_on_windows


# builds targets in an fbcode target configuration, unsupported on mac RE workers
def fbcode_linux_only() -> bool:
    return sys.platform == "linux"


@buck_test(inplace=False)
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


# TODO(marwhal): Fix and enable on Windows
@buck_test(inplace=True, skip_for_os=["windows"])
async def test_what_ran_json_target_without_explicit_test_cases(buck: Buck) -> None:
    await buck.test("fbcode//buck2/tests/targets/rules/sh_test:test")
    out = await buck.log("what-ran", "--format", "json")
    out = [line.strip() for line in out.stdout.splitlines()]
    out = [json.loads(line) for line in out if line]
    assert len(out) == 1, "out should have 1 line: `{}`".format(out)

    repro = out[0]
    assert repro["reason"] == "test.run"
    assert repro["identity"] == "buck2/tests/targets/rules/sh_test:test"
    assert repro["reproducer"]["executor"] == "Local"
    assert repro["reproducer"]["details"]["command"][1] == "arg1"
    assert repro["extra"]["testcases"] == []


@buck_test(inplace=False)
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

    with open(record_file) as f:
        record = json.load(f)
    logged_size = record["data"]["Record"]["data"]["InvocationRecord"][
        "compressed_event_log_size_bytes"
    ]

    assert logged_size == log_size_in_disk


@buck_test(inplace=False)
async def test_replay(buck: Buck) -> None:
    await buck.build("//:EEE")
    replay = await buck.log("replay", "-v2")
    assert "//:EEE" in replay.stderr


if fbcode_linux_only():

    @buck_test(inplace=True)
    async def test_what_ran(buck: Buck) -> None:
        await buck.build("fbcode//buck2/tests/targets/rules/cxx/hello_world:welcome")
        out = await buck.log("what-ran")
        assert "welcome" in out.stdout

        await buck.test("fbcode//buck2/tests/targets/rules/sh_test:test")
        out = await buck.log("what-ran")
        assert "sh_test/test.py arg1" in out.stdout

    @buck_test(inplace=True)
    async def test_what_ran_filter_category(buck: Buck) -> None:
        await buck.build("fbsource//fbobjc/buck2/samples/hello_world:HelloWorldBundle")
        out = await buck.log(
            "what-ran",
            "--filter-category",
            ".*cxx.*",
            "--format",
            "json",
        )
        out = [line.strip() for line in out.stdout.splitlines()]
        out = [json.loads(line) for line in out if line]
        assert any(
            "cxx_link_executable" or "cxx_compile" in rec["identity"] for rec in out
        ), "action identity must contain the filtered category: `{}`".format(out)

    @buck_test(inplace=True)
    async def test_what_ran_show_std_err(buck: Buck) -> None:
        await expect_failure(
            buck.build("fbcode//buck2/tests/targets/rules/genrule/bad/...")
        )
        out = await buck.log("what-ran", "--show-std-err", "--format", "json")
        out = [line.strip() for line in out.stdout.splitlines()]
        out = [json.loads(line) for line in out if line]
        assert any(
            rec["std_err"] == "" or rec["std_err"] == "HELLO_STDERR\n" for rec in out
        ), "we should have some empty std_errs and also HELLO_STDERR since we echo it in TARGETS: `{}`".format(
            out
        )

        out = await buck.log(
            "what-ran", "--show-std-err", "--omit-empty-std-err", "--format", "json"
        )
        out = [line.strip() for line in out.stdout.splitlines()]
        out = [json.loads(line) for line in out if line]
        assert all(
            rec["std_err"] != "" for rec in out
        ), "we should have no empty std_errs: `{}`".format(out)

    @buck_test(inplace=True)
    async def test_what_ran_json_target_with_test_cases(buck: Buck) -> None:
        await buck.test("fbcode//buck2/tests/targets/rules/go/test:test")
        out = await buck.log("what-ran", "--format", "json")
        out = [line.strip() for line in out.stdout.splitlines()]
        out = [json.loads(line) for line in out if line]
        out = [repro for repro in out if repro.get("reason", "").startswith("test.")]
        assert len(out) == 2, "out should have 2 test lines: `{}`".format(out)

        repros = {repro["reason"]: repro for repro in out}

        # test discovery
        discovery = repros["test.discovery"]
        assert discovery["identity"] == "buck2/tests/targets/rules/go/test:test"
        assert discovery["reproducer"]["executor"] == "Local"

        # test running
        repro = repros["test.run"]
        assert repro["reason"] == "test.run"
        assert repro["identity"] == "buck2/tests/targets/rules/go/test:test"
        assert repro["reproducer"]["executor"] == "Local"
        assert repro["extra"]["testcases"] == ["TestFoo"]

    @buck_test(inplace=True)
    async def test_what_ran_csv_target_with_test_cases(buck: Buck) -> None:
        await buck.test("fbcode//buck2/tests/targets/rules/go/test:test")
        out = await buck.log("what-ran", "--format", "csv")
        out = [line.strip() for line in out.stdout.splitlines()]
        header = ["reason", "identity", "executor", "reproducer"]
        out = [dict(zip(header, record)) for record in csv.reader(out) if record]
        assert out[0] == dict(
            zip(header, header)
        ), "ensure that first entry in csv is the header"
        out = [repro for repro in out if repro.get("reason", "").startswith("test.")]
        assert len(out) == 2, "out should have 2 test lines: `{}`".format(out)

        repros = {repro["reason"]: repro for repro in out}

        # test discovery
        discovery = repros["test.discovery"]
        assert discovery["identity"] == "buck2/tests/targets/rules/go/test:test"

        # test running
        repro = repros["test.run"]
        assert repro["reason"] == "test.run"
        assert repro["identity"] == "buck2/tests/targets/rules/go/test:test"


@buck_test(inplace=False)
async def test_last_log(buck: Buck) -> None:
    await buck.build("//:EEE")
    out = await buck.log("last")
    path = out.stdout.strip()
    assert os.path.exists(path)
    assert "/log/" in path or "\\log\\" in path
    out2 = await buck.log("path")
    assert path == out2.stdout.strip()


@buck_test(inplace=False)
async def test_last_log_all(buck: Buck) -> None:
    await buck.build("//:EEE")
    out = await buck.log("last", "--all")
    paths = list(out.stdout.splitlines())
    assert len(paths) > 0
    for path in paths:
        assert os.path.exists(path)
        assert "/log/" in path or "\\log\\" in path


# TODO: This would be more reliable if it were an isolated test.
@buck_test(inplace=True)
async def test_what_ran_local(buck: Buck) -> None:
    target = "fbcode//buck2/tests/targets/rules/genrule:mktemp"
    await buck.build(
        target,
        "--no-remote-cache",
    )
    out = await buck.log("what-ran")
    out = [line.strip() for line in out.stdout.splitlines() if target in line]
    assert len(out) == 1

    assert "\tlocal\t" in out[0]
    assert "\tre\t" not in out[0]


@buck_test(inplace=True)
async def test_what_failed(buck: Buck) -> None:
    pkg = "fbcode//buck2/tests/targets/rules/genrule/bad"
    bad = "my_genrule_bad_with_dep"
    good = "stub"

    await expect_failure(buck.build(f"{pkg}:{bad}"))
    out = await buck.log("what-failed")

    # Only the failed command should be in what-failed.
    assert f"{pkg}:{bad}" in out.stdout
    assert f"{pkg}:{good}" not in out.stdout

    # Even though both commands are here.
    out = await buck.log("what-ran")
    assert f"{pkg}:{bad}" in out.stdout
    assert f"{pkg}:{good}" in out.stdout


@buck_test(inplace=False)
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


@buck_test(inplace=False)
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
