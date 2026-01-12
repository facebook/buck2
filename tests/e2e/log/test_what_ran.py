# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

# pyre-strict


import csv
import json
import sys

from buck2.tests.e2e_util.api.buck import Buck
from buck2.tests.e2e_util.asserts import expect_failure
from buck2.tests.e2e_util.buck_workspace import buck_test


# builds targets in an fbcode target configuration, unsupported on mac RE workers
def fbcode_linux_only() -> bool:
    return sys.platform == "linux"


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
    assert repro["identity"] == "fbcode//buck2/tests/targets/rules/sh_test:test"
    assert repro["reproducer"]["executor"] == "Local"
    assert repro["reproducer"]["details"]["command"][1] == "arg1"
    assert repro["extra"]["testcases"] == []


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
        ), (
            "we should have some empty std_errs and also HELLO_STDERR since we echo it in TARGETS: `{}`".format(
                out
            )
        )

        out = await buck.log(
            "what-ran", "--show-std-err", "--omit-empty-std-err", "--format", "json"
        )
        out = [line.strip() for line in out.stdout.splitlines()]
        out = [json.loads(line) for line in out if line]
        assert all(rec["std_err"] != "" for rec in out), (
            "we should have no empty std_errs: `{}`".format(out)
        )

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
        assert discovery["identity"] == "fbcode//buck2/tests/targets/rules/go/test:test"

        # test running
        repro = repros["test.run"]
        assert repro["reason"] == "test.run"
        assert repro["identity"] == "fbcode//buck2/tests/targets/rules/go/test:test"
        assert repro["reproducer"]["executor"] == "Local"
        assert repro["extra"]["testcases"] == ["TestFoo"]

    @buck_test(inplace=True)
    async def test_what_ran_csv_target_with_test_cases(buck: Buck) -> None:
        await buck.test("fbcode//buck2/tests/targets/rules/go/test:test")
        out = await buck.log("what-ran", "--format", "csv")
        out = [line.strip() for line in out.stdout.splitlines()]
        header = ["reason", "identity", "executor", "reproducer"]
        out = [dict(zip(header, record)) for record in csv.reader(out) if record]
        assert out[0] == dict(zip(header, header)), (
            "ensure that first entry in csv is the header"
        )
        out = [repro for repro in out if repro.get("reason", "").startswith("test.")]
        assert len(out) == 2, "out should have 2 test lines: `{}`".format(out)
        repros = {repro["reason"]: repro for repro in out}

        # test discovery
        discovery = repros["test.discovery"]
        assert discovery["identity"] == "fbcode//buck2/tests/targets/rules/go/test:test"

        # test running
        repro = repros["test.run"]
        assert repro["reason"] == "test.run"
        assert repro["identity"] == "fbcode//buck2/tests/targets/rules/go/test:test"


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
