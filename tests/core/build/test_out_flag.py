# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

# pyre-strict


import os
import tempfile
from pathlib import Path

from buck2.tests.e2e_util.api.buck import Buck
from buck2.tests.e2e_util.asserts import expect_failure
from buck2.tests.e2e_util.buck_workspace import buck_test


@buck_test()
async def test_out_single_default_output(buck: Buck) -> None:
    with tempfile.TemporaryDirectory() as out:
        output = os.path.join(out, "output")
        await buck.build("//:a", "--out", output)
        with open(output) as readable:
            assert readable.read() == "a\n"


@buck_test()
async def test_out_overwrite(buck: Buck) -> None:
    with tempfile.TemporaryDirectory() as out:
        output = os.path.join(out, "output")
        await buck.build("//:a", "--out", output)
        await buck.build("//:a", "--out", output)


@buck_test()
async def test_out_parent_not_exist(buck: Buck) -> None:
    with tempfile.TemporaryDirectory() as out:
        output = os.path.join(out, "notexist", "output")
        await buck.build("//:a", "--out", output)
        with open(output) as readable:
            assert readable.read() == "a\n"


@buck_test()
async def test_out_single_default_output_to_dir(buck: Buck) -> None:
    with tempfile.TemporaryDirectory() as out:
        await buck.build("//:a", "--out", out)
        with open(Path(out) / "a.txt") as readable:
            assert readable.read() == "a\n"


@buck_test()
async def test_out_no_outputs(buck: Buck) -> None:
    with tempfile.NamedTemporaryFile("w") as out:
        await expect_failure(
            buck.build("//:none", "--out", out.name),
            stderr_regex="produced zero default outputs",
        )


@buck_test()
async def test_out_multiple_outputs(buck: Buck) -> None:
    with tempfile.NamedTemporaryFile("w") as out:
        await expect_failure(
            buck.build("//:ab", "--out", out.name),
            stderr_regex="produced 2 outputs",
        )


@buck_test()
async def test_out_multiple_targets(buck: Buck) -> None:
    with tempfile.NamedTemporaryFile("w") as out:
        await expect_failure(
            buck.build("//:a", "//:b", "--out", out.name),
            stderr_regex="command built multiple top-level targets",
        )


@buck_test()
async def test_out_directory(buck: Buck) -> None:
    with tempfile.TemporaryDirectory() as out:
        await buck.build("//:dir", "--out", out)
        assert (Path(out) / "b.txt").exists()
        assert (Path(out) / "nested_dir" / "a.txt").exists()


@buck_test()
async def test_out_stdout_multiple(buck: Buck) -> None:
    result = await buck.build("//:a", "//:b", "--out", "-")

    # The e2e test runner adds a `--build-report` flag in order to be able
    # to parse out failures. In normal usage of `--out -` there wouldn't be this
    # extra line of JSON on the stdout, we'd _just_ get the requested outputs.
    a, b, build_report, trailing = result.stdout.split("\n")
    assert (a, b) == ("a", "b") or (a, b) == ("b", "a")
    assert build_report.startswith("{")
    assert trailing == ""


@buck_test()
async def test_out_stdout_none(buck: Buck) -> None:
    await buck.build("--out", "-")


@buck_test()
async def test_out_stdout_directory(buck: Buck) -> None:
    await expect_failure(
        buck.build("//:dir", "--out", "-"),
        stderr_regex="produces a default output that is a directory, and cannot be sent to stdout",
    )
