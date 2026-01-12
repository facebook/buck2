# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

# pyre-strict


from buck2.tests.e2e_util.api.buck import Buck
from buck2.tests.e2e_util.asserts import expect_failure
from buck2.tests.e2e_util.buck_workspace import buck_test


def assert_file_content_matches(file_path: str, exptected_content: str) -> None:
    with open(file_path, "r") as f:
        content = f.read()
        assert content == exptected_content


@buck_test()
async def test_streaming_output_ensured_artifact(buck: Buck) -> None:
    result = await buck.bxl(
        "//streaming.bxl:streaming_output_ensured_artifact",
    )

    lines = result.stdout.splitlines()

    streaming_output_artifact_idx = -1
    output_file_path = ""
    line_before_print_idx = -1

    for idx, line in enumerate(lines):
        # output by `ctx.output.print(ensured_output)`
        if "output.txt" in line and streaming_output_artifact_idx == -1:
            output_file_path = line
            streaming_output_artifact_idx = idx
        # output by `ctx.output.print("Line before streaming print")`
        if "Line before streaming print" in line:
            line_before_print_idx = idx

    assert streaming_output_artifact_idx != -1, (
        "Cound not find the streaming print of ensured artifact"
    )
    assert line_before_print_idx != -1, "Cound not find the normal ctx.output.print"

    assert streaming_output_artifact_idx < line_before_print_idx, (
        "The streaming print is not before the normal print"
    )

    assert_file_content_matches(output_file_path, "hello world!")


@buck_test()
async def test_streaming_output(buck: Buck) -> None:
    result = await buck.bxl(
        "//streaming.bxl:streaming_output",
    )

    lines = result.stdout.splitlines()

    streaming_output_idx = -1
    line_before_print_idx = -1

    for idx, line in enumerate(lines):
        if "This is the streaming output" in line and streaming_output_idx == -1:
            streaming_output_idx = idx
        if "Line before streaming print" in line:
            line_before_print_idx = idx

    assert streaming_output_idx != -1, "Cound not find the streaming print"
    assert line_before_print_idx != -1, "Cound not find the normal ctx.output.print"

    assert streaming_output_idx < line_before_print_idx, (
        "The streaming print is not before the normal print"
    )


@buck_test()
async def test_streaming_output_without_duplicates(buck: Buck) -> None:
    result = await buck.bxl(
        "//streaming.bxl:streaming_output_without_duplicates",
    )

    streaming_output = "This is the streaming output"
    streaming_ensured_artifact_output = "output.txt"
    normal_output = "This is the normal output"

    stdout = result.stdout
    assert stdout.count(streaming_output) == 1, "Expected only one streaming output"
    assert stdout.count(streaming_ensured_artifact_output) == 1, (
        "Expected only one streaming ensured artifact output"
    )
    assert stdout.count(normal_output) == 1, "Expected only one normal output"

    # call again to check when bxl key is cached

    result = await buck.bxl(
        "//streaming.bxl:streaming_output_without_duplicates",
    )
    stdout = result.stdout
    assert stdout.count(streaming_output) == 1, (
        "Expected only one streaming output when cached"
    )
    assert stdout.count(streaming_ensured_artifact_output) == 1, (
        "Expected only one streaming ensured artifact output when cached"
    )
    assert stdout.count(normal_output) == 1, (
        "Expected only one normal output when cached"
    )


@buck_test()
async def test_streaming_output_waits_on(buck: Buck) -> None:
    result = await buck.bxl(
        "//streaming.bxl:streaming_output_waits_on",
    )

    stdout = result.stdout

    file0_idx = -1
    file1_idx = -1
    waits_on_output_idx = -1

    for idx, line in enumerate(stdout.splitlines()):
        if "output0.txt" in line:
            file0_idx = idx
        if "output1.txt" in line:
            file1_idx = idx
        if "Waits on two file" in line:
            waits_on_output_idx = idx

    assert file0_idx != -1 and file1_idx != -1 and waits_on_output_idx != -1, (
        "Cound not find the streaming print"
    )

    assert file0_idx < waits_on_output_idx or file1_idx < waits_on_output_idx, (
        "wait_on should after the one of the ensured artifact streaming print"
    )


@buck_test()
async def test_streaming_output_json(buck: Buck) -> None:
    async def check_output() -> None:
        result = await buck.bxl(
            "//streaming.bxl:streaming_output_json",
        )

        stdout = result.stdout

        file0_idx = -1
        file1_idx = -1
        waits_on_output_idx = -1
        linde_before_streaming_idx = -1

        for idx, line in enumerate(stdout.splitlines()):
            json_line = None
            if "output0.txt" in line:
                file0_idx = idx
                json_line = line
            if "output1.txt" in line:
                file1_idx = idx
                json_line = line
            if "waits_on" in line:
                waits_on_output_idx = idx
                json_line = line
            if "Line before print streaming" in line:
                linde_before_streaming_idx = idx
            if json_line is not None:
                json_line = json_line.strip()
                assert json_line.startswith("{") and json_line.endswith("}"), (
                    "Expected json line"
                )

        assert (
            file0_idx != -1
            and file1_idx != -1
            and waits_on_output_idx != -1
            and linde_before_streaming_idx != -1
        ), "Cound not find the output"

        assert file0_idx < waits_on_output_idx or file1_idx < waits_on_output_idx, (
            "wait_on should after the one of the ensured artifact streaming print"
        )

        assert linde_before_streaming_idx > max(
            file0_idx, file1_idx, waits_on_output_idx
        ), "The streaming print is not after the normal print"

    await check_output()
    await check_output()


@buck_test()
async def test_stream_output_fail(buck: Buck) -> None:
    async def check() -> None:
        await expect_failure(
            buck.bxl(
                "//streaming.bxl:stream_output_fail",
            ),
            stdout_regex="Streaming output",
        )

    await check()
    # check when cached, streaming output is still printed
    await check()
