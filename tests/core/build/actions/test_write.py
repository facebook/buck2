# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

# pyre-strict


import os

from buck2.tests.e2e_util import asserts
from buck2.tests.e2e_util.api.buck import Buck
from buck2.tests.e2e_util.asserts import expect_failure
from buck2.tests.e2e_util.buck_workspace import buck_test

from buck2.tests.e2e_util.helper.utils import filter_events


@buck_test(data_dir="write")
async def test_write_files(buck: Buck) -> None:
    result = await buck.build(
        "//:simple",
        "//:uses_declared_output",
        "//:uses_declared_output_as_output",
        "//:declares_output",
        "//:is_executable",
        "//:writes_array_of_commands",
        "//:writes_command_lines",
        "//:writes_frozen_command_lines",
        "//:with_inputs_and_copy",
        "//:writes_absolute",
    )
    build_report = result.get_build_report()

    simple = build_report.output_for_target("//:simple", rel_path=True)

    output = build_report.output_for_target("//:uses_declared_output")
    assert output.read_text().rstrip() == "some content"
    asserts.assert_not_executable(output)

    output = build_report.output_for_target("//:uses_declared_output_as_output")
    assert output.read_text().rstrip() == "some content"
    asserts.assert_not_executable(output)

    output = build_report.output_for_target("//:declares_output")
    assert output.read_text().rstrip() == "some content"
    asserts.assert_not_executable(output)

    output = build_report.output_for_target("//:is_executable")
    assert output.read_text().rstrip() == "some content"
    asserts.assert_executable(output)

    output = build_report.output_for_target("//:writes_array_of_commands")
    assert output.read_text().rstrip() == f"{str(simple)}\nsome content"
    asserts.assert_not_executable(output)

    output = build_report.output_for_target("//:writes_command_lines")
    assert output.read_text().rstrip() == f"{str(simple)}\nsome content"
    asserts.assert_not_executable(output)

    output = build_report.output_for_target("//:writes_frozen_command_lines")
    assert output.read_text().rstrip() == str(simple)
    asserts.assert_not_executable(output)

    output = build_report.output_for_target("//:with_inputs_and_copy")
    assert output.read_text().rstrip() == "some content"

    output = build_report.output_for_target("//:writes_absolute")
    assert os.path.isabs(output.read_text().strip())


@buck_test(data_dir="write_fails")
async def test_write_files_fails_invalid_content(buck: Buck) -> None:
    await expect_failure(
        buck.build("//:fails_on_invalid_contents"),
        stderr_regex="Type of parameter `content`",
    )


@buck_test(data_dir="write_fails")
async def test_write_files_fails_invalid_output(buck: Buck) -> None:
    await expect_failure(
        buck.build("//:fails_on_invalid_output"),
        stderr_regex="Type of parameter `output`",
    )


@buck_test(data_dir="write")
async def test_output_size(buck: Buck) -> None:
    await buck.build("//:simple")

    output_size = await filter_events(
        buck,
        "Event",
        "data",
        "SpanEnd",
        "data",
        "ActionExecution",
        "output_size",
    )

    assert output_size
    assert output_size[0] == 8
