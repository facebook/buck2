# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.


import json
from pathlib import Path

from buck2.tests.e2e_util.api.buck import Buck
from buck2.tests.e2e_util.asserts import expect_failure
from buck2.tests.e2e_util.buck_workspace import buck_test


def _read_rendered_inputs(script: Path, origin: Path) -> list[str]:
    inline, macro, direct = script.read_text().splitlines()
    assert macro.startswith("@")
    macro_path = origin / macro[1:]
    return [
        (origin / inline).read_text(),
        (origin / macro_path.read_text()).read_text(),
        (origin / direct).read_text(),
    ]


@buck_test()
async def test_writer_inputs(buck: Buck) -> None:
    result = await buck.build(
        "//:mixed", "//:inline[primary]", "//:macro[primary]", "//:direct[primary]"
    )
    script = result.get_build_report().output_for_target("root//:mixed")
    assert _read_rendered_inputs(script, buck.cwd) == ["inline", "macro", "direct"]

    query = await buck.bxl("//inputs.bxl:inputs")
    inputs = json.loads(query.stdout)
    macro = next(path for path in inputs if path.endswith(".macro"))
    assert set(inputs["script"]) == {
        "script",
        "inline.primary",
        "direct.primary",
        macro,
    }
    assert set(inputs[macro]) == {macro, "macro.primary"}
    assert set(inputs["inputs.json"]) == {
        "inputs.json",
        "inline.primary",
        "direct.primary",
    }
    assert {
        "inline.primary",
        "macro.primary",
        "direct.primary",
        "hidden.primary",
    } <= set(inputs["run"])

    await expect_failure(
        buck.build("//:mixed[run]"), stderr_regex="Execution resource requested"
    )
    await expect_failure(
        buck.build("//:retained"), stderr_regex="Execution resource requested"
    )
    result = await buck.build("//:mixed[json]")
    output = result.get_build_report().output_for_target(
        "root//:mixed", sub_target="json"
    )
    assert [
        (buck.cwd / path).read_text() for path in json.loads(output.read_text())
    ] == [
        "inline",
        "direct",
    ]
    await expect_failure(
        buck.build("//:retained[json]"), stderr_regex="Execution resource requested"
    )


@buck_test()
async def test_macro_writer_ignores_inline_inputs(buck: Buck) -> None:
    await buck.build("//:macro_only[macros]")
    await expect_failure(
        buck.build("//:macro_only"), stderr_regex="Execution resource requested"
    )


@buck_test()
async def test_hidden_macro_inputs(buck: Buck) -> None:
    result = await buck.build(
        "//:hidden_macro_writer",
        "//:inline[primary]",
        "//:macro[primary]",
        "//:direct[primary]",
    )
    script = result.get_build_report().output_for_target("root//:hidden_macro_writer")
    assert _read_rendered_inputs(script, buck.cwd) == ["inline", "macro", "direct"]


@buck_test()
async def test_relative_macro_inputs(buck: Buck) -> None:
    result = await buck.build(
        "//:relative",
        "//:origin",
        "//:inline[primary]",
        "//:macro[primary]",
        "//:direct[primary]",
    )
    report = result.get_build_report()
    script = report.output_for_target("root//:relative")
    origin = report.output_for_target("root//:origin").parent
    assert _read_rendered_inputs(script, origin) == ["inline", "macro", "direct"]
