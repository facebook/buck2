# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

# pyre-strict

import ast
from typing import Any

from buck2.tests.e2e_util.api.buck import Buck
from buck2.tests.e2e_util.asserts import expect_failure
from buck2.tests.e2e_util.buck_workspace import buck_test


def extract_test_output(stderr: str) -> dict[str, Any]:
    """Extract the DATA_LOAD_TEST_OUTPUT dict from buck2 stderr."""
    marker = "DATA_LOAD_TEST_OUTPUT: "
    for line in stderr.splitlines():
        idx = line.find(marker)
        if idx != -1:
            return ast.literal_eval(line[idx + len(marker) :])  # type: ignore[no-any-return]
    raise AssertionError(f"Could not find '{marker}' in stderr:\n{stderr}")


@buck_test()
async def test_load_format_selects_the_parser(buck: Buck) -> None:
    result = await buck.targets("root//:")
    data = extract_test_output(result.stderr)

    assert data["lock"]["version"] == 3
    assert [p["name"] for p in data["lock"]["package"]] == ["serde", "toml"]
    assert data["metadata"] == {"name": "example", "version": 1}
    assert data["greeting"] == "hello"


@buck_test()
async def test_load_format_files_are_tracked_dependencies(buck: Buck) -> None:
    """The point of routing `format=` through ImportPath is that the loaded file is a
    dependency of the parse, so editing it invalidates the build file."""
    await buck.targets("root//:")

    (buck.cwd / "deps.lock").write_text('version = 4\n\n[[package]]\nname = "anyhow"\n')
    (buck.cwd / "metadata.txt").write_text('{"name": "renamed", "version": 2}\n')
    (buck.cwd / "rules.bzl.in").write_text('greeting = "goodbye"\n')

    result = await buck.targets("root//:")
    data = extract_test_output(result.stderr)

    assert data["lock"]["version"] == 4
    assert [p["name"] for p in data["lock"]["package"]] == ["anyhow"]
    assert data["metadata"] == {"name": "renamed", "version": 2}
    assert data["greeting"] == "goodbye"


@buck_test()
async def test_load_format_parse_error_names_the_format(buck: Buck) -> None:
    """An unparseable file fails the whole package at load time. Because the extension no
    longer implies the parser, the error has to say which one ran and why."""
    (buck.cwd / "deps.lock").write_text("this is not valid toml {{{\n")

    await expect_failure(
        buck.targets("root//:"),
        stderr_regex=r"Parsing `root//deps\.lock` as toml, as requested by `\?format=toml`",
    )


@buck_test()
async def test_load_format_rejects_an_unknown_format(buck: Buck) -> None:
    (buck.cwd / "TARGETS.fixture").write_text('load(":metadata.txt?format=yaml", "value")\n')

    await expect_failure(
        buck.targets("root//:"),
        stderr_regex=r"Unknown load format `yaml`",
    )


@buck_test()
async def test_load_without_format_still_requires_a_known_extension(buck: Buck) -> None:
    (buck.cwd / "TARGETS.fixture").write_text('load(":metadata.txt", "value")\n')

    await expect_failure(
        buck.targets("root//:"),
        stderr_regex=r"must have suffix `\.bzl`, `\.json`, or `\.toml`, or specify `\?format=`",
    )
