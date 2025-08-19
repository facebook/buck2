# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

# pyre-strict

import json

from buck2.tests.e2e_util.api.buck import Buck
from buck2.tests.e2e_util.asserts import expect_failure
from buck2.tests.e2e_util.buck_workspace import buck_test


@buck_test()
async def test_transitive_sets(buck: Buck) -> None:
    rule = "//:bar"
    report = await buck.build(rule)
    out = report.get_build_report().output_for_target(rule)
    out = out.read_text()
    out = [line.strip() for line in out.strip().split("\n")]
    assert out == ["bar", "foo", "foo2", "foo1"]


@buck_test()
async def test_transitive_set_deduplication(buck: Buck) -> None:
    await buck.build("//:test_duplication")


@buck_test()
async def test_project_as_args_with_optional_args(buck: Buck) -> None:
    rule = "//:combined_optional_args"

    # TODO(ianc) Allow project_as_args to skip nodes that return None
    await expect_failure(
        buck.build(rule),
        stderr_regex="error: Expected `Artifact | CellPath | CellRoot | Label | OutputArtifact | ProjectRoot | ResolvedStringWithMacros | TaggedCommandLine | TargetLabel | TransitiveSetArgsProjection | WriteJsonCliArgs | cmd_args | str | RunInfo`, but got `NoneType (repr: None)`",
    )
    # report = await buck.build(rule)
    # out = report.get_build_report().output_for_target(rule)
    # out = out.read_text()
    # out = [line.strip() for line in out.strip().split("\n")]
    # assert out == ["combined_optional_args", "optional_args1"]


@buck_test()
async def test_project_as_json_with_optional_args(buck: Buck) -> None:
    rule = "//:combined_optional_json_args"

    report = await buck.build(rule)
    out = report.get_build_report().output_for_target(rule)
    out = out.read_text()
    out = json.loads(out)

    # TODO(ianc) remove the None
    assert out == ["combined_optional_json_args", "optional_json_args1", None]
