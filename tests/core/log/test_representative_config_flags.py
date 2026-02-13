# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

# pyre-unsafe

from buck2.tests.e2e_util.api.buck import Buck
from buck2.tests.e2e_util.buck_workspace import buck_test


@buck_test(write_invocation_record=True)
async def test_representative_config_flags_disregards_run_args(
    buck: Buck,
) -> None:
    res = await buck.run(
        "//:my_rule",
        "--config",
        "foo.bar=baz",
        "--",
        "--config",
        "should.not=include",
    )

    assert res.invocation_record()["representative_config_flags"] == ["-c foo.bar=baz"]


@buck_test(write_invocation_record=True)
async def test_representative_config_flags_includes_build_args(
    buck: Buck,
) -> None:
    res = await buck.build(
        "--config",
        "foo.bar=baz",
        # For `build` commands, anything after `--` is a positional arg.
        "--",
        "//:my_rule",
    )

    assert res.invocation_record()["representative_config_flags"] == ["-c foo.bar=baz"]
