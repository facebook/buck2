# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

# pyre-strict

import typing

from buck2.tests.e2e_util.api.buck import Buck
from buck2.tests.e2e_util.buck_workspace import buck_test


def with_buck2_args(output: str) -> typing.List[str]:
    return [
        "-c",
        f"test.buck2_output={output}",
    ]


@buck_test(inplace=False)
async def test_no_action_divergence_command(buck: Buck) -> None:
    await buck.build("//:simple", *with_buck2_args("foo"))
    out1 = await buck.log("last")
    path1 = out1.stdout.strip()
    await buck.build("//:simple", *with_buck2_args("foo"))
    out2 = await buck.log("last")
    path2 = out2.stdout.strip()
    out = await buck.log(
        "diff", "action-divergence", "--path1", path1, "--path2", path2
    )

    assert "No divergent actions found." in out.stdout


@buck_test(inplace=False)
async def test_action_divergence_command(buck: Buck) -> None:
    await buck.build("//:non_det", *with_buck2_args("foo"))
    await buck.build("//:non_det", *with_buck2_args("bar"))
    out = await buck.log(
        "diff", "action-divergence", "--recent1", "0", "--recent2", "1"
    )

    assert (
        "Present in both builds with differing output digests\nprelude//:non_det (<unspecified>) (write foo.txt)"
        in out.stdout
    )
