# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

# pyre-strict


from buck2.tests.e2e_util.api.buck import Buck
from buck2.tests.e2e_util.buck_workspace import buck_test


@buck_test()
async def test_output_artifact_twice_same(buck: Buck) -> None:
    res = await buck.build("root//:test_output_artifact_twice_same")
    assert (
        res.get_build_report()
        .output_for_target("root//:test_output_artifact_twice_same")
        .read_text()
        == "green lamp"
    )


@buck_test()
async def test_output_artifact_twice_with_projection(buck: Buck) -> None:
    res = await buck.build("root//:test_output_artifact_twice_with_projection")
    assert (
        res.get_build_report().output_for_target(
            "root//:test_output_artifact_twice_with_projection"
        )
        / "rel"
    ).read_text() == "red alert"
