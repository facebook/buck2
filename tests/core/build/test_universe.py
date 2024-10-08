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
async def test_build_universe(buck: Buck) -> None:
    # Run the build without universe.
    result = await buck.build("//:test")
    build_report = result.get_build_report()
    output = build_report.output_for_target("//:test")
    assert output.read_text().rstrip() == "default"

    # Now build the same target, but with the universe.
    result = await buck.build(
        "//:test",
        "--target-universe",
        "//:universe",
    )
    build_report = result.get_build_report()
    output = build_report.output_for_target("//:test")
    assert output.read_text().rstrip() == "cat"
