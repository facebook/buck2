# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

# pyre-strict


# Only used for test cases that cannot be run multithreaded.
# Sets `serialize_test_cases` on this test instance.

from pathlib import Path

from buck2.tests.e2e_util.api.buck import Buck
from buck2.tests.e2e_util.buck_workspace import buck_test


# We want to test modifying source files on Eden, which in most cases, means inplace.
# Because we are modifying source files in the buck, make sure this test doesn't get
# run multiple times.
@buck_test(inplace=True)
async def test_modify_src_eden(buck: Buck) -> None:
    rule = "fbcode//buck2/tests/targets/modify_eden:mysrcrule"
    path = Path(buck.cwd / "buck2/tests/targets/modify_eden/src.txt")
    try:
        path.write_text("HELLO\n")
        result = await buck.build(rule)
        output = result.get_build_report().output_for_target(rule)
        assert Path(output).read_text() == "HELLO\n"

        path.write_text("GOODBYE\n")
        result = await buck.build(rule)
        output = result.get_build_report().output_for_target(rule)
        assert Path(output).read_text() == "GOODBYE\n"
    finally:
        # Put the source back after we have finished
        path.write_text("HELLO\n")
