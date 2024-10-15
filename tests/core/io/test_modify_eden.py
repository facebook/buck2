# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

# pyre-strict


from pathlib import Path

from buck2.tests.e2e_util.api.buck import Buck
from buck2.tests.e2e_util.buck_workspace import buck_test


@buck_test(setup_eden=True)
async def test_modify_src_eden(buck: Buck) -> None:
    path = buck.cwd / "src.txt"

    path.write_text("HELLO\n")
    result = await buck.build("root//:copy_file")
    output = result.get_build_report().output_for_target("root//:copy_file")
    assert Path(output).read_text() == "HELLO\n"

    path.write_text("GOODBYE\n")
    result = await buck.build("root//:copy_file")
    output = result.get_build_report().output_for_target("root//:copy_file")
    assert Path(output).read_text() == "GOODBYE\n"
