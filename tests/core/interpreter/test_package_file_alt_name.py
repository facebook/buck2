# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

# pyre-strict


import os

from buck2.tests.e2e_util.api.buck import Buck
from buck2.tests.e2e_util.buck_workspace import buck_test


@buck_test()
async def test_package_file_alt_name(buck: Buck) -> None:
    output = await buck.build("//:")
    assert "AAA from BUCK_TREE" in output.stderr
    assert "AAA from PACKAGE" not in output.stderr

    os.unlink(buck.cwd / "BUCK_TREE")

    output = await buck.build("//:")
    assert "AAA from BUCK_TREE" not in output.stderr
    assert "AAA from PACKAGE" in output.stderr
