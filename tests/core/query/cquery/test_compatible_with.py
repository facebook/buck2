# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

# pyre-strict


import re

from buck2.tests.e2e_util.api.buck import Buck
from buck2.tests.e2e_util.buck_workspace import buck_test


@buck_test()
async def test_compatible_with(buck: Buck) -> None:
    for good in ["root//:pass", "root//:pass2"]:
        out = await buck.cquery(good)
        assert re.match(
            "{} \\(.*\\)\n".format(good),
            out.stdout,
        )

    for bad in ["root//:fail", "root//:fail2"]:
        out = await buck.cquery(bad)
        assert out.stdout == ""
