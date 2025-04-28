# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

# pyre-strict


from buck2.tests.e2e_util.api.buck import Buck
from buck2.tests.e2e_util.buck_workspace import buck_test
from buck2.tests.e2e_util.helper.utils import json_get


@buck_test()
async def test_no_dice_invalidation_on_root_directory_changes(buck: Buck) -> None:
    await buck.build("root//dir:")

    # Add a file to the root directory
    (buck.cwd / "file.txt").write_text("hello world")

    await buck.build("root//dir:")

    log = (await buck.log("show")).stdout.splitlines()

    for line in log:
        e = json_get(
            line,
            "Event",
            "data",
            "SpanEnd",
            "data",
            "Load",
        )
        assert e is None, "Should not have loaded anything"
