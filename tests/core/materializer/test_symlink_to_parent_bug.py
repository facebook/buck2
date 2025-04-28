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


@buck_test(skip_for_os=["windows"])
async def test_symlink_to_parent_bug(buck: Buck) -> None:
    result = await buck.build("//:whistle", "--prefer-local", "--no-remote-cache")
    out = result.get_build_report().output_for_target("//:whistle")
    assert str(out).endswith("/whistle")
    # Check the link was actually materialized.
    assert os.path.islink(out)


@buck_test(skip_for_os=["windows"])
async def test_symlink_to_self(buck: Buck) -> None:
    result = await buck.build("//:flute", "--prefer-local", "--no-remote-cache")
    out = result.get_build_report().output_for_target("//:flute")
    assert str(out).endswith("/flute")
    # Check the link was actually materialized.
    assert os.path.islink(out)


@buck_test()
async def test_noop(buck: Buck) -> None:
    return
