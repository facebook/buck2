# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

# pyre-strict


import json

from buck2.tests.e2e_util.api.buck import Buck
from buck2.tests.e2e_util.buck_workspace import buck_test


@buck_test()
async def test_transition_info_outgoing_edge(buck: Buck) -> None:
    res = await buck.cquery(
        "root//:base", "-u", ":pre_outgoing_transition", "-a", "labels"
    )
    res = json.loads(res.stdout)
    assert len(res) == 1
    assert list(res.values())[0]["labels"] == ["cat"]


@buck_test()
async def test_transition_info_incoming_edge(buck: Buck) -> None:
    res = await buck.cquery(
        "root//:base", "-u", ":pre_incoming_transition", "-a", "labels"
    )
    res = json.loads(res.stdout)
    assert len(res) == 1
    assert list(res.values())[0]["labels"] == ["cat"]
