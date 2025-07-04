# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

# pyre-strict


import json

from buck2.tests.e2e_util.api.buck import Buck
from buck2.tests.e2e_util.asserts import expect_failure
from buck2.tests.e2e_util.buck_workspace import buck_test


@buck_test()
async def test_transition_info_outgoing_edge(buck: Buck) -> None:
    res = await buck.cquery(
        "root//:base", "-u", ":pre_outgoing_transition", "-a", "labels"
    )
    res = json.loads(res.stdout)
    assert len(res) == 1
    assert list(res.values())[0]["labels"] == ["cat"]

    res = await buck.cquery(
        "root//:base", "-u", ":pre_dynamic_outgoing_transition", "-a", "labels"
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


@buck_test()
async def test_unexpected_dynamic_outgoing(buck: Buck) -> None:
    await expect_failure(
        buck.uquery("root//unexpected_dynamic:unexpected_dynamic"),
        stderr_regex="Expected `str`, but got `tuple",
    )
