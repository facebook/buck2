# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

# pyre-strict


from buck2.tests.e2e_util.api.buck import Buck
from buck2.tests.e2e_util.buck_workspace import buck_test
from buck2.tests.e2e_util.helper.utils import filter_events


@buck_test()
async def test_forward_node_supports_cutoff(buck: Buck) -> None:
    await buck.targets("--show-output", "root//:main")
    # Add a file to the root directory
    with open(buck.cwd / "TARGETS.fixture", "a") as targetsfile:
        targetsfile.write("\n# a comment\n")
    await buck.targets("--show-output", "root//:main")

    events = await filter_events(buck, "Event", "data", "SpanEnd", "data")
    loads = []
    analyses = []

    for ev in events:
        if "Load" in ev:
            loads.append(ev)
        if "Analysis" in ev:
            analyses.append(ev)

    assert len(loads) > 0
    # TODO(cjhopman): fix
    assert len(analyses) == 0, "should not have analysed anything"
