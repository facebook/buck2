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


async def check_has_uquery_path(
    buck: Buck, target: str, dep: str, expect_fail: bool = False
) -> None:
    result = await buck.uquery(
        f"somepath({target}, {dep})",
    )
    path = result.stdout.splitlines()
    # Apparently, configuration deps never show up in `somepath`. Interesting.
    assert len(path) == 0

    result = await buck.uquery(
        f"deps({target})",
        "-a",
        "buck.deps",
        "-a",
        "buck.configuration_deps",
    )
    all_deps = [
        d
        for node in json.loads(result.stdout).values()
        for deps in node.values()
        for d in deps
    ]
    if expect_fail:
        assert dep not in all_deps
    else:
        assert dep in all_deps


@buck_test()
async def test_default_target_platform(buck: Buck) -> None:
    # FIXME(JakobDegen): Bug.
    await check_has_uquery_path(
        buck, ":with_custom_dtp", "root//:base", expect_fail=True
    )


@buck_test()
async def test_configured_dep_platform(buck: Buck) -> None:
    await check_has_uquery_path(buck, ":stub_configured", "root//:base")


@buck_test()
async def test_transition_dep_refs(buck: Buck) -> None:
    # FIXME(JakobDegen): Bug.
    await check_has_uquery_path(
        buck, ":pre_out_transition", "root//:cat", expect_fail=True
    )

    # FIXME(JakobDegen): Bug.
    await check_has_uquery_path(
        buck, ":post_out_transition", "root//:cat", expect_fail=True
    )

    await check_has_uquery_path(buck, ":pre_out_transition_vnew", "root//:transition")

    await check_has_uquery_path(buck, ":pre_inc_transition_vnew", "root//:transition")


@buck_test()
async def test_select_keys(buck: Buck) -> None:
    await check_has_uquery_path(buck, ":with_select", "root//:cat")
