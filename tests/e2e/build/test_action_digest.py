# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

# pyre-strict

import asyncio
import typing
from pathlib import Path

from buck2.tests.e2e_util.api.buck import Buck
from buck2.tests.e2e_util.buck_workspace import buck_test, get_mode_from_platform
from buck2.tests.e2e_util.helper.utils import read_what_ran


# Every action is normally an action-cache hit, so a build takes well under a
# minute. After a digest change the whole graph, including the in-graph Rust
# toolchain bootstrap, has to execute on RE, which takes far longer. The test
# fails on its own clock rather than letting TPX time it out, because on diffs
# a TPX timeout is reported as a warning instead of a failure.
_BUILD_DEADLINE_S = 20 * 60


# If this test fails, it means that a change that modifies action digest was made.
# Background in this post:
# https://fb.workplace.com/groups/buck2eng/permalink/3452581371706005/
# Changes should instead be deployed by:
#   1: Create a new buck2 flag and hide the changes behind it (Ex. D59503359)
#   2: Wait for bvb that contains #1 to land. To be safe, wait for a second to land
#      as well so you're guaranteed that the first bump can no longer be
#      fast-reverted.
#   3: Activate the flag via .buckconfig (Ex. D59648609)
#       3.1: Fix/followup on any CI failures caused by cache invalidation
#   4: Observe for a couple of days to ensure that there are no issues
#   5. Remove the code associated with the config flag but NOT the config itself,
#      this way this test wouldn't need to be changed at all (Ex. D59864942)
#   6: Wait for bvb that contains #5 to land. Optionally wait for a second as above.
#   7: Remove the config flag (Ex. D59988979)
#
# Three buck2 binaries are compared pairwise: the one compiled from this
# checkout, the deployed `stable` channel, and the deployed `previous`
# channel. On a version-bump diff `stable` is the candidate and `previous`
# is the version it replaces, so the stable/previous pair is the gate that
# keeps a digest change out of production even when trunk already contains it.
@buck_test(inplace=True)
async def test_action_digest(buck: Buck) -> None:
    compiled_digests = await _build_and_collect_digests(buck, "compiled buck2")

    buck.path_to_executable = Path("buck2")
    deployed_digests = await _build_and_collect_digests(buck, "deployed buck2")

    buck.set_env("BUCK2_CHANNEL", "previous")
    previous_digests = await _build_and_collect_digests(buck, "previous buck2")

    _assert_same_digests(
        "compiled buck2", compiled_digests, "deployed buck2", deployed_digests
    )
    _assert_same_digests(
        "deployed buck2", deployed_digests, "previous buck2", previous_digests
    )


async def _build_and_collect_digests(buck: Buck, arm_name: str) -> dict[str, list[str]]:
    build = buck.build(
        get_mode_from_platform(),
        "fbcode//buck2/tests/targets/rules/rust/hello_world:welcome",
        "--remote-only",
    )
    try:
        await asyncio.wait_for(build, timeout=_BUILD_DEADLINE_S)
    except asyncio.TimeoutError:
        raise AssertionError(
            f"Build with {arm_name} did not finish within {_BUILD_DEADLINE_S}s. "
            "Every action should be an action-cache hit; a build this slow almost "
            "always means the action digests changed and the graph is being "
            "rebuilt. Refer to comment on this test for next steps."
        ) from None
    return _identities_by_digest(await read_what_ran(buck))


def _assert_same_digests(
    left_name: str,
    left: dict[str, list[str]],
    right_name: str,
    right: dict[str, list[str]],
) -> None:
    only_left = left.keys() - right.keys()
    only_right = right.keys() - left.keys()
    assert not only_left and not only_right, (
        "Action Digest was modified, refer to comment on this test for next steps.\n"
        f"Only in {left_name} ({len(only_left)}):\n"
        + _format_digests(left, only_left)
        + f"Only in {right_name} ({len(only_right)}):\n"
        + _format_digests(right, only_right)
    )


def _identities_by_digest(
    what_ran: list[dict[str, typing.Any]],
) -> dict[str, list[str]]:
    """Map each action digest to the identities of the actions that produced it.

    The graph contains identical actions in several configurations, which
    share a digest. Whichever finishes first seeds the local dep-file cache,
    and the others are then served from it without a reproducer digest, so
    only the set of digests is stable between two builds.
    """
    digests: dict[str, list[str]] = {}
    for entry in what_ran:
        digest = entry["reproducer"]["details"].get("digest")
        if digest is not None:
            digests.setdefault(digest, []).append(entry["identity"])
    return digests


def _format_digests(
    digests: dict[str, list[str]], selected: typing.AbstractSet[str]
) -> str:
    return "".join(
        f"  {digest}  {'; '.join(digests[digest])}\n" for digest in sorted(selected)
    )
