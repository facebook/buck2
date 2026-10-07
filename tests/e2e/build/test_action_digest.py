# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

# pyre-strict

import typing
from pathlib import Path

from buck2.tests.e2e_util.api.buck import Buck
from buck2.tests.e2e_util.buck_workspace import buck_test, get_mode_from_platform
from buck2.tests.e2e_util.helper.utils import read_what_ran


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
@buck_test(inplace=True)
async def test_action_digest(buck: Buck) -> None:
    await buck.build(
        get_mode_from_platform(),
        "fbcode//buck2/tests/targets/rules/rust/hello_world:welcome",
        "--remote-only",
    )
    compiled_digests = _identities_by_digest(await read_what_ran(buck))

    # TODO(nga): this should also test reverted buck2.
    buck.path_to_executable = Path("buck2")
    await buck.build(
        get_mode_from_platform(),
        "fbcode//buck2/tests/targets/rules/rust/hello_world:welcome",
        "--remote-only",
    )
    deployed_digests = _identities_by_digest(await read_what_ran(buck))

    only_compiled = compiled_digests.keys() - deployed_digests.keys()
    only_deployed = deployed_digests.keys() - compiled_digests.keys()
    assert not only_compiled and not only_deployed, (
        "Action Digest was modified, refer to comment on this test for next steps.\n"
        f"Only in compiled buck2 ({len(only_compiled)}):\n"
        + _format_digests(compiled_digests, only_compiled)
        + f"Only in deployed buck2 ({len(only_deployed)}):\n"
        + _format_digests(deployed_digests, only_deployed)
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
