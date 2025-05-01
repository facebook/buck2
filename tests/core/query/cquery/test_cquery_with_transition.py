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


def _replace_hash(s: str) -> str:
    return re.sub(r"\b[0-9a-f]{16}\b", "<HASH>", s)


@buck_test()
async def test_cquery_transition_without_target_universe(buck: Buck) -> None:
    result = await buck.cquery(
        "root//:buck",
        "--target-platforms=root//:p",
    )

    # Both configurations for the target are returned: the default, and the transition
    lines = result.stdout.splitlines()
    assert 2 == len(lines)
    assert _replace_hash(lines[0]) == "root//:buck (root//:p#<HASH>)"
    assert _replace_hash(lines[1]) == "root//:buck (transitioned-to-reindeer#<HASH>)"

    # Test cquery with "%s".
    result = await buck.cquery(
        "%s",
        "root//:buck",
        "root//:moose",
        "--target-platforms=root//:p",
    )

    lines = result.stdout.splitlines()
    assert 4 == len(lines)
    assert _replace_hash(lines[0]) == "root//:buck (root//:p#<HASH>)"
    assert _replace_hash(lines[1]) == "root//:buck (transitioned-to-reindeer#<HASH>)"
    assert _replace_hash(lines[2]) == "root//:moose (root//:p#<HASH>)"
    assert _replace_hash(lines[3]) == "root//:moose (transitioned-to-reindeer#<HASH>)"

    # Test cquery with "%Ss"
    result = await buck.cquery(
        "%Ss",
        "root//:buck",
        "root//:moose",
        "--target-platforms=root//:p",
    )

    lines = result.stdout.splitlines()
    assert 4 == len(lines)
    assert _replace_hash(lines[0]) == "root//:buck (root//:p#<HASH>)"
    assert _replace_hash(lines[1]) == "root//:buck (transitioned-to-reindeer#<HASH>)"
    assert _replace_hash(lines[2]) == "root//:moose (root//:p#<HASH>)"
    assert _replace_hash(lines[3]) == "root//:moose (transitioned-to-reindeer#<HASH>)"


@buck_test()
async def test_cquery_transition_with_target_universe(buck: Buck) -> None:
    result = await buck.cquery(
        "root//:buck",
        "--target-platforms=root//:p",
        "--target-universe",
        "root//:buck",
    )

    lines = result.stdout.splitlines()
    assert 2 == len(lines)
    assert _replace_hash(lines[0]) == "root//:buck (root//:p#<HASH>)"
    assert _replace_hash(lines[1]) == "root//:buck (transitioned-to-reindeer#<HASH>)"

    # Test cquery with "%s".
    result = await buck.cquery(
        "%s",
        "root//:buck",
        "root//:moose",
        "--target-platforms=root//:p",
        "--target-universe",
        "root//:buck,root//:moose",
    )

    lines = result.stdout.splitlines()
    assert 4 == len(lines)
    assert _replace_hash(lines[0]) == "root//:buck (root//:p#<HASH>)"
    assert _replace_hash(lines[1]) == "root//:buck (transitioned-to-reindeer#<HASH>)"
    assert _replace_hash(lines[2]) == "root//:moose (root//:p#<HASH>)"
    assert _replace_hash(lines[3]) == "root//:moose (transitioned-to-reindeer#<HASH>)"

    # Test cquery with "%Ss".
    result = await buck.cquery(
        "%Ss",
        "root//:buck",
        "root//:moose",
        "--target-platforms=root//:p",
        "--target-universe",
        "root//:buck,root//:moose",
    )

    lines = result.stdout.splitlines()
    assert 4 == len(lines)
    assert _replace_hash(lines[0]) == "root//:buck (root//:p#<HASH>)"
    assert _replace_hash(lines[1]) == "root//:buck (transitioned-to-reindeer#<HASH>)"
    assert _replace_hash(lines[2]) == "root//:moose (root//:p#<HASH>)"
    assert _replace_hash(lines[3]) == "root//:moose (transitioned-to-reindeer#<HASH>)"
