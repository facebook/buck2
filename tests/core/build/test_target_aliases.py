# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

# pyre-strict


import json

from buck2.tests.e2e_util.api.buck import Buck
from buck2.tests.e2e_util.asserts import expect_failure
from buck2.tests.e2e_util.buck_workspace import buck_test


@buck_test()
async def test_target_aliases(buck: Buck) -> None:
    await buck.targets("alias")
    await buck.cquery("deps(alias)")

    await buck.targets("chain")
    await buck.cquery("deps(chain)")

    res = await buck.targets("--resolve-alias", "alias", "chain", "//targets:target")
    assert [line.strip() for line in res.stdout.splitlines()] == [
        "root//targets:target"
    ] * 3

    # Following a broken alias should fail
    await expect_failure(
        buck.targets("--resolve-alias", "bad"), stderr_regex="Invalid alias: `bad`"
    )

    # Asking for a non-existent alias / target should also fail. Note that
    # we're not capable of telling the difference between an alias that doesn't
    # exist vs. one that is broken.
    await expect_failure(
        buck.targets("--resolve-alias", "oops"), stderr_regex="Invalid alias: `oops`"
    )

    await expect_failure(
        buck.targets("--resolve-alias", "targets:not_existent"),
        stderr_regex="Invalid alias:.*Target does not exist in package",
    )
    await expect_failure(
        buck.targets("--resolve-alias", "broken:broken"),
        stderr_regex="Invalid alias:.*Package cannot be evaluated.*Parse error",
    )
    await expect_failure(
        buck.targets("--resolve-alias", "not_existent:not_existent"),
        stderr_regex="Invalid alias:.*Package cannot be evaluated.*does not exist",
    )
    await expect_failure(
        buck.targets("--resolve-alias", "..."),
        stderr_regex="Invalid alias.*does not expand to a single target",
    )


@buck_test()
async def test_resolve_alias_json(buck: Buck) -> None:
    res = await buck.targets(
        "--resolve-alias", "alias", "chain", "//targets:target", "--json"
    )

    assert json.loads(res.stdout) == [
        {
            "alias": "alias",
            "buck.package": "root//targets",
            "name": "target",
        },
        {
            "alias": "chain",
            "buck.package": "root//targets",
            "name": "target",
        },
        {
            "alias": "//targets:target",
            "buck.package": "root//targets",
            "name": "target",
        },
    ]


@buck_test()
async def test_resolve_alias_json_lines(buck: Buck) -> None:
    res = await buck.targets(
        "--resolve-alias", "alias", "chain", "//targets:target", "--json-lines"
    )

    lines = [line.strip() for line in res.stdout.splitlines()]
    lines = [line for line in lines if line]

    assert [json.loads(line) for line in res.stdout.splitlines()] == [
        {
            "alias": "alias",
            "buck.package": "root//targets",
            "name": "target",
        },
        {
            "alias": "chain",
            "buck.package": "root//targets",
            "name": "target",
        },
        {
            "alias": "//targets:target",
            "buck.package": "root//targets",
            "name": "target",
        },
    ]
