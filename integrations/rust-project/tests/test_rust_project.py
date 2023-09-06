# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

import json

from xplat.build_infra.buck_e2e.api.buck import Buck
from xplat.build_infra.buck_e2e.buck_workspace import buck_test

# Uses a dependency graph like this:
#
# foo:a         foo:b
#    |            |
#   \/           \/
# bar:c         bar:d
#    |            |
#    \-> foo:e  <-/
#         |
#         \/
#        foo:f
#
# `foo:a` and `bar:d` are workspaces within package `foo`
# Skip on mac and windows to avoid having to deal with mode files
@buck_test(inplace=True, skip_for_os=["darwin", "windows"])
async def test_workspaces(buck: Buck) -> None:
    result = await buck.bxl(
        "prelude//rust/rust-analyzer/resolve_deps.bxl:expand_and_resolve",
        "--",
        "--targets",
        "//buck2/integrations/rust-project/tests/targets/foo:e",
    )
    result = json.loads(result.stdout)
    assert result["expanded_targets"] == [
        "fbcode//buck2/integrations/rust-project/tests/targets/bar:d",
        "fbcode//buck2/integrations/rust-project/tests/targets/foo:a",
    ]
    target_and_in_workspace = {
        t: v["in_workspace"] for t, v in result["resolved_deps"].items()
    }
    assert target_and_in_workspace == {
        "fbcode//buck2/integrations/rust-project/tests/targets/foo:a": True,
        "fbcode//buck2/integrations/rust-project/tests/targets/bar:c": False,
        "fbcode//buck2/integrations/rust-project/tests/targets/bar:d": True,
        "fbcode//buck2/integrations/rust-project/tests/targets/foo:e": True,
        "fbcode//buck2/integrations/rust-project/tests/targets/foo:f": True,
    }

    # The target being edited is not in any workspaces
    result = await buck.bxl(
        "prelude//rust/rust-analyzer/resolve_deps.bxl:expand_and_resolve",
        "--",
        "--targets",
        "//buck2/integrations/rust-project/tests/targets/bar:c",
    )
    result = json.loads(result.stdout)
    assert result["expanded_targets"] == [
        "fbcode//buck2/integrations/rust-project/tests/targets/bar:c"
    ]
    target_and_in_workspace = {
        t: v["in_workspace"] for t, v in result["resolved_deps"].items()
    }
    assert target_and_in_workspace == {
        "fbcode//buck2/integrations/rust-project/tests/targets/bar:c": True,
        "fbcode//buck2/integrations/rust-project/tests/targets/foo:e": False,
        "fbcode//buck2/integrations/rust-project/tests/targets/foo:f": False,
    }


@buck_test(inplace=True, skip_for_os=["darwin", "windows"])
async def test_alias(buck: Buck) -> None:
    result = await buck.bxl(
        "prelude//rust/rust-analyzer/resolve_deps.bxl:expand_and_resolve",
        "--",
        "--targets",
        "fbcode//buck2/integrations/rust-project/tests/targets/alias/...",
    )
    result = json.loads(result.stdout)
    assert result["expanded_targets"] == [
        "fbcode//buck2/integrations/rust-project/tests/targets/alias:l",
        "fbcode//buck2/integrations/rust-project/tests/targets/alias:l_alias",
    ]


# FIXME: Remove once actual tests work on mac and windows
@buck_test(inplace=True)
async def test_noop(buck: Buck) -> None:
    return
