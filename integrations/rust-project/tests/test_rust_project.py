# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.


import json
from pathlib import Path

from buck2.tests.e2e_util.api.buck import Buck
from buck2.tests.e2e_util.buck_workspace import buck_test


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
        "prelude//rust/rust-analyzer/resolve_deps.bxl:resolve_targets",
        "--",
        "--targets",
        "//buck2/integrations/rust-project/tests/targets/foo:e",
    )
    result = json.load(open(result.stdout.rstrip()))
    assert result["expanded_targets"] == [
        "fbcode//buck2/integrations/rust-project/tests/targets/bar:d",
        "fbcode//buck2/integrations/rust-project/tests/targets/foo:a",
    ]
    target_and_in_workspace = {
        t: v["in_workspace"] for t, v in result["resolved_deps"].items()
    }
    expected_subset = {
        "fbcode//buck2/integrations/rust-project/tests/targets/foo:a": True,
        "fbcode//buck2/integrations/rust-project/tests/targets/bar:c": False,
        "fbcode//buck2/integrations/rust-project/tests/targets/bar:d": True,
        "fbcode//buck2/integrations/rust-project/tests/targets/foo:e": True,
        "fbcode//buck2/integrations/rust-project/tests/targets/foo:f": True,
    }
    assert expected_subset.items() <= target_and_in_workspace.items()

    # The target being edited is not in any workspaces
    result = await buck.bxl(
        "prelude//rust/rust-analyzer/resolve_deps.bxl:resolve_targets",
        "--",
        "--targets",
        "//buck2/integrations/rust-project/tests/targets/bar:c",
    )
    result = json.load(open(result.stdout.rstrip()))
    assert result["expanded_targets"] == [
        "fbcode//buck2/integrations/rust-project/tests/targets/bar:c"
    ]
    target_and_in_workspace = {
        t: v["in_workspace"] for t, v in result["resolved_deps"].items()
    }

    expected_subset = {
        "fbcode//buck2/integrations/rust-project/tests/targets/bar:c": True,
        "fbcode//buck2/integrations/rust-project/tests/targets/foo:e": False,
        "fbcode//buck2/integrations/rust-project/tests/targets/foo:f": False,
    }

    assert expected_subset.items() <= target_and_in_workspace.items()


@buck_test(inplace=True, skip_for_os=["darwin", "windows"])
async def test_alias(buck: Buck) -> None:
    result = await buck.bxl(
        "prelude//rust/rust-analyzer/resolve_deps.bxl:resolve_targets",
        "--",
        "--targets",
        "fbcode//buck2/integrations/rust-project/tests/targets/alias/...",
    )
    result = json.load(open(result.stdout.rstrip()))
    assert result["expanded_targets"] == [
        "fbcode//buck2/integrations/rust-project/tests/targets/alias:l",
        "fbcode//buck2/integrations/rust-project/tests/targets/alias:l_alias",
    ]


@buck_test(inplace=True, skip_for_os=["darwin", "windows"])
async def test_resolve_owning_buildfile_no_extra_targets(buck: Buck) -> None:
    result = await buck.bxl(
        "prelude//rust/rust-analyzer/resolve_deps.bxl:resolve_owning_buildfile",
        "--",
        "--max_extra_targets=0",
        "--files",
        str(
            Path("buck2/integrations/rust-project/tests/targets/foo/lib_f.rs").resolve()
        ),
    )
    result = json.loads(result.stdout)
    assert len(result) == 1
    buildfile_path, owners = result.popitem()
    assert buildfile_path.endswith(
        "buck2/integrations/rust-project/tests/targets/foo/BUCK"
    )
    owners.sort()
    assert owners == [
        "fbcode//buck2/integrations/rust-project/tests/targets/foo:f",
        "fbcode//buck2/integrations/rust-project/tests/targets/foo:f-unittest",
    ]


@buck_test(inplace=True, skip_for_os=["darwin", "windows"])
async def test_exclude_workspaces(buck: Buck) -> None:
    result = await buck.bxl(
        "prelude//rust/rust-analyzer/resolve_deps.bxl:resolve_targets",
        "--",
        "--targets",
        "//buck2/integrations/rust-project/tests/targets/foo:e",
        "--exclude_workspaces=true",
    )
    result = json.load(open(result.stdout.rstrip()))
    assert result["expanded_targets"] == [
        "fbcode//buck2/integrations/rust-project/tests/targets/foo:e",
    ]


@buck_test(inplace=True, skip_for_os=["darwin", "windows"])
async def test_fallback_compatible_with(buck: Buck) -> None:
    result = await buck.bxl(
        "prelude//rust/rust-analyzer/resolve_deps.bxl:resolve_targets",
        "--",
        "--targets",
        "//buck2/integrations/rust-project/tests/targets/foo:g_with_compatibility",
    )
    result = json.load(open(result.stdout.rstrip()))

    assert len(result["expanded_targets"]) > 0
    assert len(result["resolved_deps"]) > 0

    dep = list(result["resolved_deps"].values())[0]

    assert dep["kind"] == "prelude//rules.bzl:rust_library"
    assert dep["source_folder"] is not None


# FIXME: Remove once actual tests work on mac and windows
@buck_test(inplace=True)
async def test_noop(buck: Buck) -> None:
    return
