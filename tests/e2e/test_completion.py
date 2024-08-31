# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

# pyre-strict

from buck2.tests.e2e_util.api.buck import Buck
from buck2.tests.e2e_util.buck_workspace import buck_test


@buck_test(inplace=False)
async def test_completion_generates_a_script_for_bash_completion(buck: Buck) -> None:
    result = await buck.completion("bash")
    assert result.stdout != ""


@buck_test(inplace=False)
async def test_completion_generates_a_script_for_zsh_completion(buck: Buck) -> None:
    result = await buck.completion("zsh")
    assert result.stdout != ""


@buck_test(inplace=False)
async def test_completes_simple_directories(buck: Buck) -> None:
    result = await buck.complete("--target", "d")
    assert (
        result.stdout
        == """dir1/
dir1:
dir2/
"""
    )


@buck_test(inplace=False)
async def test_completes_simple_cells(buck: Buck) -> None:
    result = await buck.complete("--target", "cel")
    assert (
        result.stdout
        == """cell2a//
cell2a//:
cell3//
cell3//:
"""
    )


@buck_test(inplace=False)
async def test_completes_rules(buck: Buck) -> None:
    result = await buck.complete("--target", "dir1:")
    assert (
        result.stdout
        == """dir1:target1a
dir1:target1b
"""
    )
