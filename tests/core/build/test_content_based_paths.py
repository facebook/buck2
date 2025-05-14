# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

# pyre-strict


from typing import List

from buck2.tests.e2e_util.api.buck import Buck
from buck2.tests.e2e_util.buck_workspace import buck_test


async def build_target_with_different_platforms_and_verify_output_paths_are_identical(
    buck: Buck,
    target: str,
    args: List[str] | None = None,
) -> None:
    if args is None:
        args = []
    result1 = await buck.build(
        target,
        "--target-platforms",
        "root//:p_default",
        "--show-output",
        *args,
    )
    result2 = await buck.build(
        target,
        "--target-platforms",
        "root//:p_cat",
        "--show-output",
        *args,
    )

    path1 = result1.get_target_to_build_output().get(target)
    path2 = result2.get_target_to_build_output().get(target)

    assert path1 is not None
    assert "output_artifact" not in path1
    assert path1 == path2


@buck_test()
async def test_write_with_content_based_path(buck: Buck) -> None:
    target = "root//:write_with_content_based_path"
    await build_target_with_different_platforms_and_verify_output_paths_are_identical(
        buck, target
    )


@buck_test()
async def test_run_remote_with_content_based_path(buck: Buck) -> None:
    target = "root//:run_remote_with_content_based_path"
    await build_target_with_different_platforms_and_verify_output_paths_are_identical(
        buck,
        target,
        ["--remote-only"],
    )


@buck_test()
async def test_copy_with_content_based_path(buck: Buck) -> None:
    target = "root//:copy_with_content_based_path"
    await build_target_with_different_platforms_and_verify_output_paths_are_identical(
        buck, target
    )


@buck_test()
async def test_symlink_with_content_based_path(buck: Buck) -> None:
    target = "root//:symlink_with_content_based_path"
    await build_target_with_different_platforms_and_verify_output_paths_are_identical(
        buck, target
    )
