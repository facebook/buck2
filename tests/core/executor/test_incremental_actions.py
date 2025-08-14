# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

# pyre-strict


from buck2.tests.e2e_util.api.buck import Buck
from buck2.tests.e2e_util.api.buck_result import BuckResult
from buck2.tests.e2e_util.buck_workspace import buck_test
from buck2.tests.e2e_util.helper.utils import random_string


@buck_test()
async def test_basic_incremental_action_local_only(buck: Buck) -> None:
    result = await buck.run(
        "root//:basic_incremental_action",
        "--local-only",
        "-c",
        f"test.seed={random_string()}",
    )
    assert result.stdout == "foo"
    result = await buck.run(
        "root//:basic_incremental_action",
        "--local-only",
        "-c",
        f"test.seed={random_string()}",
    )
    assert result.stdout == "foo bar"
    result = await buck.run(
        "root//:basic_incremental_action",
        "--local-only",
        "-c",
        f"test.seed={random_string()}",
    )
    assert result.stdout == "foo bar bar"


@buck_test()
async def test_incremental_action_from_remote_action(buck: Buck) -> None:
    result = await buck.run(
        "root//:basic_incremental_action",
        "--remote-only",
        "-c",
        f"test.seed={random_string()}",
    )
    assert result.stdout == "foo"
    result = await buck.run(
        "root//:basic_incremental_action",
        "--local-only",
        "-c",
        f"test.seed={random_string()}",
    )
    assert result.stdout == "foo bar"


@buck_test()
async def test_incremental_action_with_non_incremental_inbetween(buck: Buck) -> None:
    result = await buck.run(
        "root//:basic_incremental_action",
        "-c",
        f"test.seed={random_string()}",
    )
    assert result.stdout == "foo"

    result = await buck.run(
        "root//:basic_incremental_action",
        "--local-only",
        "-c",
        f"test.seed={random_string()}",
        "-c",
        "test.use_incremental=false",
    )
    assert result.stdout == "foo"

    result = await buck.run(
        "root//:basic_incremental_action",
        "--remote-only",
        "-c",
        f"test.seed={random_string()}",
        "-c",
        "test.use_incremental=false",
    )
    assert result.stdout == "foo"

    result = await buck.run(
        "root//:basic_incremental_action",
        "--local-only",
        "-c",
        f"test.seed={random_string()}",
    )
    assert result.stdout == "foo bar"


@buck_test()
# Note that the test scenario below should not actually happen in the real world, it's set up
# this way to ensure that incremental actions are cached properly instead of being re-executed
async def test_basic_incremental_action_cached(buck: Buck) -> None:
    result = await buck.run(
        "root//:basic_incremental_action",
        "--remote-only",
    )
    assert result.stdout == "foo"

    buck.kill()

    result = await buck.run(
        "root//:basic_incremental_action",
        "--local-only",
    )
    # This is the correct/expected behavior because it means that the cached output was used and the action was
    # not re-executed because re-execution would have resulted in the output to be "foo bar". See below
    assert result.stdout == "foo"

    result = await buck.run(
        "root//:basic_incremental_action",
        "--local-only",
        "-c",
        f"test.seed={random_string()}",
    )
    assert result.stdout == "foo bar"


async def incremental_action_interleave_platforms_helper(
    buck: Buck, platform: str, use_content_based_path: bool
) -> BuckResult:
    return await buck.run(
        "root//:basic_incremental_action",
        "--target-platforms",
        platform,
        "--local-only",
        "-c",
        f"test.seed={random_string()}",
        "-c",
        f"test.use_content_based_path={use_content_based_path}",
    )


@buck_test()
async def test_incremental_action_interleave_platforms_aabb(buck: Buck) -> None:
    result = await incremental_action_interleave_platforms_helper(
        buck, "root//:p_default", use_content_based_path=False
    )
    assert result.stdout == "foo"
    result = await incremental_action_interleave_platforms_helper(
        buck, "root//:p_default", use_content_based_path=False
    )
    assert result.stdout == "foo bar"
    result = await incremental_action_interleave_platforms_helper(
        buck, "root//:p_cat", use_content_based_path=False
    )
    assert result.stdout == "foo"
    result = await incremental_action_interleave_platforms_helper(
        buck, "root//:p_cat", use_content_based_path=False
    )
    assert result.stdout == "foo bar"


@buck_test()
async def test_incremental_action_different_platforms_abab(buck: Buck) -> None:
    result = await incremental_action_interleave_platforms_helper(
        buck, "root//:p_default", use_content_based_path=False
    )
    assert result.stdout == "foo"
    result = await incremental_action_interleave_platforms_helper(
        buck, "root//:p_cat", use_content_based_path=False
    )
    assert result.stdout == "foo"
    result = await incremental_action_interleave_platforms_helper(
        buck, "root//:p_default", use_content_based_path=False
    )
    assert result.stdout == "foo bar"
    result = await incremental_action_interleave_platforms_helper(
        buck, "root//:p_cat", use_content_based_path=False
    )
    assert result.stdout == "foo bar"


@buck_test()
async def test_incremental_action_different_platforms_abba(buck: Buck) -> None:
    result = await incremental_action_interleave_platforms_helper(
        buck, "root//:p_default", use_content_based_path=False
    )
    assert result.stdout == "foo"
    result = await incremental_action_interleave_platforms_helper(
        buck, "root//:p_cat", use_content_based_path=False
    )
    assert result.stdout == "foo"
    result = await incremental_action_interleave_platforms_helper(
        buck, "root//:p_cat", use_content_based_path=False
    )
    assert result.stdout == "foo bar"
    result = await incremental_action_interleave_platforms_helper(
        buck, "root//:p_default", use_content_based_path=False
    )
    assert result.stdout == "foo bar"
