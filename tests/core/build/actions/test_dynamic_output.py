# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

# pyre-strict


import asyncio
import fileinput
import time

from buck2.tests.e2e_util.api.buck import Buck
from buck2.tests.e2e_util.asserts import expect_failure
from buck2.tests.e2e_util.buck_workspace import buck_test
from buck2.tests.e2e_util.helper.golden import golden, sanitize_stderr


@buck_test(data_dir="everything")
async def test_dynamic_output(buck: Buck) -> None:
    await buck.build("root//:")


@buck_test(data_dir="everything_new")
async def test_dynamic_output_new(buck: Buck) -> None:
    await buck.build("root//:")


@buck_test(data_dir="empty_dynamic_list")
async def test_empty_dynamic_list(buck: Buck) -> None:
    await buck.build("root//:empty_test")


@buck_test(data_dir="artifact_eq_bug")
async def test_artifact_eq_bug(buck: Buck) -> None:
    await buck.build("root//:bug")


@buck_test(data_dir="many_rebound_outputs", skip_for_os=["windows"])
async def test_many_rebound_outputs_incremental_rebuild(buck: Buck) -> None:
    """
    Every artifact re-bound through a dynamic_output gets its own action key
    that redirects to the producing action and stores that action's *entire*
    `ActionOutputs` as its dice value. On an incremental rebuild all of those
    keys recompute, and DICE equality-compares each one's old and new value on
    its serialized core state thread. If that comparison is not O(1), the
    rebuild does O(N^2) work serialized on one thread (N per key, N keys); for
    real dist-ThinLTO links (~64k outputs on the index action) that stalled
    rebuilds for over ten minutes.

    The rebuild below does strictly less work than the initial build, so we can
    use the initial build to calibrate for host speed. With the O(N^2) behavior
    the rebuild is one to two orders of magnitude slower than the initial build
    instead.
    """
    start = time.monotonic()
    await buck.build("root//:check")
    first = time.monotonic() - start

    # Invalidate the analysis (and with it every action key). The seed changes
    # the content of exactly one of the dynamic action's outputs (see the
    # fixture for why that matters).
    with fileinput.input(buck.cwd / "TARGETS.fixture", inplace=True) as f:
        for line in f:
            print(line.replace('seed = "A"', 'seed = "B"'), end="")

    # Once the rebuild hits this threshold the test's outcome is decided, so
    # don't wait for it to finish: with the O(N^2) behavior that would take
    # many more minutes and hit the 10 minute test level timeout, which our
    # test infra handles poorly.
    threshold = max(60.0, 3.0 * first)
    start = time.monotonic()
    timed_out = False
    try:
        await asyncio.wait_for(buck.build("root//:check"), timeout=threshold)
    except asyncio.TimeoutError:
        timed_out = True
        await buck.kill()
    second = time.monotonic() - start

    assert not timed_out and second < threshold, (
        f"incremental rebuild took {second:.1f}s vs {first:.1f}s for the "
        "initial build; dice is likely doing deep ActionOutputs comparisons"
    )


@buck_test(data_dir="analysis_failure")
async def test_dynamic_output_analysis_failure(buck: Buck) -> None:
    result = await expect_failure(
        buck.build("root//:analysis_failure"),
        stderr_regex="Analysis failed: this is a test failure message",
    )
    golden(
        output=sanitize_stderr(result.stderr),
        rel_path="analysis_failure/golden/test_dynamic_output_analysis_failure.golden.txt",
    )
