# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

# pyre-strict


from buck2.tests.e2e_util.api.buck import Buck
from buck2.tests.e2e_util.buck_workspace import buck_test
from buck2.tests.e2e_util.helper.utils import expect_exec_count, random_string


@buck_test(inplace=True)
async def test_remote_worker(buck: Buck) -> None:
    target = "fbcode//buck2/tests/targets/rules/remote_worker:run_two_worker_rules"
    result = await buck.build(
        target,
        "-c",
        f"test.cache_buster={random_string()}",
    )

    output = result.get_build_report().output_for_target(target)

    output_path = buck.cwd / output
    with open(output_path, "r") as f:
        output_lines = f.readlines()
        assert len(output_lines) == 2
        # We would like to check that both lines are the same, as that means that
        # both actions used the same persistent worker.
        # However, RE just does a best effort to use the same persistent worker,
        # so we can't guarantee this is the case.
        # assert output_lines[0].strip() == output_lines[1].strip()


@buck_test(inplace=True)
async def test_remote_worker_caches(buck: Buck) -> None:
    target = "fbcode//buck2/tests/targets/rules/remote_worker:run_remote_worker_1"
    args = [
        target,
    ]
    await buck.build(*args)

    buck.kill()

    await buck.build(*args)
    await expect_exec_count(buck, 0)
