# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

# pyre-strict


import json
from pathlib import Path

from buck2.tests.e2e_util.api.buck import Buck
from buck2.tests.e2e_util.asserts import expect_failure
from buck2.tests.e2e_util.buck_workspace import buck_test
from buck2.tests.e2e_util.helper.utils import random_string, read_what_ran


@buck_test()
async def test_executor_with_dependencies(buck: Buck) -> None:
    # Smoke test: run on RE and correctly pass the `remote_execution_dependencies` parameter specified in the platform
    # The RE external dependency (https://fburl.com/wiki/e55nloow) is purposefully wrong as the smc_tier is non existent,
    # We just want to check that the parameter is passed correctly to RE
    await expect_failure(
        buck.build(
            ":target_without_dependencies",
            "-c",
            "build.execution_platforms=root//platforms:platforms_with_dependencies",
            "-c",
            f"test.cache_buster={random_string()}",
        ),
        # Full error message looks like this: P1217423393
        stderr_regex='facebook::remote_execution::scheduler::TaskCancelledException: Error acquiring dependency TaskDependencyRequest { dependency: TDependency { smc_tier: "buck2_smoke_test_tier", id: "dep_a"',
    )


@buck_test()
async def test_good_target_with_dependencies(buck: Buck) -> None:
    result = await buck.build(
        ":good_target_with_dependencies",
        "-c",
        "build.execution_platforms=root//platforms:platforms_without_dependencies",
        "-c",
        f"test.cache_buster={random_string()}",
        "--show-full-output",
    )
    output_dict = result.get_target_to_build_output()

    for _target, output in output_dict.items():
        with Path(output).open() as f:
            deps = json.load(f)
        assert len(deps) == 1
        assert deps[0]["smc_tier"] == "noop"
        assert deps[0]["id"] == "foo"
        # reservation_id is a random string which is 20 characters long
        assert len(deps[0]["reservation_id"]) == 20

    # Make sure it actually did run on RE.
    out = await read_what_ran(buck)
    executors = {line["identity"]: line["reproducer"]["executor"] for line in out}
    expected = {
        "root//:good_target_with_dependencies (<unspecified>) (cp)": "Re",
    }
    assert executors == expected


@buck_test()
async def test_bad_target_with_dependencies(buck: Buck) -> None:
    await expect_failure(
        buck.build(
            ":bad_target_with_dependencies",
            "-c",
            "build.execution_platforms=root//platforms:platforms_without_dependencies",
            "-c",
            f"test.cache_buster={random_string()}",
        ),
        stderr_regex="error: too many fields set for RE dependency: `extra_field, id, smc_tier`",
    )
