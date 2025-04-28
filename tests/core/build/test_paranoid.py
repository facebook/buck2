# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

# pyre-strict


import asyncio
import json
import random
import string
from pathlib import Path
from typing import Any, Dict, List

from buck2.tests.e2e_util.api.buck import Buck
from buck2.tests.e2e_util.asserts import expect_failure
from buck2.tests.e2e_util.buck_workspace import buck_test

# FIXME(JakobDegen): Give these tests their own data dir, instead of sharing one

# TODO(T184317763): either those tests are flaky or paranoid mode is broken,
# to repro uncomment and run `buck2 test '@fbcode//mode/opt-asan' fbcode//buck2/tests/e2e/build:test_paranoid -- --exact 'buck2/tests/e2e/build:test_paranoid - test_paranoid.py::test_paranoid_allows_fallback_after_re_failure' --run-disabled`
# @buck_test(
#     data_dir="execution_platforms",
#     skip_for_os=["windows"],
# )
# @env("BUCK2_TEST_FAIL_RE_DOWNLOADS", "true")
# async def test_paranoid_allows_fallback_after_re_failure(
#     buck: Buck,
# ) -> None:
#     """
#     Currently, this is only supported in paranoid mode. This is a smaller issue
#     than it might seem, since it only enters the picture if RE claims, cancels
#     local, and *then* we need a fallback, which is fairly unlikely since most
#     of the work is done by then, and also if that fails, there's no reason to
#     expect deferred materialization won't fail later, which is a much bigger
#     (and much more irrecoverable) problem.
#     """

#     def args():
#         return [
#             "root//executor_race_tests:slower_locally_and_works_on_both",
#             "-c",
#             f"test.cache_buster={random_string()}",
#         ]

#     await expect_failure(
#         buck.build(*args()),
#         stderr_regex="Injected error",
#     )

#     await buck.build(
#         *args(),
#         env={"BUCK_PARANOID": "true"},
#     )


@buck_test(
    data_dir="execution_platforms",
    skip_for_os=["windows"],
)
async def test_paranoid_ignores_preferences(
    buck: Buck,
) -> None:
    def args() -> List[str]:
        return [
            "root//executor_race_tests:fails_slow_on_re_works_locally_prefer_remote",
            "-c",
            f"test.cache_buster={random_string()}",
        ]

    await expect_failure(
        buck.build(*args()),
        stderr_regex="Remote command returned non-zero exit code 1",
    )

    await buck.build(
        *args(),
        env={"BUCK_PARANOID": "true"},
    )


# TODO(T184317763): either those tests are flaky or paranoid mode is broken,
# to repro uncomment and run `buck2 test '@fbcode//mode/opt-asan' fbcode//buck2/tests/e2e/build:test_paranoid -- --exact 'buck2/tests/e2e/build:test_paranoid - test_paranoid.py::test_paranoid_forces_fallback_on_failure' --run-disabled`
# @buck_test(
#     data_dir="execution_platforms",
#     skip_for_os=["windows"],
# )
# async def test_paranoid_forces_fallback_on_failure(
#     buck: Buck,
# ) -> None:
#     def args():
#         return [
#             "root//executor_race_tests:slower_and_works_only_locally_local_not_preferred",
#             "-c",
#             f"test.cache_buster={random_string()}",
#         ]

#     await expect_failure(
#         buck.build(*args()),
#         stderr_regex="Remote command returned non-zero exit code 1",
#     )

#     await buck.build(
#         *args(),
#         env={"BUCK_PARANOID": "true"},
#     )


@buck_test(
    data_dir="execution_platforms",
    skip_for_os=["windows"],
)
async def test_paranoid_ignores_low_pass_filter(
    buck: Buck,
) -> None:
    def args() -> List[str]:
        return [
            "root//executor_race_tests:fails_slow_on_re_works_locally_heavyweight",
            "-c",
            f"test.cache_buster={random_string()}",
            "-c",
            "test.experimental_low_pass_filter=true",
        ]

    await expect_failure(
        buck.build(*args()),
        stderr_regex="Remote command returned non-zero exit code 1",
    )

    await buck.build(
        *args(),
        env={"BUCK_PARANOID": "true"},
    )


@buck_test(
    data_dir="execution_platforms",
)
async def test_paranoid_enable_disable(
    buck: Buck,
    tmp_path: Path,
) -> None:
    env = {"BUCK2_PARANOID_PATH": str(tmp_path / "paranoid.info")}

    # Start the daemon
    await buck.build(env=env)

    async def config() -> Dict[str, Any]:
        status = (await buck.status()).stdout
        config = json.loads(status)["daemon_constraints"]["daemon_startup_config"]
        print(config)
        return json.loads(config)

    # Its not paranoid.
    assert not (await config())["paranoid"]

    # Still not, we didn't run any commands, so no restart.
    await buck.debug("paranoid", "enable", env=env)
    assert not (await config())["paranoid"]

    # Run a command, should restart and enable.
    await buck.build(env=env)
    assert (await config())["paranoid"]

    # Turn it off
    await buck.debug("paranoid", "disable", env=env)
    await buck.build(env=env)
    assert not (await config())["paranoid"]

    # Turn it back on, but not for long.
    await buck.debug("paranoid", "enable", "--ttl", "10s", env=env)
    await buck.build(env=env)
    assert (await config())["paranoid"]

    # Wait until it turns off.
    await asyncio.sleep(15)
    await buck.build(env=env)
    assert not (await config())["paranoid"]


@buck_test(data_dir="execution_platforms")
async def test_noop(buck: Buck) -> None:
    return


def random_string() -> str:
    return "".join(random.choice(string.ascii_lowercase) for i in range(256))
