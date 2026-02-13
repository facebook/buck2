# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

# pyre-strict

from buck2.tests.e2e_util.api.buck import Buck
from buck2.tests.e2e_util.asserts import expect_failure
from buck2.tests.e2e_util.buck_workspace import buck_test, env


@buck_test()
async def test_validation_affects_build_command(buck: Buck) -> None:
    await expect_failure(
        buck.build(":plate"),
        stderr_regex="""
Validation for `prelude//:mate \\(<unspecified>\\)` failed:

Here I am describing the failure reason

Full validation result is located at""",
    )
    await buck.build(":date")


@buck_test(write_invocation_record=True)
async def test_validation_affects_run_command(buck: Buck) -> None:
    res = await expect_failure(
        buck.run(
            ":plate",
        ),
        stderr_regex="""
Validation for `prelude//:mate \\(<unspecified>\\)` failed:

Here I am describing the failure reason

Full validation result is located at""",
    )

    record = res.invocation_record()
    assert len(record["errors"]) == 1

    await buck.run(":date")


@buck_test(write_invocation_record=True)
@env("BUCK2_ALLOW_INTERNAL_TEST_RUNNER_DO_NOT_USE", "1")
async def test_validation_affects_test_command(buck: Buck) -> None:
    res = await expect_failure(
        buck.test(
            ":plate",
            test_executor="",
        ),
        stderr_regex="""
Validation for `prelude//:mate \\(<unspecified>\\)` failed:

Here I am describing the failure reason

Full validation result is located at""",
    )

    record = res.invocation_record()
    assert len(record["errors"]) == 1

    await buck.test(":date", test_executor="")


@buck_test(write_invocation_record=True)
async def test_validation_affects_install_command(buck: Buck) -> None:
    res = await expect_failure(
        buck.install(
            ":plate",
        ),
        stderr_regex="Validation for `prelude//:mate \\(<unspecified>\\)` failed",
    )

    record = res.invocation_record()
    assert len(record["errors"]) == 1

    # It's too complicated to set up installer properly.
    # We intentionally fail on the installer side, but interpret
    # an attempt to run it as a successful verification.
    res = await expect_failure(
        buck.install(
            ":date",
        ),
        stderr_regex="Installer: Incoming connection accepted, now closing it",
    )

    record = res.invocation_record()
    assert len(record["errors"]) == 1


@buck_test()
async def test_optional_validation(buck: Buck) -> None:
    await buck.build(":optional_passing")

    # Optional validations are not run by default.
    await buck.build(":optional_failing")

    # Expect a failure when run with --enable-optional-validations.
    await expect_failure(
        buck.build(":optional_failing", "--enable-optional-validations", "whistle"),
        stderr_regex="Validation for `.+` failed",
    )
