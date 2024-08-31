#!/usr/bin/env fbpython
# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

# pyre-strict


from buck2.tests.e2e_util.api.buck import Buck
from buck2.tests.e2e_util.asserts import expect_failure
from buck2.tests.e2e_util.buck_workspace import buck_test


_DATA_PATTERN_PREFIX: str = "fbcode//buck2/tests/e2e/build/test_build_missing_data"


@buck_test(inplace=True)
async def test_build_skip_missing(buck: Buck) -> None:
    result = await buck.build(
        f"{_DATA_PATTERN_PREFIX}:existing",
        f"{_DATA_PATTERN_PREFIX}:missing",
        f"--target-platforms={_DATA_PATTERN_PREFIX}:p",
        "--skip-missing-targets",
    )

    out = (
        result.get_build_report()
        .output_for_target(f"{_DATA_PATTERN_PREFIX}:existing")
        .read_text()
    )
    assert "existing" == out.strip()
    assert "Skipped 1 missing targets:" in result.stderr


@buck_test(inplace=True)
async def test_build_skip_missing_fails_on_missing_package(buck: Buck) -> None:
    await expect_failure(
        buck.build(
            f"{_DATA_PATTERN_PREFIX}:existing",
            f"{_DATA_PATTERN_PREFIX}/bad-package:existing",
            f"--target-platforms={_DATA_PATTERN_PREFIX}:p",
            "--skip-missing-targets",
        ),
        stderr_regex=f"`{_DATA_PATTERN_PREFIX}/bad-package`",
    )
