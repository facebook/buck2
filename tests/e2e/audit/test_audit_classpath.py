# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

# pyre-strict

# FIXME(JakobDegen): These tests should be isolated and moved into `tests/isolated/audit`

from pathlib import Path
from typing import Iterable, Set

from buck2.tests.e2e_util.api.buck import Buck
from buck2.tests.e2e_util.asserts import expect_failure
from buck2.tests.e2e_util.buck_workspace import buck_test


def _classpath_jars(classpaths: Iterable[str]) -> Set[str]:
    return {Path(p).name for p in classpaths}


@buck_test(inplace=True, skip_for_os=["windows"])
async def test_audit_classpath(buck: Buck) -> None:
    await expect_failure(
        buck.audit("classpath", "fbsource//fbandroid/buck2/tests/good/classpath:top"),
        stderr_regex=r"Using `audit classpath` is no longer supported. Use the `\[classpath\]` or `\[classpath_targets\]` sub-targets instead.",
    )


@buck_test(inplace=True, skip_for_os=["windows"])
async def test_audit_classpath_binary(buck: Buck) -> None:
    await expect_failure(
        buck.audit("classpath", "fbsource//fbandroid/buck2/tests/good/classpath:apk"),
        stderr_regex=r"Using `audit classpath` is no longer supported. Use the `\[classpath\]` or `\[classpath_targets\]` sub-targets instead.",
    )


@buck_test(inplace=True, skip_for_os=["windows"])
async def test_audit_classpath_json(buck: Buck) -> None:
    top = "fbsource//fbandroid/buck2/tests/good/classpath:top"
    direct_dep = "fbsource//fbandroid/buck2/tests/good/classpath:direct_dep"

    await expect_failure(
        buck.audit("classpath", top, direct_dep, "--json"),
        stderr_regex=r"Using `audit classpath` is no longer supported. Use the `\[classpath\]` or `\[classpath_targets\]` sub-targets instead.",
    )
