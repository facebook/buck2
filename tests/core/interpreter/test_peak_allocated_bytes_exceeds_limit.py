# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

# pyre-strict

import re

from buck2.tests.e2e_util.api.buck import Buck
from buck2.tests.e2e_util.asserts import expect_failure
from buck2.tests.e2e_util.buck_workspace import buck_test
from buck2.tests.e2e_util.helper.golden import golden, sanitize_stderr


def _sanitize_memory(s: str) -> str:
    return re.sub(r"\b\d[\d.]*\s*(?:bytes|[KMGT]i?B)\b", "<USAGE>", s)


@buck_test()
async def test_peak_allocated_bytes_exceeds_limit(buck: Buck) -> None:
    res = await expect_failure(buck.uquery("//:EEE"))
    golden(
        output=_sanitize_memory(sanitize_stderr(res.stderr)),
        rel_path="golden/peak_allocated_bytes_exceeds_limit.golden.stderr",
    )
