# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

# pyre-strict


import json
import re
from pathlib import Path

from buck2.tests.e2e_util.api.buck import Buck
from buck2.tests.e2e_util.asserts import expect_failure
from buck2.tests.e2e_util.buck_workspace import buck_test, BuckTestCase
from buck2.tests.e2e_util.helper.golden import golden


def _replace_hash(s: str) -> str:
    return re.sub(r"\b[0-9a-f]{16}\b", "<HASH>", s)


class DynamicTest(BuckTestCase):
    @buck_test()
    async def test_bxl_dynamic_action(self, buck: Buck) -> None:
        result = await buck.bxl(
            "//:dynamic.bxl:dynamic_test",
        )
        outputs = result.stdout.strip()
        assert Path(outputs).read_text() == "content"

    @buck_test()
    async def test_bxl_dynamic_with_bxl_ctx(self, buck: Buck) -> None:
        result = await buck.bxl(
            "//:dynamic.bxl:dynamic_test_with_bxl_ctx",
        )

        outputs = json.loads(result.stdout)
        golden_result = {}
        for k, v in outputs.items():
            golden_result.update({k: _replace_hash(Path(v).read_text())})

        golden(
            output=json.dumps(golden_result, indent=2),
            rel_path="happy_path_dynamic_ctx.golden.json",
        )

    @buck_test()
    async def test_bxl_dynamic_cannot_access_exec_deps(self, buck: Buck) -> None:
        await expect_failure(
            buck.bxl("//:dynamic.bxl:dynamic_test_execution_resolution"),
            stderr_regex="Anon targets and dynamic actions cannot access `bxl.Actions.exec_deps`",
        )

    @buck_test()
    async def test_bxl_dynamic_incompatible_targets(self, buck: Buck) -> None:
        result = await buck.bxl(
            "//:dynamic.bxl:dynamic_test_incompatible_targets",
        )

        assert "Skipped 1 incompatible targets" in result.stderr
