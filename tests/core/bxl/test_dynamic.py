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
from buck2.tests.e2e_util.buck_workspace import buck_test
from buck2.tests.e2e_util.helper.golden import golden


def _replace_hash(s: str) -> str:
    return re.sub(r"\b[0-9a-f]{16}\b", "<HASH>", s)


@buck_test()
async def test_bxl_dynamic_action(buck: Buck) -> None:
    result = await buck.bxl(
        "//:dynamic.bxl:dynamic_test",
    )
    outputs = result.stdout.strip()
    assert Path(outputs).read_text() == "content"


@buck_test()
async def test_bxl_dynamic_with_bxl_ctx(buck: Buck) -> None:
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


# Very simple test that the exec_deps/toolchains get propagatd to the dynamic bxl_ctx correctly
@buck_test(allow_soft_errors=True)
async def test_bxl_dynamic_execution_resolution(buck: Buck) -> None:
    result = await buck.bxl(
        "//:dynamic.bxl:dynamic_test_execution_resolution",
    )

    outputs = json.loads(result.stdout)

    assert Path(outputs["dynamic"]).read_text() == Path(outputs["root"]).read_text()


@buck_test()
async def test_bxl_dynamic_incompatible_targets(buck: Buck) -> None:
    result = await buck.bxl(
        "//:dynamic.bxl:dynamic_test_incompatible_targets",
    )

    assert "Skipped 1 incompatible targets" in result.stderr
