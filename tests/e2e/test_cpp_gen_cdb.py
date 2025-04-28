# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

# pyre-strict


import json
import platform
from pathlib import Path

from buck2.tests.e2e_util.api.buck import Buck
from buck2.tests.e2e_util.buck_workspace import buck_test


# TODO(marwhal): Fix and enable on Windows
@buck_test(inplace=True, skip_for_os=["windows"])
async def test_no_quotes(buck: Buck) -> None:
    result = await buck.bxl(
        "fbcode//tools/build/buck/bxl/cpp_lsp/cpp_gen_cdb.bxl:cpp_gen_cdb",
        "--",
        "--filename",
        str(
            buck.cwd.parent
            / "fbcode/buck2/tests/targets/cpp_gen_cdb/basic/src/main.cpp"
        ),
        "--os",
        platform.system().lower(),
    )
    outputs = json.loads(result.stdout)
    compdb_path = Path(outputs["compilationDatabasePath"]) / ".." / "compdb.json"

    with open(compdb_path) as f:
        commands = json.load(f)

    # check that the define is present without any shell quotes
    arguments = commands[0]["arguments"]
    assert arguments.index("-DM_FOO_BAR=1")


# TODO(marwhal): Add this back one at least one test in this file passes on Windows
@buck_test(inplace=True)
async def test_noop(buck: Buck) -> None:
    return
