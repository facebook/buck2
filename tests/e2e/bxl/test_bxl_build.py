# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

# pyre-strict


import json
import os
from pathlib import Path

from buck2.tests.e2e_util.api.buck import Buck
from buck2.tests.e2e_util.buck_workspace import buck_test


@buck_test(inplace=False, data_dir="bxl/simple")
async def test_bxl_build(buck: Buck) -> None:

    result = await buck.bxl(
        "//bxl/build.bxl:build_test",
        "--",
        "--target",
        ":buildable",
    )
    outputs = json.loads(result.stdout)
    assert (buck.cwd / Path(outputs["root//:buildable"][0])).read_text() == "FOO"

    result = await buck.bxl(
        "//bxl/build.bxl:cquery_build_test",
    )
    outputs = result.stdout.splitlines()[0]
    assert (buck.cwd / Path(outputs)).read_text() == "FOO"


@buck_test(inplace=False, data_dir="bxl/simple")
async def test_bxl_build_stats(buck: Buck) -> None:
    result = await buck.bxl(
        "//bxl/build.bxl:build_stats",
        "--",
        "--targets",
        "root//build/...",
    )
    stats = json.loads(result.stdout)
    assert stats["root//build:pass"]["artifacts"] == 1
    assert stats["root//build:pass"]["failures"] == 0
    assert stats["root//build:fail"]["artifacts"] == 0
    assert stats["root//build:fail"]["failures"] == 1


@buck_test(inplace=False, data_dir="bxl/simple")
async def test_bxl_build_order(buck: Buck) -> None:
    await buck.bxl("//build_artifacts_order/check.bxl:check")


@buck_test(
    inplace=False,
    data_dir="no_materialization_bxl_build",
)
async def test_bxl_build_no_materialization(buck: Buck) -> None:
    result = await buck.bxl(
        "//remote_text.bxl:build",
        "--",
        "--materializations=skip",
    )

    [output] = result.stdout.splitlines()
    assert os.path.exists(buck.cwd / Path(output)) is False

    result = await buck.bxl(
        "//remote_text.bxl:build",
        "--",
        "--materializations=materialize",
    )

    [output] = result.stdout.splitlines()
    assert os.path.exists(buck.cwd / Path(output)) is True
