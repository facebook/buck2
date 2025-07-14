# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

# pyre-strict


from __future__ import annotations

from buck2.tests.e2e_util.api.buck import Buck
from buck2.tests.e2e_util.buck_workspace import buck_test
from buck2.tests.e2e_util.helper.utils import random_string


@buck_test()
async def test_incremental_file_materialized(buck: Buck) -> None:
    result = await buck.run("root//:plate", "-c", f"test.seed={random_string()}")
    assert result.stdout == "0"
    result = await buck.run("root//:plate", "-c", f"test.seed={random_string()}")
    assert result.stdout == "1"


@buck_test()
async def test_incremental_dir_materialized(buck: Buck) -> None:
    result = await buck.run("root//:mate", "-c", f"test.seed={random_string()}")
    assert result.stdout == "0"
    result = await buck.run("root//:mate", "-c", f"test.seed={random_string()}")
    assert result.stdout == "1"


@buck_test()
async def test_incremental_file_not_materialized(buck: Buck) -> None:
    result = await buck.run("root//:flute", "-c", f"test.seed={random_string()}")
    assert result.stdout == "0"
    result = await buck.run("root//:flute", "-c", f"test.seed={random_string()}")
    assert result.stdout == "1"


@buck_test()
async def test_incremental_dir_not_materialized(buck: Buck) -> None:
    result = await buck.run("root//:suite", "-c", f"test.seed={random_string()}")
    assert result.stdout == "0"
    result = await buck.run("root//:suite", "-c", f"test.seed={random_string()}")
    assert result.stdout == "1"
