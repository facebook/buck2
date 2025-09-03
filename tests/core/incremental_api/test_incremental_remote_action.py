# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

# pyre-strict


from __future__ import annotations

import json

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


@buck_test()
async def test_remote_cache_is_used(buck: Buck) -> None:
    seed = random_string()
    result = await buck.run("root//:plate", "-c", f"test.seed={seed}")
    assert result.stdout == "0"
    result = await buck.run("root//:plate", "-c", f"test.seed={random_string()}")
    assert result.stdout == "1"

    # For the next build with already used seed we expect the action to be taken from the cache
    result = await buck.run("root//:plate", "-c", f"test.seed={seed}")
    assert result.stdout == "0"

    out = await buck.log("what-ran", "--format", "json")
    out = [line.strip() for line in out.stdout.splitlines()]
    out = [json.loads(line) for line in out if line]
    assert len(out) == 1, "out should have 1 line: `{}`".format(out)
    repro = out[0]
    assert repro["reproducer"]["executor"] == "Cache"
