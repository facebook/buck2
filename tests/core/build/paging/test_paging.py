# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

# pyre-strict


import re
import tempfile
from pathlib import Path

from buck2.tests.e2e_util.api.buck import Buck
from buck2.tests.e2e_util.api.buck_result import BuildResult
from buck2.tests.e2e_util.buck_workspace import buck_test


def _enable_pagable_storage(buck: Buck) -> None:
    db_dir = Path(tempfile.mkdtemp(prefix="dice_db_"))
    # Enable pagable DICE storage. Set before the first command so the daemon
    # starts with it configured (see `configure_dice_for_buck`).
    buck.set_env("BUCK2_DICE_DB_PATH", str(db_dir))
    buck.set_env("PAGABLE_STORAGE_BACKEND", "sqlite")


async def _build(buck: Buck) -> BuildResult:
    return await buck.build("//:mysrcrule")


def _output(result: BuildResult) -> str:
    output = result.get_build_report().output_for_target("root//:mysrcrule")
    return Path(output).read_text()


async def _paged_out_count(buck: Buck) -> int:
    out = (await buck.debug("hydration", "status")).stdout
    match = re.search(r"(\d+) paged out", out)
    assert match is not None, f"unexpected status output:\n{out}"
    return int(match.group(1))


def _paged_in_count(result: BuildResult) -> int:
    # Populated by `write_invocation_record=True` on the test.
    return int(result.invocation_record().get("page_in_count", 0))


@buck_test(data_dir="paging", write_invocation_record=True)
async def test_incremental_build_after_page_out(buck: Buck) -> None:
    # Incremental builds must stay correct after values are paged out to disk,
    # relying on on-demand page-in during the build (no manual page-in). Page-in
    # is measured per command via `page_in_count` in the invocation record.
    #
    # Note: this does not check that stale paged-out values are reclaimed from
    # storage. Garbage-collecting them is future work (reference counting).
    _enable_pagable_storage(buck)

    (buck.cwd / "src.txt").write_text("content-0\n")
    assert _output(await _build(buck)) == "content-0\n"

    # Page the whole graph out to disk; in-memory values are evicted.
    await buck.debug("hydration", "page-out")
    assert await _paged_out_count(buck) > 0, (
        "expected node values to actually be paged out"
    )

    # Rebuild with no changes: served entirely by on-demand page-in of the
    # paged-out values. The per-command page-in total is in the invocation record.
    result = await _build(buck)
    assert _output(result) == "content-0\n"
    paged_in = _paged_in_count(result)
    assert paged_in > 0, (
        f"expected values to be paged back in during rebuild, got {paged_in}"
    )

    # Invalidate: dependent nodes recompute, the rest hydrate on demand.
    (buck.cwd / "src.txt").write_text("content-1\n")
    assert _output(await _build(buck)) == "content-1\n"

    # Page out again (now including the recomputed values) and invalidate once
    # more, to confirm repeated page-out cycles keep producing correct results.
    await buck.debug("hydration", "page-out")
    assert await _paged_out_count(buck) > 0, (
        "expected node values to be paged out again"
    )
    (buck.cwd / "src.txt").write_text("content-2\n")
    assert _output(await _build(buck)) == "content-2\n"
