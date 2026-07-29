# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

# pyre-strict


import asyncio
import re
from pathlib import Path

from buck2.tests.e2e_util.api.buck import Buck
from buck2.tests.e2e_util.api.buck_result import BuildResult
from buck2.tests.e2e_util.buck_workspace import buck_test, env

# The fixture's `.buckconfig` sets two `DaemonStartupConfig`s:
# `buck2_hydration.enable_paging` (pagable DICE storage on disk) and
# `buck2_hydration.page_out_on_idle` (page the graph out when the daemon goes idle).

# `buck2_data.PageOutStarted::*` — the invocation record serializes the
# `page_out_started` enum field as its integer value (see data.proto).
_PAGE_OUT_STARTED_STARTED = 1
_PAGE_OUT_STARTED_DISABLED_AFTER_ERROR = 9


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
    # Populated by `write_invocation_record=True` on the test; absent/null when 0.
    return int(result.invocation_record().get("page_in_count") or 0)


def _target_output(result: BuildResult, target: str) -> str:
    output = result.get_build_report().output_for_target(f"root//:{target}")
    return Path(output).read_text()


async def _wait_for_page_out_idle(buck: Buck) -> int:
    # `status --wait` blocks until any in-progress idle page-out finishes, so tests
    # observe the settled state without polling. Returns the paged-out node count.
    out = (await buck.debug("hydration", "status", "--wait")).stdout
    paged_out = re.search(r"(\d+) paged out", out)
    in_progress = re.search(r"page-out in progress: (yes|no)", out)
    assert (
        paged_out is not None
        and in_progress is not None
        and in_progress.group(1) == "no"
    ), f"unexpected status output after --wait:\n{out}"
    return int(paged_out.group(1))


@buck_test(data_dir="paging", write_invocation_record=True)
async def test_incremental_build_after_page_out(buck: Buck) -> None:
    # Incremental builds must stay correct after an explicit `buck2 debug
    # hydration page-out`, relying on on-demand page-in during the build. Page-in
    # is measured per command via `page_in_count` in the invocation record.
    #
    # Note: this does not check that stale paged-out values are reclaimed from
    # storage. Garbage-collecting them is future work (reference counting).
    #
    # Pagable storage is set up by `buck2_hydration.enable_paging = true` in the
    # fixture `.buckconfig` (a `DaemonStartupConfig`).
    (buck.cwd / "src.txt").write_text("content-0\n")
    assert _output(await _build(buck)) == "content-0\n"

    # Page the whole graph out to disk; in-memory values are evicted.
    await buck.debug("hydration", "page-out")
    assert await _paged_out_count(buck) > 0, (
        "expected node values to actually be paged out"
    )

    # Rebuild with no changes: served by on-demand page-in of the paged-out
    # values. The per-command page-in total is in the invocation record.
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


@buck_test(data_dir="paging", write_invocation_record=True)
async def test_page_out_frozen_value_into_already_paged_out_heap(
    buck: Buck,
) -> None:
    # Regression test for a page-out serialization panic:
    #   FrozenValue pointer ... not found in any registered heap's chunk index
    #   (starlark-rust/.../pagable/starlark_serialize_context.rs)
    #
    # `module_const_a` and `module_const_b` each hold, in their analysis result, a
    # `FrozenValue` that lives on `rules.bzl`'s shared module heap. The panic only
    # surfaces across *two* page-outs: the first evicts the shared module heap, and
    # the second must serialize the other target's `FrozenValue` into that
    # now-paged-out (unregistered) heap. A single page-out of a fresh graph never
    # trips it, because every reachable heap is still registered.
    await buck.build("//:module_const_a")
    await buck.debug("hydration", "page-out")
    assert await _paged_out_count(buck) > 0, (
        "expected the first target's values (and the shared module heap) to page out"
    )

    # Analyze the second target, then page out again. Serializing its provider's
    # `FrozenValue` into the already-paged-out module heap must not crash.
    await buck.build("//:module_const_b")
    await buck.debug("hydration", "page-out")

    # The second page-out must leave module_const_b hydratable.
    await buck.build("//:module_const_b")


@buck_test(data_dir="paging", write_invocation_record=True)
async def test_page_in_shared_anon_target(buck: Buck) -> None:
    # Bound post-page-out commands because the old typetag mismatch hung hydration.
    command_timeout_seconds = 60
    result = await buck.build("//:uses_anon_a")
    assert _target_output(result, "uses_anon_a") == "anonymous target\n"

    await asyncio.wait_for(
        buck.debug("hydration", "page-out"), timeout=command_timeout_seconds
    )
    paged_out_count = await asyncio.wait_for(
        _paged_out_count(buck), timeout=command_timeout_seconds
    )
    assert paged_out_count > 0, "expected the anonymous target analysis to be paged out"

    # Analyze a new parent that requests the same anonymous target. Unlike a
    # no-op rebuild of the first parent, this has to load the paged-out anonymous
    # target analysis result before it can resolve the promise.
    result = await asyncio.wait_for(
        buck.build("//:uses_anon_b"), timeout=command_timeout_seconds
    )
    assert _target_output(result, "uses_anon_b") == "anonymous target\n"
    assert _paged_in_count(result) > 0, (
        "expected the anonymous target analysis to be paged back in"
    )


@buck_test(data_dir="paging", write_invocation_record=True)
async def test_page_out_on_idle(buck: Buck) -> None:
    # With `buck2_hydration.page_out_on_idle`, the daemon pages the DICE graph out to
    # disk in a background task once it goes idle after a command. Subsequent
    # builds stay correct by paging values back in on demand.
    (buck.cwd / "src.txt").write_text("content-0\n")
    result = await _build(buck)
    assert _output(result) == "content-0\n"
    # The sole active command triggers the idle page-out and records it.
    assert (
        result.invocation_record().get("page_out_started") == _PAGE_OUT_STARTED_STARTED
    ), "expected the sole active command to trigger an idle page-out"

    # Page-out runs in a detached background task once the daemon is idle. Wait
    # for it to finish, then confirm the graph was actually paged out.
    assert await _wait_for_page_out_idle(buck) > 0, (
        "expected idle page-out to actually page values out"
    )

    # The paged-out values must now be paged back in on the next build.
    result = await _build(buck)
    assert _output(result) == "content-0\n"
    paged_in = _paged_in_count(result)
    assert paged_in > 0, (
        f"expected paged-out values to be paged back in, got {paged_in}"
    )

    # Incremental correctness across invalidations (each also schedules an idle
    # page-out): dependent nodes recompute, the rest hydrate on demand.
    (buck.cwd / "src.txt").write_text("content-1\n")
    assert _output(await _build(buck)) == "content-1\n"

    (buck.cwd / "src.txt").write_text("content-2\n")
    assert _output(await _build(buck)) == "content-2\n"


@buck_test(data_dir="paging", write_invocation_record=True)
@env("BUCK2_TEST_FAIL_PAGE_OUT", "true")
async def test_idle_page_out_disabled_after_error(buck: Buck) -> None:
    # A failed idle page-out disables idle page-out for the rest of the daemon's
    # lifetime: the failure is likely persistent, so retrying at the end of every
    # command would repeat the work and re-log the error. `BUCK2_TEST_FAIL_PAGE_OUT`
    # injects the failure on the idle path only.
    (buck.cwd / "src.txt").write_text("content-0\n")
    result = await _build(buck)
    assert _output(result) == "content-0\n"
    # The cold build computes values, so it schedules an idle page-out...
    assert (
        result.invocation_record().get("page_out_started") == _PAGE_OUT_STARTED_STARTED
    ), "the cold build should schedule an idle page-out"

    # ...which then fails (injected). Wait for the background task to settle; it
    # sets the "disabled" flag before releasing the single-flight guard that
    # `status --wait` blocks on, so the flag is observable once this returns.
    await _wait_for_page_out_idle(buck)

    # A rebuild that recomputes values would normally schedule another page-out,
    # but the prior failure has disabled idle page-out, so it must not.
    (buck.cwd / "src.txt").write_text("content-1\n")
    result = await _build(buck)
    assert _output(result) == "content-1\n"
    assert (
        result.invocation_record().get("page_out_started")
        == _PAGE_OUT_STARTED_DISABLED_AFTER_ERROR
    ), "a prior idle page-out failure must disable idle page-out for future commands"


@buck_test(data_dir="paging", write_invocation_record=True)
async def test_page_out_triggered_only_when_values_computed(buck: Buck) -> None:
    # A command triggers an idle page-out only when it computed values worth
    # paging out. The cold build does; a following no-op rebuild does not.
    (buck.cwd / "src.txt").write_text("content-0\n")
    result = await _build(buck)
    assert _output(result) == "content-0\n"
    assert (
        result.invocation_record().get("page_out_started") == _PAGE_OUT_STARTED_STARTED
    ), "the cold build computes values, so it triggers a page-out"
    await _wait_for_page_out_idle(buck)

    result = await _build(buck)
    assert _output(result) == "content-0\n"
    assert (
        result.invocation_record().get("page_out_started") != _PAGE_OUT_STARTED_STARTED
    ), "a no-op rebuild computes nothing new, so it should not trigger a page-out"


@buck_test(data_dir="paging", write_invocation_record=True)
async def test_page_out_at_most_once(buck: Buck) -> None:
    # A value is paged out at most once: once an incremental build pages a value
    # back in (or recomputes it), it stays resident rather than being paged out
    # again.
    (buck.cwd / "src.txt").write_text("content-0\n")
    assert _output(await _build(buck)) == "content-0\n"
    await _wait_for_page_out_idle(buck)

    # This build pages its working set back in and recomputes the affected nodes.
    # Those values were already paged out once, so they stay resident: nothing new
    # to page out, so the build does not trigger a page-out.
    (buck.cwd / "src.txt").write_text("content-1\n")
    result = await _build(buck)
    assert _output(result) == "content-1\n"
    # Precondition: this build actually paged values back in (otherwise the
    # assertion below would hold vacuously).
    assert _paged_in_count(result) > 0, (
        "expected the incremental build to page its working set back in"
    )
    assert (
        result.invocation_record().get("page_out_started") != _PAGE_OUT_STARTED_STARTED
    ), (
        "values paged in by an incremental build must stay resident, leaving "
        "nothing new to page out"
    )

    # The working set stayed resident, so the next incremental build pages nothing
    # back in.
    (buck.cwd / "src.txt").write_text("content-2\n")
    result = await _build(buck)
    assert _output(result) == "content-2\n"
    assert _paged_in_count(result) == 0, (
        f"expected the working set to stay resident, but {_paged_in_count(result)} "
        "values were paged in"
    )
