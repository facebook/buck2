# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

# pyre-strict


from buck2.tests.e2e_util.api.buck import Buck
from buck2.tests.e2e_util.buck_workspace import buck_test, env
from buck2.tests.e2e_util.helper.utils import filter_events, random_string


async def get_action_digest_traces(buck: Buck) -> list[dict[str, object]]:
    """Get all ActionDigestTrace events from the log."""
    return await filter_events(
        buck,
        "Event",
        "data",
        "Instant",
        "data",
        "ActionDigestTrace",
    )


@buck_test()
@env("BUCK2_ACTION_DIGEST_TRACE_LG_SAMPLE_RATE", "0")  # Sample all digests
async def test_action_digest_trace_cache_miss(buck: Buck) -> None:
    """Test that cache miss events are traced."""
    # Make sure action is not cached by using a random input
    with open(buck.cwd / "input.txt", "w") as f:
        f.write(random_string())

    await buck.build(
        "root//:simple",
        "--remote-only",
    )

    traces = await get_action_digest_traces(buck)
    # Should have at least one trace event
    assert len(traces) >= 1, f"Expected at least 1 trace event, got {len(traces)}"

    # Check for cache miss or remote execution events
    events = [t.get("event") for t in traces]
    # Event values: CACHE_HIT=1, CACHE_MISS=2, REMOTE_EXECUTION=3, CACHE_UPLOAD=4
    # With --no-remote-cache we skip cache lookup, so we should see REMOTE_EXECUTION
    assert any(e in [3] for e in events), (
        f"Expected REMOTE_EXECUTION event, got events: {events}"
    )


@buck_test()
@env("BUCK2_ACTION_DIGEST_TRACE_LG_SAMPLE_RATE", "0")  # Sample all digests
async def test_action_digest_trace_cache_hit(buck: Buck) -> None:
    """Test that cache hit events are traced."""
    # First build to populate cache
    with open(buck.cwd / "input.txt", "w") as f:
        f.write(random_string())

    await buck.build(
        "root//:simple",
        "--remote-only",
    )

    await buck.kill()
    # Second build should hit cache
    await buck.build(
        "root//:simple",
        "--remote-only",
    )

    traces = await get_action_digest_traces(buck)
    # Should have at least one trace event
    assert len(traces) >= 1, f"Expected at least 1 trace event, got {len(traces)}"

    # Check for cache hit event
    events = [t.get("event") for t in traces]
    # Event values: CACHE_HIT=1
    assert any(e == 1 for e in events), (
        f"Expected CACHE_HIT event, got events: {events}"
    )


@buck_test()
@env(
    "BUCK2_ACTION_DIGEST_TRACE_LG_SAMPLE_RATE", "64"
)  # Sample rate so high nothing is sampled
async def test_action_digest_trace_sampling(buck: Buck) -> None:
    """Test that sampling works - with very high rate, nothing should be sampled."""
    with open(buck.cwd / "input.txt", "w") as f:
        f.write(random_string())

    await buck.build(
        "root//:simple",
        "--remote-only",
        "--no-remote-cache",
    )

    traces = await get_action_digest_traces(buck)
    # With sampling rate of 64 bits (impossible), should have no traces
    assert len(traces) == 0, (
        f"Expected 0 trace events with high sampling rate, got {len(traces)}"
    )
