# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

# pyre-strict

from buck2.tests.e2e_util.api.buck import Buck
from buck2.tests.e2e_util.buck_workspace import buck_test
from buck2.tests.e2e_util.helper.utils import read_what_ran


@buck_test()
async def test_unstable_action_digest(buck: Buck) -> None:
    args = [
        "-c",
        "test.local_enabled=false",
        "-c",
        "test.remote_enabled=true",
        "//:test",
    ]

    await buck.test(*args)
    first_what_ran = await read_what_ran(buck)
    first_digests = [
        entry["reproducer"]["details"]["digest"]
        for entry in first_what_ran
        if entry["reason"] == "test.run"
    ]
    assert len(first_digests) == 1, "Expected one test.run entry"

    await buck.test(*args)
    second_what_ran = await read_what_ran(buck)
    second_digests = [
        entry["reproducer"]["details"]["digest"]
        for entry in second_what_ran
        if entry["reason"] == "test.run"
    ]
    assert len(second_digests) == 1, "Expected one test.run entry"

    assert first_digests[0] != second_digests[0], (
        f"Test action digests do not differ between runs: {first_digests[0]}"
    )


@buck_test()
async def test_stable_action_digest_with_deterministic_paths(buck: Buck) -> None:
    args = [
        "-c",
        "test.local_enabled=false",
        "-c",
        "test.remote_enabled=true",
        "-c",
        "buck2.use_deterministic_test_execution_paths=true",
        "//:test",
    ]

    await buck.test(*args)
    first_what_ran = await read_what_ran(buck)
    first_digests = [
        entry["reproducer"]["details"]["digest"]
        for entry in first_what_ran
        if entry["reason"] == "test.run"
    ]
    assert len(first_digests) == 1, "Expected one test.run entry"

    await buck.test(*args)
    second_what_ran = await read_what_ran(buck)
    second_digests = [
        entry["reproducer"]["details"]["digest"]
        for entry in second_what_ran
        if entry["reason"] == "test.run"
    ]
    assert len(second_digests) == 1, "Expected one test.run entry"

    assert first_digests[0] == second_digests[0], (
        f"Test action digests differ between runs: {first_digests[0]} vs {second_digests[0]}"
    )


@buck_test()
async def test_stress_runs_have_different_action_digests(buck: Buck) -> None:
    await buck.test(
        "-c",
        "test.local_enabled=false",
        "-c",
        "test.remote_enabled=true",
        "-c",
        "buck2.use_deterministic_test_execution_paths=true",
        "//:test",
        "--",
        "--stress-runs",
        "2",
    )
    what_ran = await read_what_ran(buck)
    test_runs = [entry for entry in what_ran if entry["reason"] == "test.run"]
    assert len(test_runs) == 2, (
        f"Expected exactly 2 test.run entries for stress runs, got {len(test_runs)}"
    )

    digests = [entry["reproducer"]["details"]["digest"] for entry in test_runs]
    assert digests[0] != digests[1], (
        f"Stress run action digests should differ but were both: {digests[0]}"
    )


@buck_test()
async def test_remote_test_execution_cached(buck: Buck) -> None:
    args = [
        "-c",
        "test.local_enabled=false",
        "-c",
        "test.remote_enabled=true",
        "-c",
        "buck2.use_deterministic_test_execution_paths=true",
        "//:cacheable_test",
    ]

    await buck.test(*args)

    await buck.test(*args)
    second_what_ran = await read_what_ran(buck, "--emit-cache-queries")
    second_test_runs = [
        entry
        for entry in second_what_ran
        if entry["reason"] == "test.run"
        and entry.get("reproducer", {}).get("executor") == "Cache"
    ]
    assert len(second_test_runs) == 1, (
        f"Expected exactly one cached test.run entry, got {len(second_test_runs)}"
    )
