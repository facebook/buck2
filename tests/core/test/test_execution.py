# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

# pyre-strict

import os

from buck2.tests.e2e_util.api.buck import Buck
from buck2.tests.e2e_util.asserts import expect_failure
from buck2.tests.e2e_util.buck_workspace import buck_test, BuckTestCase
from buck2.tests.e2e_util.helper.utils import random_string, read_what_ran


class ExecutionTest(BuckTestCase):
    @buck_test()
    async def test_stable_action_digest_with_deterministic_paths(
        self, buck: Buck
    ) -> None:
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

        assert first_digests[0] == second_digests[0], (
            f"Test action digests differ between runs: {first_digests[0]} vs {second_digests[0]}"
        )

    @buck_test()
    async def test_stress_runs_have_different_action_digests(self, buck: Buck) -> None:
        await buck.test(
            "-c",
            "test.local_enabled=false",
            "-c",
            "test.remote_enabled=true",
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
    async def test_remote_test_execution_cached(self, buck: Buck) -> None:
        args = [
            "-c",
            "test.local_enabled=false",
            "-c",
            "test.remote_enabled=true",
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

    @buck_test()
    async def test_remote_test_execution_not_cached_for_stress_runs(
        self, buck: Buck
    ) -> None:
        args = [
            "-c",
            "test.local_enabled=false",
            "-c",
            "test.remote_enabled=true",
            "//:cacheable_test",
            "--",
            "--stress-runs",
            "2",
        ]

        await buck.test(*args)

        await buck.test(*args)
        what_ran = await read_what_ran(buck)
        test_runs = [entry for entry in what_ran if entry["reason"] == "test.run"]
        assert len(test_runs) == 2, (
            f"Expected exactly 2 test.run entries for stress runs, got {len(test_runs)}"
        )

        # Stress runs disable caching — even on the second invocation, both runs
        # should execute remotely rather than hitting the cache.
        for entry in test_runs:
            executor = entry.get("reproducer", {}).get("executor", "")
            assert executor == "Re", (
                f"Expected Re executor for stress runs, got: {executor}"
            )

    @buck_test()
    async def test_local_test_execution_not_cached(self, buck: Buck) -> None:
        seed = random_string()
        args = [
            "-c",
            "test.local_enabled=true",
            "-c",
            "test.remote_enabled=false",
            "-c",
            f"test.seed={seed}",
            "//:cacheable_test",
        ]

        await buck.test(*args)

        await buck.test(*args)
        second_what_ran = await read_what_ran(buck)
        second_test_runs = [
            entry for entry in second_what_ran if entry["reason"] == "test.run"
        ]
        assert len(second_test_runs) == 1, (
            f"Expected exactly one test.run entry, got {len(second_test_runs)}"
        )
        assert second_test_runs[0]["reproducer"]["executor"] == "Local", (
            "Expected test to run locally, not be cached!"
        )

    @buck_test()
    async def test_remote_test_execution_not_cached_with_no_remote_cache(
        self,
        buck: Buck,
    ) -> None:
        args = [
            "-c",
            "test.local_enabled=false",
            "-c",
            "test.remote_enabled=true",
            "--no-remote-cache",
            "//:cacheable_test",
        ]

        await buck.test(*args)

        await buck.test(*args)
        second_what_ran = await read_what_ran(buck)
        second_test_runs = [
            entry for entry in second_what_ran if entry["reason"] == "test.run"
        ]
        assert len(second_test_runs) == 1, (
            f"Expected exactly one test.run entry, got {len(second_test_runs)}"
        )
        assert second_test_runs[0]["reproducer"]["executor"] == "Re", (
            "Expected test to run remotely, not be cached!"
        )

    @buck_test()
    async def test_remote_test_execution_not_cached_with_disable_flag(
        self,
        buck: Buck,
    ) -> None:
        args = [
            "-c",
            "test.local_enabled=false",
            "-c",
            "test.remote_enabled=true",
            "//:cacheable_test",
            "--",
            "--disable-test-execution-caching",
        ]

        await buck.test(*args)

        await buck.test(*args)
        second_what_ran = await read_what_ran(buck)
        second_test_runs = [
            entry for entry in second_what_ran if entry["reason"] == "test.run"
        ]
        assert len(second_test_runs) == 1, (
            f"Expected exactly one test.run entry, got {len(second_test_runs)}"
        )
        assert second_test_runs[0]["reproducer"]["executor"] == "Re", (
            "Expected test to run remotely, not be cached!"
        )

    @buck_test()
    async def test_outputs_of_a_local_run_are_on_disk(self, buck: Buck) -> None:
        # The test runner reads a test's outputs out of process, so after a local run Buck asks
        # the materializer for them by value; the only copy of what the test wrote is the one on
        # disk.
        run = buck.test(
            "-c",
            "test.local_enabled=true",
            "-c",
            "test.remote_enabled=false",
            "//:writes_output",
        )
        if os.environ.get("BUCK2_TEST_MATERIALIZER") == "v2":
            # The disk-state core goes to the CAS for what only the test's own run put on disk.
            await expect_failure(run, stderr_regex="Error materializing test outputs")
            return
        await run
        written = sorted(
            p.read_text()
            for p in (buck.cwd / "buck-out" / "v2" / "test").rglob("written_by_test")
        )
        assert written == ["list", "run"]
