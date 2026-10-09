# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

# pyre-strict

import io
import unittest

import buck_test_main


class _NoCoverage:
    def get_coverage(self) -> None:
        return None


def _subtests_case() -> tuple[type[unittest.TestCase], list[bool]]:
    after_skip: list[bool] = []

    class Subtests(unittest.TestCase):
        def test_subtest_fails(self) -> None:
            for i in range(2):
                with self.subTest(i=i):
                    self.assertEqual(i, 0, "value %d is not zero" % i)

        def test_subtest_skips(self) -> None:
            with self.subTest(case="unsupported"):
                self.skipTest("platform not supported")
            after_skip.append(True)

    return Subtests, after_skip


def _results_of(case: type[unittest.TestCase]) -> dict[str, dict[str, object]]:
    suite = unittest.defaultTestLoader.loadTestsFromTestCase(case)
    result = buck_test_main.BuckTestResult(
        io.StringIO(), False, 0, False, _NoCoverage(), suite
    )
    suite.run(result)
    return {str(entry["testCase"]): entry for entry in result.getResults()}


class SubtestReportingTest(unittest.TestCase):
    def test_a_skipped_subtest_leaves_the_test_passing(self) -> None:
        case, after_skip = _subtests_case()
        result = _results_of(case)["test_subtest_skips"]
        self.assertEqual(result["type"], "SUCCESS")
        self.assertEqual(result["message"], "Skipped: platform not supported")
        self.assertEqual(after_skip, [True])

    def test_a_failed_subtest_keeps_its_message(self) -> None:
        case, _ = _subtests_case()
        result = _results_of(case)["test_subtest_fails"]
        self.assertEqual(result["type"], "FAILURE")
        self.assertEqual(
            result["message"], "AssertionError: 1 != 0 : value 1 is not zero"
        )
        self.assertIn("test_subtest_fails", str(result["stacktrace"]))
