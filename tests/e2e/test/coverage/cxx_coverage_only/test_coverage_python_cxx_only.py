# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

# pyre-strict


import json
import tempfile

from buck2.tests.e2e_util.api.buck import Buck
from buck2.tests.e2e_util.buck_workspace import buck_test


@buck_test(inplace=True)
async def test_python_coverage(buck: Buck) -> None:
    with tempfile.NamedTemporaryFile("w") as covfile:
        await buck.test(
            "@fbcode//mode/dbgo-cov",
            "fbcode//buck2/tests/targets/rules/python/coverage:test",
            "--",
            "--collect-coverage",
            f"--coverage-output={covfile.name}",
        )
        paths = []
        with open(covfile.name) as results:
            for line in results:
                paths.append(json.loads(line)["filepath"])
    assert "fbcode/buck2/tests/targets/rules/python/coverage/lib.py" in paths, str(
        paths
    )


@buck_test(inplace=True)
async def test_python_coverage_filtering_by_folder(buck: Buck) -> None:
    folder_to_collect = "buck2/tests/targets/rules/python/coverage"
    with tempfile.NamedTemporaryFile("w") as covfile:
        await buck.test(
            "@fbcode//mode/dbgo-cov",
            "fbcode//buck2/tests/targets/rules/python/coverage:test",
            "-c",
            f"fbcode.cxx_coverage_only={folder_to_collect}",
            "--",
            "--collect-coverage",
            f"--coverage-output={covfile.name}",
        )
        paths = []
        with open(covfile.name) as results:
            for line in results:
                paths.append(json.loads(line)["filepath"])
    assert set(paths) == {
        f"fbcode/{folder_to_collect}/lib.py",
        f"fbcode/{folder_to_collect}/test.py",
    }, (
        f"Only folder fbcode/{folder_to_collect} should have coverage, instead got coverage for {str(paths)}"
    )
