# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

# pyre-strict


import json
import tempfile

from buck2.tests.e2e_util.api.buck import Buck
from buck2.tests.e2e_util.buck_workspace import buck_test


@buck_test(inplace=True)
async def test_apple_coverage(buck: Buck) -> None:
    with tempfile.NamedTemporaryFile("w") as covfile:
        await buck.test(
            "-c",
            "xplat.available_platforms=APPLE,CXX",
            "-c",
            "code_coverage.enable=all",
            "fbsource//fbobjc/Samples/TestInfra/TpxUnitTests:TpxUnitTests",
            "--",
            "--collect-coverage",
            f"--coverage-output={covfile.name}",
        )
        paths = []
        with open(covfile.name) as results:
            for line in results:
                paths.append(json.loads(line)["filepath"])
    assert (
        "fbobjc/Samples/TestInfra/TpxUnitTests/TpxUnitTests/TpxUnitTests.m" in paths
    ), str(paths)


@buck_test(inplace=True)
async def test_apple_coverage_xplat(buck: Buck) -> None:
    with tempfile.NamedTemporaryFile("w") as covfile:
        await buck.test(
            "-c",
            "xplat.available_platforms=APPLE,CXX",
            "-c",
            "code_coverage.enable=all",
            "fbsource//xplat/testinfra/playground/cpp:example_testApple",
            "--",
            "--collect-coverage",
            f"--coverage-output={covfile.name}",
        )
        paths = []
        with open(covfile.name) as results:
            for line in results:
                paths.append(json.loads(line)["filepath"])
    assert "xplat/testinfra/playground/cpp/ExampleTest.cpp" in paths, str(paths)
