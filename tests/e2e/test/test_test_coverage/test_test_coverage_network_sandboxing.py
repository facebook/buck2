# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

# pyre-strict

import json
from pathlib import Path

from buck2.tests.e2e_util.api.buck import Buck
from buck2.tests.e2e_util.buck_workspace import buck_test


@buck_test(inplace=True)
async def test_cpp_test_coverage_with_network_sandboxing(
    buck: Buck, tmp_path: Path
) -> None:
    coverage_file = tmp_path / "coverage.txt"
    await buck.test(
        "@fbcode//mode/dbgo-cov",
        "fbcode//buck2/tests/targets/rules/cxx:cpp_test_pass",
        "-c",
        # We need to build the llvm coverage wrapper from source because otherwise this is served
        # via msdk which will fail to download the wrapper when wrapped in an `unshare` call.
        "coverage.llvm_build_from_source=true",
        "--",
        "--collect-coverage",
        f"--coverage-output={coverage_file}",
        "--experimental-force-linux-network-sandboxing",
    )
    paths = []
    with open(coverage_file) as results:
        for line in results:
            paths.append(json.loads(line)["filepath"])

    assert "fbcode/buck2/tests/targets/rules/cxx/cpp_test_pass.cpp" in paths, str(paths)
    assert "fbcode/common/gtest/LightMain.cpp" in paths, str(paths)
