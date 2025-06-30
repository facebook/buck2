# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

# pyre-strict


import json
from pathlib import Path
from typing import List, Optional

from buck2.tests.e2e_util.api.buck import Buck


async def collect_coverage_for(
    buck: Buck,
    tmp_path: Path,
    target: str,
    filter: List[str],
    mode: Optional[str] = None,
) -> List[str]:
    coverage_file = tmp_path / "coverage.txt"
    filter_str = " ".join(filter)
    buck_args = []
    if mode is not None:
        buck_args.append(mode)
    buck_args.extend(
        [
            "--config",
            "fbcode.coverage_selective=true",
            "--config",
            f"fbcode.cxx_coverage_only={filter_str}",
            target,
            "--",
            "--collect-coverage",
            f"--coverage-output={coverage_file}",
        ]
    )
    await buck.test(*buck_args)
    paths = []
    with open(coverage_file) as results:
        for line in results:
            paths.append(json.loads(line)["filepath"])

    return list(set(paths))
