# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

# pyre-strict

import json
import sys
from pathlib import Path

from buck2.tests.e2e_util.api.buck import Buck
from buck2.tests.e2e_util.buck_workspace import buck_test
from buck2.tests.e2e_util.helper.utils import random_string, read_what_ran


@buck_test()
async def test_target_with_two_gang_workers(buck: Buck) -> None:
    """Test that a target with two gang workers builds successfully on RE."""
    result = await buck.build(
        ":target_with_two_gang_workers",
        "-c",
        "build.execution_platforms=root//platforms:platforms",
        "-c",
        f"test.cache_buster={random_string()}",
        "--show-full-output",
    )
    result.check_returncode()

    # Read and validate the content of /run/re_worker/gang_info (copied to output)
    output_dict = result.get_target_to_build_output()
    for _, output_path in output_dict.items():
        with Path(output_path).open() as f:
            content = f.read()
            # Parse as GetGangInfoResponse JSON
            gang_info = json.loads(content)
            print(f"gang_info: {gang_info}", file=sys.stderr)
            # Assert that we got 2 workers back
            workers = gang_info.get("workers", [])
            assert len(workers) == 2, f"Expected 2 gang workers, got {len(workers)}"

    # Make sure it actually ran on RE
    out = await read_what_ran(buck)
    executors = {line["identity"]: line["reproducer"]["executor"] for line in out}
    expected = {
        "root//:target_with_two_gang_workers (<unspecified>) (cp)": "Re",
    }
    assert executors == expected
