# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

import json
import tempfile
from pathlib import Path

from buck2.tests.e2e_util.api.buck import Buck
from buck2.tests.e2e_util.buck_workspace import buck_test


# TODO: FIXME
@buck_test()
async def test_representative_config_flags_incorrectly_contain_run_args(
    buck: Buck,
) -> None:
    with tempfile.TemporaryDirectory() as d:
        invocation_record_file = Path(d) / "record"
        await buck.run(
            "//:my_rule",
            "--unstable-write-invocation-record",
            str(invocation_record_file),
            "--",
            "--config",
            "foo=bar",
        )
        with open(invocation_record_file) as f:
            invocation_record = json.load(f)

    assert invocation_record["data"]["Record"]["data"]["InvocationRecord"][
        "representative_config_flags"
    ] == ["-c foo=bar"]
