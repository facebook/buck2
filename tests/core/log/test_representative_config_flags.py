# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

# pyre-unsafe

import json
import tempfile
from pathlib import Path

from buck2.tests.e2e_util.api.buck import Buck
from buck2.tests.e2e_util.buck_workspace import buck_test


@buck_test()
async def test_representative_config_flags_disregards_run_args(
    buck: Buck,
) -> None:
    with tempfile.TemporaryDirectory() as d:
        invocation_record_file = Path(d) / "record"
        await buck.run(
            "//:my_rule",
            "--unstable-write-invocation-record",
            str(invocation_record_file),
            "--config",
            "foo.bar=baz",
            "--",
            "--config",
            "should.not=include",
        )
        with open(invocation_record_file) as f:
            invocation_record = json.load(f)

    assert invocation_record["data"]["Record"]["data"]["InvocationRecord"][
        "representative_config_flags"
    ] == ["-c foo.bar=baz"]


@buck_test()
async def test_representative_config_flags_includes_build_args(
    buck: Buck,
) -> None:
    with tempfile.TemporaryDirectory() as d:
        invocation_record_file = Path(d) / "record"
        await buck.build(
            "--unstable-write-invocation-record",
            str(invocation_record_file),
            "--config",
            "foo.bar=baz",
            # For `build` commands, anything after `--` is a positional arg.
            "--",
            "//:my_rule",
        )
        with open(invocation_record_file) as f:
            invocation_record = json.load(f)

    assert invocation_record["data"]["Record"]["data"]["InvocationRecord"][
        "representative_config_flags"
    ] == ["-c foo.bar=baz"]
