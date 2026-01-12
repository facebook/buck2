# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

# pyre-strict

import csv
import random
import string

from buck2.tests.e2e_util.api.buck import Buck
from buck2.tests.e2e_util.buck_workspace import buck_test


@buck_test()
async def test_what_uploaded_csv(buck: Buck) -> None:
    # Use a random content on every test invocation to make sure we actually get uploads
    content = "".join(random.choices(string.ascii_uppercase + string.digits, k=20))
    await buck.build("//:upload_rule", "--remote-only", "-c", "test.content=" + content)
    out = await buck.log("what-uploaded", "--format", "csv")
    header = ["action", "digests_uploaded", "bytes_uploaded"]
    out = [
        dict(zip(header, record))
        for record in csv.reader(out.stdout.splitlines())
        if record
    ]
    assert len(out) > 0, "out should have some uploads"
    assert out[0] == dict(zip(header, header)), (
        "ensure that first entry in csv is the header"
    )
    assert int(out[1]["digests_uploaded"]) > 0, "second entry should be upload digests"


@buck_test()
async def test_what_uploaded_aggregated(buck: Buck) -> None:
    # Use a random content on every test invocation to make sure we actually get uploads
    content = "".join(random.choices(string.ascii_uppercase + string.digits, k=20))
    await buck.build("//:upload_rule", "--remote-only", "-c", "test.content=" + content)
    out = await buck.log("what-uploaded", "--aggregate-by-ext")
    out = [line.split() for line in out.stdout.splitlines() if line]
    assert len(out) > 0, "out should have some uploads"
    assert out[0] == ["txt", "1", "20"], f"unexpected output: {out}"
