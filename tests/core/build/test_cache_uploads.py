# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

# pyre-strict


import sys

from buck2.tests.e2e_util.api.buck import Buck
from buck2.tests.e2e_util.buck_workspace import buck_test, env
from buck2.tests.e2e_util.helper.utils import json_get, random_string


async def _assert_locally_executed_upload_attempted(buck: Buck, count: int = 1) -> None:
    await _assert_upload_attempted(buck, count)


async def _assert_upload_attempted(buck: Buck, count: int) -> None:
    log = (await buck.log("show")).stdout.strip().splitlines()
    uploads = []
    excluded_uploads = []

    for line in log:
        e = json_get(
            line,
            "Event",
            "data",
            "SpanEnd",
            "data",
            "CacheUpload",
        )
        if e is None:
            continue
        if e["success"] or e["re_error_code"] == "PERMISSION_DENIED":
            # Tolerate permission denied errors because we don't have a choice on CI :(
            uploads.append(e)
        else:
            excluded_uploads.append(e)

    if len(uploads) == count:
        return
    else:
        print(f"Expected {count} uploads", file=sys.stderr)
        print(f"Actual uploads: {uploads}", file=sys.stderr)
        print(f"Excluded uploads: {excluded_uploads}", file=sys.stderr)
        raise AssertionError("Wrong number of uploads, see above")


@buck_test()
async def test_re_uploads(buck: Buck) -> None:
    args = ["-c", f"write.text={random_string()}"]
    await buck.build("root//:write", *args)
    await _assert_locally_executed_upload_attempted(buck, 1)


@buck_test()
async def test_re_uploads_dir(buck: Buck) -> None:
    args = ["-c", f"write.text={random_string()}"]
    await buck.build("root//:write_in_dir", *args)
    await _assert_locally_executed_upload_attempted(buck, 1)


@buck_test()
async def test_re_uploads_limit(buck: Buck) -> None:
    args = ["-c", f"write.text={random_string()}"]
    await buck.build("root//:write_xxl", *args)
    await _assert_locally_executed_upload_attempted(buck, 0)
