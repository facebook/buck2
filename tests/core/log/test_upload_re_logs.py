# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

# pyre-strict


import os
import sys

from buck2.tests.e2e_util.api.buck import Buck
from buck2.tests.e2e_util.buck_workspace import buck_test, env
from buck2.tests.e2e_util.helper.utils import json_get
from manifold.clients.python.manifold_client_deprecated import Client as ManifoldClient


BUCKET_CONFIG = {"bucket": "buck2_re_logs", "apikey": "buck2_re_logs-key"}

# This test was failing in macos sandcastle, so attempt fix suggested
# here: https://fb.workplace.com/groups/fbpython/permalink/5214295275278464/
if sys.platform != "windows" and os.path.exists("/etc/ssl/cert.pem"):
    os.environ["SSL_CERT_FILE"] = "/etc/ssl/cert.pem"


async def manifold_exists(path: str) -> bool:
    with ManifoldClient(BUCKET_CONFIG) as client:
        return client.exists(bucket=BUCKET_CONFIG["bucket"], path=path)


@buck_test()
@env("SANDCASTLE", "1")  # wait for logs to finish uploading
async def test_upload_re_logs(buck: Buck) -> None:
    # Build a trivial action
    await buck.build("root//:run")

    session_id = await extract_re_session_id(buck)
    await buck.debug("upload-re-logs", "--session-id", session_id)
    assert await manifold_exists(path=f"flat/{session_id}.log.zst") is True


async def extract_re_session_id(buck: Buck) -> str:
    result = await buck.log("show")
    session_id = None
    for line in result.stdout.splitlines():
        session_id = json_get(
            line, "Event", "data", "Instant", "data", "ReSession", "session_id"
        )
        if session_id:
            break
    assert session_id is not None
    return session_id
