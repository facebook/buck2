# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

# pyre-strict


import os
import random
import string
import sys
from pathlib import Path

from buck2.tests.e2e_util.api.buck import Buck
from buck2.tests.e2e_util.buck_workspace import buck_test, env
from manifold.clients.python.manifold_client_deprecated import Client as ManifoldClient

TEST_TRACE_ID = "f115b5da-7d81-47cc-9c4a-57e283bfa384"
BUCKET_CONFIG = {"bucket": "buck2_logs", "apikey": "buck2_logs-key"}

# This test was failing in macos sandcastle, so attempt fix suggested
# here: https://fb.workplace.com/groups/fbpython/permalink/5214295275278464/
if sys.platform != "windows" and os.path.exists("/etc/ssl/cert.pem"):
    os.environ["SSL_CERT_FILE"] = "/etc/ssl/cert.pem"


async def manifold_exists(path: str) -> bool:
    with ManifoldClient(BUCKET_CONFIG) as client:
        return client.exists(bucket="buck2_logs", path=path)


async def manifold_file_size(path: str) -> int:
    with ManifoldClient(BUCKET_CONFIG) as client:
        return client.getFileSize(bucket="buck2_logs", path=path)


EVENT_LOG_PLACEHOLDER = """
Plants are living organisms that belong to the kingdom Plantae. They are characterized by their ability to produce their own food through photosynthesis, which is the process of converting sunlight, water, and carbon dioxide into glucose and oxygen. Plants come in many different shapes and sizes, from small mosses to towering trees, and can be found in almost every ecosystem on Earth. They play a vital role in the planet's ecology, serving as primary producers at the base of the food chain and providing habitats for a wide range of animal species. In addition to their ecological importance, plants have many practical uses for humans, such as food, medicine, clothing, and shelter.
"""


def random_name() -> str:
    alphabet = string.ascii_letters + string.digits
    return "".join(random.choice(alphabet) for _ in range(8))


@buck_test(inplace=True)
@env("BUCK2_TEST_MANIFOLD_CHUNK_BYTES", str(32))
@env("BUCK2_TEST_MANIFOLD_TTL_S", str(84_000))  # 1 day
async def test_persist_event_logs(buck: Buck, tmp_path: Path) -> None:
    local_log = tmp_path / "test.txt"

    manifold_name = f"test_{random_name()}.txt"
    await buck.debug(
        "persist-event-logs",
        "--manifold-name",
        manifold_name,
        "--local-path",
        str(local_log),
        "--trace-id",
        TEST_TRACE_ID,
        input=EVENT_LOG_PLACEHOLDER.encode(),
    )

    assert Path(local_log).exists()

    with open(local_log, "r") as f:
        assert f.read() == EVENT_LOG_PLACEHOLDER

    manifold_size = await manifold_file_size(path=f"flat/{manifold_name}")
    # Some OSs return str, others int. Just enforce str to compare
    assert str(manifold_size) == str(len(EVENT_LOG_PLACEHOLDER))


@buck_test(inplace=True)
async def test_persist_event_logs_not_uploaded(buck: Buck, tmp_path: Path) -> None:
    local_log = tmp_path / "test.txt"

    manifold_name = f"test_{random_name()}.txt"
    await buck.debug(
        "persist-event-logs",
        "--manifold-name",
        manifold_name,
        "--local-path",
        str(local_log),
        "--no-upload",
        "--trace-id",
        TEST_TRACE_ID,
        input=EVENT_LOG_PLACEHOLDER.encode(),
    )

    assert Path(local_log).exists()

    with open(local_log, "r") as f:
        assert f.read() == EVENT_LOG_PLACEHOLDER

    assert await manifold_exists(path=f"flat/{manifold_name}") is False
