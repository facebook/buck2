# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

# pyre-strict


import os
import re
import typing
from pathlib import Path

from buck2.tests.e2e_util.api.buck import Buck
from buck2.tests.e2e_util.buck_workspace import buck_test
from buck2.tests.e2e_util.helper.utils import filter_events
from facebook.remote_execution.common import thrift_types, types
from facebook.remote_execution.re_client_lib_if.client.types import (
    EmbeddedCASDaemonClientCfg,
)
from facebook.remote_execution.re_client_lib_if.remote_execution_metadata.types import (
    RemoteExecutionMetadata,
)
from remote_execution.client_lib.wrappers.python.re_client import (
    PyREClientParams,
    REClient,
)
from tae.testx.py.testx_helpers import TestXClient


ARTIFACTS_DIR_NAME = "artifacts_directory"
ANNOTATIONS_DIR_NAME = "artifact_annotations_directory"
TPX_EXEC_DIR = "tpx_execution_dir"
USE_CASE_ID = "tpx-default"
EMBEDDED_CAS_NAME = "tpx"


@buck_test(inplace=True)
async def test_produce_artifacts(buck: Buck) -> None:
    await buck.test(
        "fbcode//buck2/tests/targets/rules/python_test:test_produce_artifacts",
    )


@buck_test(inplace=True)
async def test_remote_artifact_directory_is_materialized_by_default(buck: Buck) -> None:
    await buck.test(
        "fbcode//buck2/tests/targets/rules/python_test:test_produce_artifacts",
        "--remote-only",
    )

    materialized_paths = await filter_events(
        buck, "Event", "data", "SpanEnd", "data", "Materialization", "path"
    )

    assert_has_dir(ARTIFACTS_DIR_NAME, materialized_paths)
    assert_has_dir(ANNOTATIONS_DIR_NAME, materialized_paths)
    assert_has_dir(TPX_EXEC_DIR, materialized_paths)


@buck_test(inplace=True)
async def test_remote_artifact_directory_is_not_materialized_when_cas_support_enabled(
    buck: Buck,
) -> None:
    await buck.test(
        "fbcode//buck2/tests/targets/rules/python_test:test_produce_artifacts_in_cas",
        "--no-remote-cache",
        "--remote-only",
    )

    materialized_paths = await filter_events(
        buck, "Event", "data", "SpanEnd", "data", "Materialization", "path"
    )
    assert_has_no_dir(ARTIFACTS_DIR_NAME, materialized_paths)
    assert_has_dir(ANNOTATIONS_DIR_NAME, materialized_paths)
    assert_has_dir(TPX_EXEC_DIR, materialized_paths)


@buck_test(inplace=True)
async def test_remote_artifact_has_cas_handle_with_right_ttl(buck: Buck) -> None:
    output = await buck.test(
        "fbcode//buck2/tests/targets/rules/python_test:test_produce_artifacts_in_cas",
        "--remote-only",
    )

    run_id = extract_test_run_id(output.stderr)
    tests_artifact_data = get_testx_client().artifacts_list(run_id)
    re_metadata = RemoteExecutionMetadata(
        use_case_id=USE_CASE_ID,
    )
    re_client = get_re_client(USE_CASE_ID)

    expected_ttl = 13 * 86400  # TTL should be more than 13 days (in seconds)

    for data in tests_artifact_data:
        for artifact in data.artifacts:
            assert artifact.handle.cas_digest
            tdigest = py3_to_python_tdigest(artifact.handle.cas_digest)
            digest_with_ttl = await get_digests_ttl(re_client, re_metadata, [tdigest])
            assert len(digest_with_ttl) == 1
            assert digest_with_ttl[0].ttl >= expected_ttl


#########
# Helpers
#########


def has_dir(dir_name: str, paths: typing.List[str]) -> bool:
    return any(dir_name in path for path in paths)


def assert_has_dir(dir_name: str, paths: typing.List[str]) -> None:
    assert has_dir(dir_name, paths), f"Directory {dir_name} not found in {paths}"


def assert_has_no_dir(dir_name: str, paths: typing.List[str]) -> None:
    assert not has_dir(dir_name, paths), f"Directory {dir_name} found in {paths}"


def get_path_from_env(envvar: str) -> Path:
    path = os.getenv(envvar)
    assert path
    return Path(path)


def py3_to_python_tdigest(testx_cas_digest: thrift_types.TDigest) -> types.TDigest:
    return types.TDigest(
        hash=testx_cas_digest.hash,
        size_in_bytes=testx_cas_digest.size_in_bytes,
    )


def get_testx_binary() -> Path:
    return get_path_from_env("TESTX_BIN")


def get_testx_client() -> TestXClient:
    return TestXClient(binary=get_testx_binary(), caller="buck-e2e")


def get_re_client(use_case: str) -> REClient:
    client_params = PyREClientParams().with_embedded_cas(
        EmbeddedCASDaemonClientCfg(name=EMBEDDED_CAS_NAME)
    )
    return REClient(use_case, re_client_params=client_params)


async def get_digests_ttl(
    re_client: REClient,
    metadata: RemoteExecutionMetadata,
    digests: typing.Sequence[types.TDigest],
) -> typing.Sequence[types.TDigestWithTtl]:
    response = await re_client.get_digests_ttl(metadata, digests)
    assert response
    return response.digests_with_ttl


def extract_test_run_id(haystack: str) -> int:
    matches = re.findall("^.*Test session:.*/(\\d+).*$", haystack, re.MULTILINE)
    match = next(iter(matches), None)
    assert match, f"Test run ID not found in {haystack}"
    return int(match)
