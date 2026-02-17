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
from pathlib import Path

from buck2.tests.e2e_util.api.buck import Buck
from buck2.tests.e2e_util.buck_workspace import buck_test
from tae.testx.py.testx_helpers import TestXClient


def get_path_from_env(envvar: str) -> Path:
    path = os.getenv(envvar)
    assert path
    return Path(path)


def get_testx_binary() -> Path:
    return get_path_from_env("TESTX_BIN")


def get_testx_client() -> TestXClient:
    return TestXClient(binary=get_testx_binary(), caller="buck-e2e")


def extract_test_run_id(haystack: str) -> int:
    matches = re.findall("^.*Test session:.*/(\\d+).*$", haystack, re.MULTILINE)
    match = next(iter(matches), None)
    assert match, f"Test run ID not found in {haystack}"
    return int(match)


@buck_test(inplace=True)
async def test_passing_test_details_uploaded_to_artifacts(buck: Buck) -> None:
    output = await buck.test(
        "fbcode//buck2/tests/targets/rules/python_test:test_produce_test_details",
        "--remote-only",
        "--",
        "--upload-passing-details",
    )

    run_id = extract_test_run_id(output.stderr)

    tests_artifact_data = get_testx_client().artifacts_list(run_id)

    assert len(tests_artifact_data) > 0, "Should have artifact data for test run"

    details_artifact = None
    for data in tests_artifact_data:
        for artifact in data.artifacts:
            if (
                hasattr(artifact.handle, "everstore_handle")
                and artifact.handle.everstore_handle is not None
            ):
                details_artifact = artifact
                break
        if details_artifact is not None:
            break

    assert details_artifact is not None, "Should have artifact with everstore_handle"

    assert hasattr(details_artifact.handle, "everstore_handle"), (
        "Details artifact should have everstore_handle"
    )
    assert details_artifact.handle.everstore_handle is not None, (
        "Details artifact everstore_handle should not be None"
    )
    assert details_artifact.handle.everstore_handle.handle is not None, (
        "Everstore handle string should not be None"
    )
    assert details_artifact.annotations.type.test_details_artifact is not None, (
        "Artifact should be of type test_details_artifact"
    )
