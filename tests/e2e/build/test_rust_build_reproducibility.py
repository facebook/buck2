# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

# pyre-strict

import hashlib

from buck2.tests.e2e_util.api.buck import Buck
from buck2.tests.e2e_util.buck_workspace import buck_test, get_mode_from_platform


def sha256_file(path: str) -> str:
    h = hashlib.sha256()
    with open(path, "rb") as f:
        for chunk in iter(lambda: f.read(8192), b""):
            h.update(chunk)
    return h.hexdigest()


# Regression testing for the differences in execution environment causing https://github.com/rust-lang/rust/issues/153898
# to generate different crate hashes.
@buck_test(inplace=True)
async def test_rust_build_reproducibility(buck: Buck) -> None:
    target = "fbcode//buck2/tests/targets/rules/rust/source_map_consistency/repro:repro"

    # Build 1: remote execution
    result_remote = await buck.build_without_report(
        get_mode_from_platform(),
        target,
        "--remote-only",
        "--show-full-simple-output",
    )
    remote_path = result_remote.stdout.strip()
    remote_hash = sha256_file(remote_path)

    await buck.kill()

    # Build 2: local execution, no remote cache
    result_local = await buck.build_without_report(
        get_mode_from_platform(),
        target,
        "--local-only",
        "--no-remote-cache",
        "--show-full-simple-output",
    )
    local_path = result_local.stdout.strip()
    local_hash = sha256_file(local_path)

    assert remote_hash == local_hash, (
        f"Build output differs between remote and local execution.\n"
        f"Remote ({remote_path}): {remote_hash}\n"
        f"Local  ({local_path}): {local_hash}"
    )
