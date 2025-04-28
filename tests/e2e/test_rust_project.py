# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

# pyre-strict


import json
import os
import subprocess

from buck2.tests.e2e_util.buck_workspace import buck_test


@buck_test(inplace=True)
async def test_rust_binary() -> None:
    rust_project_bin = os.environ["RUST_PROJECT_BIN"]

    env = os.environ.copy()
    env["BUCK2_HARD_ERROR"] = "false"

    result = subprocess.run(
        [
            rust_project_bin,
            "develop",
            "--stdout",
            "--pretty",
            "fbcode//buck2/tests/targets/rules/rust/hello_world:welcome",
        ],
        stdout=subprocess.PIPE,
        env=env,
    )

    json_generated = json.loads(result.stdout)

    assert "sysroot" in json_generated.keys()
    assert "sysroot_src" in json_generated.keys()
    assert "crates" in json_generated.keys()
