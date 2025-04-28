# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

# pyre-strict


import sys

from buck2.tests.e2e_util.api.buck import Buck
from buck2.tests.e2e_util.buck_workspace import buck_test

from buck2.tests.e2e_util.helper.golden import golden


async def _run_test(buck: Buck, name: str) -> None:
    if sys.platform == "win32":
        kind = "win"
        targets = [
            "file_metadata/nested",
        ]
    else:
        # Set up some symlinks. Do this here to avoid relying on the test runner
        # copying these correctly.
        symlink_dir = buck.cwd / "file_metadata" / "symlinks"
        symlink_dir.mkdir()
        (symlink_dir / "internal").symlink_to("../file")
        (symlink_dir / "external").symlink_to("/absolute")

        kind = "unix"
        targets = [
            "file_metadata",
            "file_metadata/symlinks/internal/traverse",
            "file_metadata/symlinks/external/traverse",
        ]

    await buck.build()  # Start Buck2

    res = await buck.debug("file-status", "--show-matches", *targets)
    golden(output=res.stdout, rel_path=f"golden/{name}.{kind}.out")
    assert "MISMATCH" not in res.stdout


@buck_test(
    setup_eden=True,
    extra_buck_config={
        "buck2": {
            "allow_eden_io": "false",
            "source_digest_algorithm": "SHA1",
        }
    },
)
async def test_default(buck: Buck) -> None:
    await _run_test(buck, "default")


@buck_test(
    setup_eden=True,
    extra_buck_config={
        "buck2": {
            "allow_eden_io": "true",
            "source_digest_algorithm": "SHA1",
        }
    },
)
async def test_eden(buck: Buck) -> None:
    await _run_test(buck, "default")


@buck_test(
    setup_eden=True,
    extra_buck_config={
        "buck2": {
            "allow_eden_io": "false",
            "source_digest_algorithm": "BLAKE3-KEYED",
        }
    },
)
async def test_blake3(buck: Buck) -> None:
    await _run_test(buck, "blake3")


@buck_test(
    setup_eden=True,
    extra_buck_config={
        "buck2": {
            "source_digest_algorithm": "BLAKE3-KEYED",
        }
    },
)
async def test_eden_blake3(buck: Buck) -> None:
    await _run_test(buck, "blake3")
