# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

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


# FIXME(minglunli): Test was passing for the wrong reason, reached out to Eden and will disable the test for now

# Thrift Read API is only enabled for MacOS
# @buck_test(
#     setup_eden=True,
#     extra_buck_config={
#         "buck2": {
#             "allow_eden_io": "true",
#             "use_eden_thrift_read": "true",
#         }
#     },
#     skip_for_os=["windows", "linux"],
# )
# async def test_eden_large_config_file(buck: Buck) -> None:
#     # String below is ~55 bytes, so this writes ~2.7GB which is comfortably over 2GB
#     writes = 50000000

#     with open(buck.cwd / ".buckconfig", "a") as f:
#         # Picked a random config and writing that a bunch of times to make the file large
#         # This will result in a .buckconfig file to actually be valid, so an error should
#         # either be a size issue or it should succeed
#         while writes > 0:
#             f.write("[buck2]\n")
#             f.write("compute_action_inputs_hash_enabled = false\n")
#             writes -= 1

#     await expect_failure(
#         buck.build(),
#         # TODO(minglunli): Should check for Thrift size limit error message, but CI is on old Eden version 20250403-074430
#         #                  There's a bug that's fixed in 20250422-124337 which lets Eden return the correct error message
#         # stderr_regex="Thrift size limit",
#     )


@buck_test(setup_eden=True, data_dir="test_large_action_input")
async def test_large_action_input(buck: Buck) -> None:
    result = await buck.build("//:large_action_input")
    output = result.get_build_report().output_for_target("//:large_action_input")
    assert int(output.read_text().rstrip()) > 2147483648  # Wrote more than 2GB
