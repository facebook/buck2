# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

# pyre-strict


import json
from pathlib import Path

from buck2.tests.e2e_util.api.buck import Buck
from buck2.tests.e2e_util.asserts import expect_failure
from buck2.tests.e2e_util.buck_workspace import buck_test


@buck_test(inplace=False, data_dir="bxl/simple")
async def test_bxl_root(buck: Buck) -> None:
    result = await buck.bxl(
        "//bxl:root.bxl:root_test",
    )

    assert str(buck.cwd) in result.stdout


@buck_test(inplace=False, data_dir="bxl/simple")
async def test_bxl_cell_root(buck: Buck) -> None:
    result = await buck.bxl(
        "fbcode//cell_root.bxl:cell_root_test",
    )

    assert str(buck.cwd / "fbcode") in result.stdout


@buck_test(inplace=False, data_dir="bxl/simple")
async def test_bxl_instant_event(buck: Buck) -> None:
    await buck.bxl(
        "//bxl/event.bxl:good",
    )

    # Get event log
    log = (await buck.log("show")).stdout.strip()
    lines = log.splitlines()
    # try to find starlark instant event

    found_event = False
    for line in lines:
        if "StarlarkUser" in line:
            assert "foo" in line
            assert "bool_value" in line
            assert "string_value" in line
            assert "int_value" in line
            found_event = True
            break

    if not found_event:
        raise AssertionError("Failed to find starlark instant event.")

    # Shouldn't fail
    await buck.bxl("//bxl/event.bxl:metadata_with_duration")

    await expect_failure(
        buck.bxl(
            "//bxl/event.bxl:bad_metadata",
        ),
        stderr_regex="Metadata should be a dict where keys are strings, and values are strings, ints, bools, or dicts/lists of the mentioned types. Got type: `list`",
    )

    await expect_failure(
        buck.bxl(
            "//bxl/event.bxl:bad_metadata_key",
        ),
        stderr_regex="Metadata keys should be strings. Got type: `int`",
    )

    await expect_failure(
        buck.bxl(
            "//bxl/event.bxl:bad_metadata_value",
        ),
        stderr_regex="Metadata values should be strings, ints, bools, or dicts/lists of the mentioned types. Key `key` had value type `tuple`",
    )

    result = await buck.bxl(
        "//bxl/event.bxl:ensured_artifact",
    )

    artifact_path = result.stdout.strip()

    # Get event log
    lines = (await buck.log("show-user")).stdout.strip().splitlines()
    found_event = False
    for line in lines:
        if "StarlarkUserEvent" in line:
            metadata = json.loads(line)["StarlarkUserEvent"]["metadata"]
            assert metadata["rel_path"] == artifact_path
            assert metadata["abs_path"] == str(Path(buck.cwd / artifact_path))
            assert metadata["nested"]["nested_artifact"] == artifact_path
            found_event = True
            break

    if not found_event:
        raise AssertionError("Failed to find starlark instant event.")


@buck_test(inplace=False, data_dir="bxl/simple")
async def test_bxl_read_config(buck: Buck) -> None:
    result = await buck.bxl(
        "-c",
        "key.section=foo",
        "//bxl/read_config.bxl:read_config_test",
    )

    assert "foo" in result.stdout
    assert "True" in result.stdout


@buck_test(inplace=False, data_dir="bxl/simple")
async def test_load_file(buck: Buck) -> None:
    result = await buck.bxl(
        "//bxl:load_file.bxl:load_test",
    )

    assert str(buck.cwd) in result.stdout
