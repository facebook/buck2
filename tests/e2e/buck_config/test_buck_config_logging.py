# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.


import json

from buck2.tests.e2e_util.api.buck import Buck
from buck2.tests.e2e_util.buck_workspace import buck_test


@buck_test(inplace=False)
async def test_buck_config_logging_disabled(buck: Buck) -> None:
    result = await buck.targets("//:")
    assert "starlark_log_buckconfig" not in result.stderr
    assert "starlark_log_all_buckconfigs" not in result.stderr


@buck_test(inplace=False)
async def test_buck_config_logging_enabled(buck: Buck) -> None:
    result = await buck.targets("//:", "--config", "buckconfig.log=test.read1")
    lines = [
        line
        for line in result.stderr.splitlines()
        if "starlark_log_buckconfig" in line
        # Unfortunately, "starlark_log_buckconfig" also shows up inside the stacktrace, so
        # try to exclude these lines here
        and "print(" not in line
    ]
    assert len(lines) == 1

    result = await buck.targets(
        "//:", "--config", "buckconfig.log=test.not_a_valid_buckconfig"
    )
    lines = [
        line for line in result.stderr.splitlines() if "starlark_log_buckconfig" in line
    ]
    assert len(lines) == 0


@buck_test(inplace=False)
async def test_buck_config_logging_enabled_json(buck: Buck) -> None:
    result = await buck.targets("//:", "--config", "buckconfig.log_json=test.read1")
    lines = [
        line for line in result.stderr.splitlines() if "starlark_log_buckconfig" in line
    ]
    assert len(lines) == 1, result.stderr
    # Terrible way to strip out the timestamp from the log line...
    read_config = json.loads(lines[0].split(maxsplit=1)[1].strip())[
        "starlark_log_buckconfig"
    ]
    assert read_config["cell"] == "root"

    result = await buck.targets(
        "//:", "--config", "buckconfig.log_json=test.not_a_valid_buckconfig"
    )
    lines = [
        line for line in result.stderr.splitlines() if "starlark_log_buckconfig" in line
    ]
    assert len(lines) == 0


@buck_test(inplace=False)
async def test_buck_config_logging_all_enabled(buck: Buck) -> None:
    result = await buck.targets(
        "//:",
        "--config",
        "buckconfig.log_all_in_json=true",
    )
    print(result.stderr)
    jsons = [
        # Terrible way to strip out the timestamp from the log line...
        json.loads(line.split(maxsplit=1)[1])["starlark_log_all_buckconfigs"]
        for line in result.stderr.splitlines()
        if '{"starlark_log_all_buckconfigs' in line
    ]
    filtered = [j for j in jsons if j["section"] == "test"]
    assert len(filtered) == 2
    for j in filtered:
        assert j["key"] in ["read1", "read2"]
        assert j["cell"] == "root"
