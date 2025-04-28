# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

# pyre-strict

from pathlib import Path

from buck2.tests.e2e_util.api.buck import Buck
from buck2.tests.e2e_util.buck_workspace import buck_test


@buck_test()
async def test_reuse_current_config_with_config_overrides_and_previous_invocation(
    buck: Buck,
    tmp_path: Path,
) -> None:
    result_file = await buck.audit_config(
        "test.key",
        "--style",
        "json",
    )

    assert result_file.get_json().get("test.key") == "val"

    config_override = tmp_path / "config_override.bcfg"
    config_override.write_text("[test]\n  key = override\n")

    result_file = await buck.audit_config(
        "--config-file",
        str(config_override),
        "--config",
        "test.key2=override2",
        "--reuse-current-config",
        "--style",
        "json",
    )

    assert result_file.get_json().get("test.key") == "val"
    assert result_file.get_json().get("test.key2") is None
    assert "using current config instead" in result_file.stderr


@buck_test()
async def test_reuse_current_config_with_config_overrides_and_no_previous_invocation(
    buck: Buck,
) -> None:
    result_file = await buck.audit_config(
        "--config",
        "test.key=override",
        "--style",
        "json",
        "--reuse-current-config",
    )
    assert result_file.get_json().get("test.key") == "override"
    assert "Ignoring --reuse-current-config flag" in result_file.stderr


@buck_test()
async def test_reuse_current_config_with_no_previous_invocation(buck: Buck) -> None:
    result_file = await buck.audit_config(
        "test.key",
        "--style",
        "json",
        "--reuse-current-config",
    )
    assert result_file.get_json().get("test.key") == "val"
    assert "Ignoring --reuse-current-config flag" in result_file.stderr
