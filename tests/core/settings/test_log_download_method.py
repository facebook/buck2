# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

# pyre-strict


import json
from typing import Any

from buck2.tests.e2e_util.api.buck import Buck
from buck2.tests.e2e_util.buck_workspace import buck_test


async def _log_download_method(buck: Buck) -> Any:
    result = await buck.status()
    config = json.loads(result.stdout)["daemon_constraints"]["daemon_startup_config"]
    return json.loads(config)["log_download_method"]


def _write_buckconfig_log_use_manifold(buck: Buck, value: bool) -> None:
    with open(buck.cwd / ".buckconfig", "a") as f:
        f.write(f"\n[buck2]\nlog_use_manifold = {str(value).lower()}\n")


def _write_buckconfig_log_url(buck: Buck, value: str) -> None:
    with open(buck.cwd / ".buckconfig", "a") as f:
        f.write(f"\n[buck2]\nlog_url = {value}\n")


@buck_test()
async def test_settings_override_buckconfig(buck: Buck) -> None:
    _write_buckconfig_log_use_manifold(buck, True)
    _write_buckconfig_log_url(buck, "abc.com")
    (buck.cwd / ".bucksettings.toml").write_text(
        'log_use_manifold = false\nlog_url = "test.com"\n'
    )

    await buck.server()
    assert await _log_download_method(buck) == {"Curl": "test.com"}


@buck_test()
async def test_log_use_manifold_fallback_to_buckconfig(buck: Buck) -> None:
    _write_buckconfig_log_use_manifold(buck, True)

    await buck.server()
    assert await _log_download_method(buck) == "Manifold"


@buck_test()
async def test_log_url_fallback_to_buckconfig(buck: Buck) -> None:
    (buck.cwd / ".bucksettings.toml").write_text("log_use_manifold = false\n")

    await buck.server()
    assert await _log_download_method(buck) == "None"

    _write_buckconfig_log_url(buck, "test.com")

    await buck.server()
    assert await _log_download_method(buck) == {"Curl": "test.com"}
