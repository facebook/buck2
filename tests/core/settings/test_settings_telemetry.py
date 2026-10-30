# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

# pyre-strict


import json
from pathlib import Path
from typing import Any, List

from buck2.tests.e2e_util.api.buck import Buck
from buck2.tests.e2e_util.buck_workspace import buck_test, BuckTestCase
from buck2.tests.e2e_util.helper.utils import filter_events, read_invocation_record


async def _observed_settings(buck: Buck) -> List[Any]:
    return await filter_events(
        buck, "Event", "data", "Instant", "data", "BuckSettingsObserved"
    )


class SettingsTelemetryTest(BuckTestCase):
    @buck_test()
    async def test_settings_reported_once_per_daemon(
        self, buck: Buck, tmp_path: Path
    ) -> None:
        first = tmp_path / "first.json"
        await buck.audit_config("--unstable-write-invocation-record", str(first))
        observed = await _observed_settings(buck)
        assert len(observed) == 1
        first_hash = read_invocation_record(first)["settings_hash"]
        assert first_hash.startswith("buck2settings-")
        assert observed[0]["hash"] == first_hash
        settings = json.loads(observed[0]["settings_json"])
        assert settings["analysis"]["record_requested_anon_targets"] is None

        # The same daemon does not report the same settings again.
        await buck.audit_config()
        assert await _observed_settings(buck) == []

        # A changed daemon setting starts a new daemon, which reports the new
        # value.
        second = tmp_path / "second.json"
        result = await buck.audit_config(
            "--setting",
            "analysis.record_requested_anon_targets=false",
            "--unstable-write-invocation-record",
            str(second),
        )
        assert "restarting daemon" in result.stderr
        observed = await _observed_settings(buck)
        assert len(observed) == 1
        second_hash = read_invocation_record(second)["settings_hash"]
        assert observed[0]["hash"] == second_hash
        assert second_hash != first_hash
        settings = json.loads(observed[0]["settings_json"])
        assert settings["analysis"]["record_requested_anon_targets"] is False

        # A changed invocation setting keeps the daemon, which still reports
        # the combination it has not served before.
        third = tmp_path / "third.json"
        result = await buck.audit_config(
            "--setting",
            "analysis.record_requested_anon_targets=false",
            "--setting",
            "agent_materialization.enabled=true",
            "--unstable-write-invocation-record",
            str(third),
        )
        assert "restarting daemon" not in result.stderr
        observed = await _observed_settings(buck)
        assert len(observed) == 1
        third_hash = read_invocation_record(third)["settings_hash"]
        assert observed[0]["hash"] == third_hash
        assert third_hash not in (first_hash, second_hash)
        settings = json.loads(observed[0]["settings_json"])
        assert settings["agent_materialization"]["enabled"] is True
