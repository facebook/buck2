# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

# pyre-strict

import json
import os
import tempfile
from dataclasses import dataclass
from typing import Optional

from buck2.tests.e2e_util.api.buck import Buck
from buck2.tests.e2e_util.buck_workspace import buck_test
from buck2.tests.e2e_util.helper.golden import golden_replace_temp_path
from buck2.tests.e2e_util.helper.utils import filter_events


@buck_test(
    extra_buck_config={
        "external_path_configs_section": {
            "external_path_configs_key": "external_path_configs_value",
        }
    },
)
async def test_external_buckconfigs(buck: Buck) -> None:
    with tempfile.NamedTemporaryFile("w", delete=False) as f:
        f.write("[test_section]\n")
        f.write("test_key = test_value\n")
        f.close()
        await buck.build(
            "@root//mode/my_mode",
            "//:test",
            "-c",
            "my_section.my_key=my_value",
            "--config-file",
            f.name,
        )
        # Make a spurious file change to trigger DICE updater state comparison
        with open(buck.cwd / "src", "w") as src:
            src.write("test")

        with open(buck.cwd / ".buckconfig.local", "w") as localconfig:
            localconfig.write("[local_section]\n")
            localconfig.write("local_key = local_value\n")
            localconfig.write("<file:included.bcfg>\n")
        await buck.build(
            "@root//mode/my_mode",
            "//:test",
            "-c",
            "my_section.my_key=my_value",
            "--config-file",
            f.name,
        )

    buckconfig_input_values = await filter_events(
        buck, "Event", "data", "Instant", "data", "BuckconfigInputValues", "components"
    )

    assert len(buckconfig_input_values) == 1
    external_configs = buckconfig_input_values[0]

    assert len(external_configs) == 5
    external_index = 0
    # The order is important here. We first have the buckconfig values from external sources
    external_path_configs = external_configs[0]["data"]["GlobalExternalConfigFile"]
    assert (
        external_path_configs["origin_path"]
        == buck._env["BUCK2_TEST_EXTRA_EXTERNAL_CONFIG"]
    )
    assert len(external_path_configs["values"]) == 2

    # Our tests inject file_watcher to external configs in test setup stage
    local_config_value = external_path_configs["values"][external_index]
    assert (
        local_config_value["section"] == "buck2"
        and local_config_value["key"] == "file_watcher"
        and local_config_value["value"] == "fs_hash_crawler"
        and not local_config_value["is_cli"]
    )
    external_index += 1
    external_path_config_value = external_path_configs["values"][external_index]
    assert (
        external_path_config_value["section"] == "external_path_configs_section"
        and external_path_config_value["key"] == "external_path_configs_key"
        and external_path_config_value["value"] == "external_path_configs_value"
        and not external_path_config_value["is_cli"]
    )

    # Next comes the values from the buckconfig.local file (which may include other files https://fburl.com/wd54jnpu)
    local_path_configs = external_configs[1]["data"]["GlobalExternalConfigFile"]
    assert len(local_path_configs["values"]) == 2
    assert local_path_configs["origin_path"] == ".buckconfig.local"
    # Note that buck parses configfiles ordered by section: https://fburl.com/rnzlt05n
    # That's why, we first have the values from the included file,
    included_config_value = local_path_configs["values"][0]
    assert (
        included_config_value["section"] == "included_section"
        and included_config_value["key"] == "included_key"
        and included_config_value["value"] == "included_value"
        and not included_config_value["is_cli"]
    )
    # The second one is for the values in the .buckconfig.local file
    local_config_value = local_path_configs["values"][1]
    assert (
        local_config_value["section"] == "local_section"
        and local_config_value["key"] == "local_key"
        and local_config_value["value"] == "local_value"
        and not local_config_value["is_cli"]
    )
    # The rest matches the same order provided by the above buck command
    # i.e. modefile, command line config flag, followed by the config file

    # We only store the path of the modefile
    assert (
        external_configs[2]["data"]["ConfigFile"]["data"]["ProjectRelativePath"]
        == "my_mode.bcfg"
    )

    # Then comes the config flag from the cli
    config_flag = external_configs[3]["data"]["ConfigValue"]
    assert config_flag["is_cli"]
    assert (
        config_flag["section"] == "my_section"
        and config_flag["key"] == "my_key"
        and config_flag["value"] == "my_value"
    )
    config_file = external_configs[4]["data"]["ConfigFile"]["data"][
        "GlobalExternalConfig"
    ]

    # Finally, the config file whose values are marked as coming from cli too
    config_file_value = config_file["values"][0]
    assert config_file_value["is_cli"]
    assert (
        config_file_value["section"] == "test_section"
        and config_file_value["key"] == "test_key"
        and config_file_value["value"] == "test_value"
    )
    assert f.name in config_file["origin_path"], (
        f"Origin path should contain config-file name: {f.name}"
    )


@buck_test(
    extra_buck_config={
        "external_path_configs_section": {
            "external_path_configs_key": "external_path_configs_value",
        }
    },
    write_invocation_record=True,
)
async def test_previous_command_with_mismatched_config(
    buck: Buck,
) -> None:
    await buck.build(
        "@root//mode/my_mode",
        "//:test",
        "-c",
        "my_section.my_key=my_value",
    )
    previous_invalidating_command = await filter_events(
        buck, "Event", "data", "Instant", "data", "PreviousCommandWithMismatchedConfig"
    )
    # No previous command, no PreviousCommandWithMismatchedConfig fired
    assert len(previous_invalidating_command) == 0

    # Rerun without any changes
    res = await buck.build(
        "@root//mode/my_mode",
        "//:test",
        "-c",
        "my_section.my_key=my_value",
    )
    trace_id = json.loads(res.stdout)["trace_id"]
    previous_invalidating_command = await filter_events(
        buck, "Event", "data", "Instant", "data", "PreviousCommandWithMismatchedConfig"
    )
    # No invalidation, no PreviousInvalidatingCommand fired
    assert len(previous_invalidating_command) == 0

    # Rerun with changes to commandline config
    await buck.build(
        "@root//mode/my_mode",
        "//:test",
        "-c",
        "my_section.my_key=my_new_value",
    )

    previous_invalidating_command = await filter_events(
        buck, "Event", "data", "Instant", "data", "PreviousCommandWithMismatchedConfig"
    )
    assert len(previous_invalidating_command) == 1
    assert previous_invalidating_command[0]["trace_id"] == trace_id
    sanitized_argv = previous_invalidating_command[0]["sanitized_argv"]
    assert (
        # sanitized_argv[0] contains the path to the buck executable which is different on every machine
        sanitized_argv[1] == "build"
        and sanitized_argv[2] == "@root//mode/my_mode"
        and sanitized_argv[3] == "//:test"
        and sanitized_argv[4] == "-c"
        and sanitized_argv[5] == "my_section.my_key=my_value"
    )
    # Make a change to .buckconfig
    with open(buck.cwd / ".buckconfig", "a") as buckconfig:
        buckconfig.write("\n[test_section]\ntest_key = test_value\n")
        await buck.build(
            "@root//mode/my_mode",
            "//:test",
            "-c",
            "my_section.my_key=my_new_value",
        )

    # Previous command didn't change any external configs but still has new_configs_used = 1 due to changes to project-relative configs.
    # We don't capture that by design at the moment.
    res = await buck.build(
        "@root//mode/my_mode",
        "//:test",
        "-c",
        "my_section.my_key=my_new_value",
    )
    trace_id = json.loads(res.stdout)["trace_id"]

    previous_invalidating_command = await filter_events(
        buck, "Event", "data", "Instant", "data", "PreviousCommandWithMismatchedConfig"
    )
    assert len(previous_invalidating_command) == 0
    assert res.invocation_record()["new_configs_used"] == 1

    # Make a change to .buckconfig.local
    with open(buck.cwd / ".buckconfig.local", "w") as localconfig:
        localconfig.write("\n[local_section]\nlocal_key = local_value\n")
    await buck.build(
        "@root//mode/my_mode",
        "//:test",
        "-c",
        "my_section.my_key=my_new_value",
    )
    previous_invalidating_command = await filter_events(
        buck, "Event", "data", "Instant", "data", "PreviousCommandWithMismatchedConfig"
    )
    assert len(previous_invalidating_command) == 1
    assert previous_invalidating_command[0]["trace_id"] == trace_id


@dataclass
class ExternalConfigsLog:
    descriptor: str
    cell: Optional[str] = None
    origin: Optional[str] = None


@buck_test()
async def test_log_external_configs(buck: Buck) -> None:
    await buck.build(
        "@root//mode/my_mode",
        "//:test",
        "-c",
        "my_section.my_key=my_value",
        "-c",
        "my_section.my_key=my_new_value",
    )

    external_configs = (await buck.log("external-configs")).stdout.strip().splitlines()
    external_configs = [e.split("\t") for e in external_configs]

    external_configs = [
        ExternalConfigsLog(
            e[0],  # section.key = value
            e[1] if len(e) > 1 else None,  # cell
            e[2] if len(e) > 2 else None,  # origin
        )
        for e in external_configs
    ]
    expected = [
        # Our tests inject file_watcher to external configs in test setup stage
        ExternalConfigsLog(
            descriptor="buck2.file_watcher = fs_hash_crawler",
            origin=buck._env["BUCK2_TEST_EXTRA_EXTERNAL_CONFIG"],
        ),
        ExternalConfigsLog(
            descriptor="my_mode.bcfg",
            origin="config-file",
        ),
        ExternalConfigsLog(
            descriptor="my_section.my_key = my_value",
            origin="cli",
        ),
        # We don't override but just display the input as it is provided by the cli
        ExternalConfigsLog(
            descriptor="my_section.my_key = my_new_value",
            origin="cli",
        ),
    ]
    assert len(external_configs) == 4

    for s, e in zip(external_configs, expected):
        assert s.descriptor == e.descriptor
        if s.origin and e.origin:
            assert e.origin == s.origin, (
                f"Expected origin '{e.origin}', but got '{s.origin}'"
            )


@buck_test()
async def test_log_external_configs_json(buck: Buck) -> None:
    await buck.build(
        "@root//mode/my_mode",
        "//:test",
        "-c",
        "my_section.my_key=my_value",
        "-c",
        "my_section.my_key=my_new_value",
    )

    external_configs = (
        (await buck.log("external-configs", "--format", "json"))
        .stdout.strip()
        .splitlines()
    )
    external_configs = [json.loads(e) for e in external_configs]

    tmp_path = os.path.dirname(buck._env["BUCK2_TEST_EXTRA_EXTERNAL_CONFIG"])
    golden_replace_temp_path(
        output=json.dumps(external_configs, sort_keys=True, indent=2),
        rel_path="events.golden.json",
        tmp_path=tmp_path,
    )
