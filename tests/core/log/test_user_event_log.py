# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

# pyre-strict


import json
import re
from pathlib import Path

import pytest
from buck2.tests.e2e_util.api.buck import Buck
from buck2.tests.e2e_util.buck_workspace import buck_test
from buck2.tests.e2e_util.helper.golden import golden


def _replace_timestamp(s: str) -> str:
    return re.sub(r"\b[0-9]+\b", "<NUMBER>", s)


@buck_test(skip_for_os=["windows"])
async def test_user_event_log_custom_output(buck: Buck, tmp_path: Path) -> None:
    local_log = tmp_path / "test.json"

    await buck.bxl(
        "root//:test.bxl:instant_event",
        "--user-event-log",
        str(local_log),
    )

    assert Path(local_log).exists()

    # do some basic validation - golden tests take care of better validation
    with open(local_log, "r") as f:
        results = f.read().splitlines()
        # assert these events can be loaded
        json.loads(results[0])["command_line_args"]
        json.loads(results[1])["StarlarkUserEvent"]
        json.loads(results[2])["StarlarkUserEvent"]


@buck_test(skip_for_os=["windows"])
async def test_user_event_log_with_actions(buck: Buck, tmp_path: Path) -> None:
    local_log = tmp_path / "test.json-lines"

    await buck.bxl(
        "root//:test.bxl:action",
        "--event-log",
        str(local_log),
    )

    results = (
        (await buck.log("show-user", str(Path(local_log).absolute())))
        .stdout.strip()
        .splitlines()[1:]
    )

    # Remove any durations
    a = json.loads(results[0])
    a["ActionExecutionEvent"]["duration_millis"] = "<NUMBER>"
    a["ActionExecutionEvent"]["input_materialization_duration_millis"] = "<NUMBER>"
    b = json.loads(results[1])
    b["BxlEnsureArtifactsEvent"]["duration_millis"] = "<NUMBER>"

    results = _replace_timestamp(f"{json.dumps(a)}\n{json.dumps(b)}")

    # Just validate the user events, let's skip the invocation record
    golden(
        output=results,
        rel_path="action_event.golden.json",
    )


@buck_test(skip_for_os=["windows"])
async def test_user_event_with_log_show_user(buck: Buck) -> None:
    await buck.bxl(
        "root//:test.bxl:instant_event",
    )

    results = (await buck.log("show-user")).stdout.strip().splitlines()[1:]

    results = _replace_timestamp("\n".join(results))

    # Just validate the user events, let's skip the invocation record
    golden(
        output=results,
        rel_path="instant_event.golden.json",
    )


@buck_test(skip_for_os=["windows"])
@pytest.mark.parametrize(
    "file_extension", [".json-lines", ".json-lines.gz", ".json-lines.zst"]
)
async def test_user_event_log_with_log_show_user_compatibility(
    buck: Buck,
    tmp_path: Path,
    file_extension: str,
) -> None:
    local_log = tmp_path / f"test.{file_extension}"

    await buck.bxl(
        "root//:test.bxl:instant_event",
        "--event-log",
        str(local_log),
    )

    results = (
        (await buck.log("show-user", str(Path(local_log).absolute())))
        .stdout.strip()
        .splitlines()[1:]
    )

    results = _replace_timestamp("\n".join(results))

    # Just validate the user events, let's skip the invocation record
    golden(
        output=results,
        rel_path="instant_event.golden.json",
    )


# Placeholder for tests to be listed successfully on Windows.
@buck_test()
async def test_noop(buck: Buck) -> None:
    return
