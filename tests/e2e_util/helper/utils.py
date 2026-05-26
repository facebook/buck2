# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.


import json
import random
import re
import string
import sys
import typing
from configparser import ConfigParser
from pathlib import Path

import psutil
from buck2.tests.e2e_util.api.buck import Buck
from buck2.tests.e2e_util.api.buck_result import InvocationRecord


def daemon_is_alive(pid: int) -> bool:
    return psutil.pid_exists(pid)


async def read_what_ran(buck: Buck, *args) -> typing.List[typing.Dict[str, typing.Any]]:
    out = await buck.log("what-ran", "--format", "json", *args)
    out = [line.strip() for line in out.stdout.splitlines()]
    out = [json.loads(line) for line in out if line]
    return out


def timestamp_ms(s: int, ns: int) -> int:
    f = int(ns / 1000000)
    assert f < 1000
    return s * 1000 + f


async def read_timestamps(buck: Buck, *args) -> typing.List[int]:
    log = (await buck.log("show")).stdout.strip().splitlines()
    return [
        timestamp_ms(*json.loads(line)["Event"]["timestamp"])
        for line in log
        if json_get(line, *args) is not None
    ]


def is_running_on_linux() -> bool:
    return sys.platform == "linux"


def is_running_on_mac() -> bool:
    return sys.platform == "darwin"


def is_running_on_windows() -> bool:
    return sys.platform == "win32"


def get_targets_from_what_ran(
    what_ran: typing.List[typing.Dict[str, typing.Any]],
) -> typing.Set[typing.Tuple[str, str]]:
    targets = set()

    for entry in what_ran:
        m = re.match(r"^(.*?)( \((.*?)\))?( \((.*?)\))?$", entry["identity"])
        assert m is not None
        rule, category = m.group(1), m.group(5)
        targets.add((rule, category))

    return targets


async def expect_exec_count(buck: Buck, n: int) -> None:
    out = await read_what_ran(buck)
    assert len(out) == n, "unexpected actions: %s" % (out,)


async def filter_events(
    buck: Buck,
    *args: str,
    rel_cwd: typing.Optional[Path] = None,
    return_root: bool = False,
) -> typing.List[typing.Any]:
    log = (await buck.log("show", rel_cwd=rel_cwd)).stdout.strip().splitlines()
    found = []
    for line in log:
        e = json_get(line, *args, return_root_on_match=return_root)
        if e is None:
            continue
        found.append(e)
    return found


def json_get(data: str, *key: str, return_root_on_match: bool = False) -> typing.Any:
    root = json.loads(data)
    cur = root

    for k in key:
        cur = cur.get(k)
        if cur is None:
            return None

    return root if return_root_on_match else cur


def random_string() -> str:
    return "".join(random.choice(string.ascii_lowercase) for i in range(256))


def replace_hashes(strings: typing.List[str]) -> typing.List[str]:
    return [replace_hash(s) for s in strings]


def replace_hash(s: str) -> str:
    return re.sub(r"\b[0-9a-f]{16}\b", "<HASH>", s)


def replace_digest(s: str) -> str:
    return re.sub(r"\b[0-9a-f]{40}:[0-9]{1,3}\b", "<DIGEST>", s)


async def get_buck2_re_use_case(buck: Buck) -> str:
    key = "buck2_re_client.override_use_case"
    config = (
        await buck.audit_config("--reuse-current-config", "--style=json", key)
    ).get_json()
    use_case = config.get(key)
    if use_case is not None:
        return use_case

    # The test harness's extra external config is part of normal Buck config
    # parsing, but `override_use_case` is filtered out of DICE-backed config
    # reads, so `audit config --reuse-current-config` may not report it. We need
    # to manually check the test config here.
    extra_config_path = buck.get_env_var("BUCK2_TEST_EXTRA_EXTERNAL_CONFIG")
    if extra_config_path is not None:
        use_case = _get_buck2_re_use_case_from_config_file(Path(extra_config_path))
        if use_case is not None:
            return use_case

    return "buck2-default"


def _get_buck2_re_use_case_from_config_file(path: Path) -> typing.Optional[str]:
    parser = ConfigParser(strict=False)
    if not parser.read(path):
        return None
    return parser.get("buck2_re_client", "override_use_case", fallback=None)


def read_invocation_record(record: Path) -> InvocationRecord:
    return InvocationRecord(record)


async def get_last_execution_kind(
    buck: Buck,
    category: typing.Optional[str] = None,
    excluded_execution_kinds: typing.Optional[typing.List[int]] = None,
    target_name: typing.Optional[str] = None,
) -> typing.Optional[int]:
    if excluded_execution_kinds is None:
        excluded_execution_kinds = []
    action_executions = await filter_events(
        buck,
        "Event",
        "data",
        "SpanEnd",
        "data",
        "ActionExecution",
    )
    for action_execution in reversed(action_executions):
        execution_kind = action_execution.get("execution_kind", None)

        if execution_kind is None or execution_kind in excluded_execution_kinds:
            continue

        if category is not None:
            action_category = action_execution.get("name", {}).get("category", None)
            if action_category != category:
                continue

        if target_name is not None:
            action_target_name = (
                action_execution.get("key", {})
                .get("owner", {})
                .get("TargetLabel", {})
                .get("label", {})
                .get("name", None)
            )
            if action_target_name != target_name:
                continue

        return execution_kind

    return None
