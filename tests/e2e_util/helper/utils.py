# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

# pyre-unsafe

import json
import random
import re
import string
import sys
import typing

from buck2.tests.e2e_util.api.buck import Buck


async def read_what_ran(buck: Buck, *args) -> typing.List[typing.Dict[str, typing.Any]]:
    out = await buck.log("what-ran", "--format", "json", *args)
    out = [line.strip() for line in out.stdout.splitlines()]
    out = [json.loads(line) for line in out if line]
    return out


def is_running_on_linux() -> bool:
    return sys.platform == "linux"


def is_running_on_mac() -> bool:
    return sys.platform == "darwin"


def is_running_on_windows() -> bool:
    return sys.platform == "win32"


def get_targets_from_what_ran(what_ran):
    targets = set()

    for entry in what_ran:
        m = re.match(r"^(.*?)( \((.*?)\))?( \((.*?)\))?$", entry["identity"])
        rule, category = m.group(1), m.group(5)
        targets.add((rule, category))

    return targets


async def expect_exec_count(buck: Buck, n: int) -> None:
    out = await read_what_ran(buck)
    assert len(out) == n, "unexpected actions: %s" % (out,)


async def filter_events(buck: Buck, *args):
    log = (await buck.log("show")).stdout.strip().splitlines()
    found = []
    for line in log:
        e = json_get(line, *args)
        if e is None:
            continue
        found.append(e)
    return found


def json_get(data, *key):
    data = json.loads(data)

    for k in key:
        data = data.get(k)
        if data is None:
            break

    return data


def random_string():
    return "".join(random.choice(string.ascii_lowercase) for i in range(256))


def replace_hashes(strings: typing.List[str]) -> typing.List[str]:
    return [_replace_hash(s) for s in strings]


def _replace_hash(s: str) -> str:
    return re.sub(r"\b[0-9a-f]{16}\b", "<HASH>", s)
