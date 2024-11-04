# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

# pyre-strict


import collections
import json
import os
import re
from pathlib import Path
from typing import Any, Dict

from buck2.tests.e2e_util.api.buck import Buck
from buck2.tests.e2e_util.buck_workspace import buck_test


def fixture(name: str) -> str:
    p = Path(os.environ["FIXTURES"]) / f"{name}.proto"
    return str(p.absolute())


@buck_test()
async def test_console_facts(buck: Buck) -> None:
    res = await buck.log(
        "replay", fixture("my_genrule0"), "--", "build", "--console", "simple"
    )
    assert re.search("Network: .*([0-9.]+)([KMG]?)B", res.stderr) is not None
    assert "Cache hits: 100%" in res.stderr
    assert "Commands: 1 (cached: 1, remote: 0, local: 0)" in res.stderr


@buck_test()
async def test_console_facts_no_repo(buck: Buck) -> None:
    res = await buck.log(
        "replay",
        fixture("my_genrule0"),
        "--",
        "build",
        "--console",
        "simple",
        rel_cwd=Path(os.path.relpath("/", buck.cwd)),
    )
    assert re.search("Network: .*([0-9.]+)([KMG]?)B", res.stderr) is not None
    assert "Cache hits: 100%" in res.stderr
    assert "Commands: 1 (cached: 1, remote: 0, local: 0)" in res.stderr


@buck_test()
async def test_super_console_facts(buck: Buck) -> None:
    res = await buck.log(
        "replay", fixture("my_genrule0"), "--", "build", "--console", "super"
    )
    assert re.search("Network: .*([0-9.]+)([KMG]?)B", res.stderr) is not None
    assert "Cache hits: 100%" in res.stderr
    assert "Commands: 1" in res.stderr


@buck_test()
async def test_whatran(buck: Buck) -> None:
    res = await buck.log(
        "what-ran",
        fixture("my_genrule0"),
    )
    assert "cache" in res.stdout
    assert (
        "87eeee88c133dfa39711399a81f500147275cfeeb1f06b6b4805f2c0588615d1:145"
        in res.stdout
    )


@buck_test()
async def test_whatran_no_repo(buck: Buck) -> None:
    res = await buck.log(
        "what-ran",
        fixture("my_genrule0"),
        rel_cwd=Path(os.path.relpath("/", buck.cwd)),
    )
    assert "cache" in res.stdout
    assert (
        "87eeee88c133dfa39711399a81f500147275cfeeb1f06b6b4805f2c0588615d1:145"
        in res.stdout
    )


@buck_test()
async def test_file_watcher_span_depth(buck: Buck) -> None:
    """
    We show spans up to depth 2 in the console. We should make sure that spans
    whose runtime depends on external tools (i.e. the file watcher) are
    displayed.
    """
    await buck.build()
    log = await buck.log("show")

    depths = collections.defaultdict(int)
    file_watcher_span = None

    for line in log.stdout.splitlines():
        line = json.loads(line)
        event = line.get("Event")
        if event is None:
            continue

        span = _get(event, "data", "SpanStart", "data")
        if span is None:
            continue

        # This event is relevant to us, but it's also not shown, so it means
        # its children are roots.
        if "DiceCriticalSection" in span:
            depth = -1
        else:
            depth = depths[event["parent_id"]] + 1

        depths[event["span_id"]] = depth

        if _get(event, "data", "SpanStart", "data", "FileWatcher"):
            file_watcher_span = event

    assert file_watcher_span is not None, "Did not find FileWatcher span"
    assert depths[file_watcher_span["span_id"]] <= 2


@buck_test()
async def test_stale_snapshot(buck: Buck, tmp_path: Path) -> None:
    original = fixture("my_genrule0")
    log = (await buck.log("show", original)).stdout

    # Now we're going to make a new log where we just delay the last event by
    # some amount of time.
    lines = log.splitlines()

    # Last event (last line is command result).
    last = lines[-2]
    last = json.loads(last)
    last["Event"]["timestamp"][0] += 20
    lines[-2] = json.dumps(last)

    logfile = tmp_path / "test.json-lines"

    with open(logfile, "w") as f:
        f.write("\n".join(lines))

    stale_message = "Resource usage: <snapshot is stale>"

    # Check it's there.
    res = await buck.log("replay", str(logfile), "--console", "simple")
    assert stale_message in res.stderr

    # Check it's not in the original one.
    res = await buck.log("replay", original, "--console", "simple")
    assert stale_message not in res.stderr


def _get(data: Dict[str, Any], *key: str) -> Dict[str, Any]:
    for k in key:
        data = data.get(k)
        if data is None:
            break

    return data
