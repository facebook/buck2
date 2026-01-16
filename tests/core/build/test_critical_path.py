# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

# pyre-strict


import json
import typing
from dataclasses import dataclass

from buck2.tests.e2e_util.api.buck import Buck
from buck2.tests.e2e_util.buck_workspace import buck_test
from buck2.tests.e2e_util.helper.golden import golden
from buck2.tests.e2e_util.helper.utils import filter_events


@dataclass
class CriticalPathLog:
    kind: str
    name: str
    category: str
    identifier: str
    execution_kind: str
    total_duration: str
    user_duration: str
    potential_improvement_duration: str


async def do_critical_path(buck: Buck) -> None:
    await buck.build("//:step_3", "--no-remote-cache")

    critical_path = (await buck.log("critical-path")).stdout.strip().splitlines()
    critical_path = [e.split("\t") for e in critical_path]

    trimmed_critical_path = [
        CriticalPathLog(e[0], e[1].split(" ")[0], e[2], e[3], e[4], e[5], e[6], e[7])
        for e in critical_path
        if e[0] not in ("waiting")
    ]

    expected = [
        ("buckd_command_init", ""),
        ("file-watcher-wait", ""),
        ("other-command-start-overhead", ""),
        ("listing", "root//"),
        ("load", "root//"),
        ("analysis", "root//:step_0"),
        ("analysis", "root//:step_1"),
        ("analysis", "root//:step_2"),
        ("analysis", "root//:step_3"),
        ("action", "root//:step_0"),
        ("action", "root//:step_1"),
        ("action", "root//:step_2"),
        ("action", "root//:step_3"),
        ("materialization", "root//:step_3"),
        ("compute-critical-path", ""),
    ]

    # Check this second because the loop above gives a better error when one is wrong.
    assert len(trimmed_critical_path) == len(expected)

    for s, e in zip(reversed(trimmed_critical_path), reversed(expected)):
        if s.kind == "action":
            assert s.execution_kind != ""
            assert s.execution_kind != "ACTION_EXECUTION_NOTSET"
        else:
            assert s.execution_kind == ""

        assert s.kind == e[0]
        assert s.name == e[1]


@buck_test()
async def test_critical_path_longest_path_graph(buck: Buck) -> None:
    with open(buck.cwd / ".buckconfig", "a") as f:
        f.write("[buck2]\n")
        f.write("critical_path_backend2 = longest-path-graph\n")
    await do_critical_path(buck)


@buck_test()
async def test_critical_path_json(buck: Buck) -> None:
    import json

    await buck.build("//:step_3", "--no-remote-cache")
    critical_path = (
        (await buck.log("critical-path", "--format", "json"))
        .stdout.strip()
        .splitlines()
    )
    critical_path = [json.loads(e) for e in critical_path]

    trimmed_critical_path = [e for e in critical_path if e["kind"] not in ("waiting")]

    expected = [
        ("buckd_command_init", None),
        ("file-watcher-wait", None),
        ("other-command-start-overhead", None),
        ("listing", "root//"),
        ("load", "root//"),
        ("analysis", "root//:step_0"),
        ("analysis", "root//:step_1"),
        ("analysis", "root//:step_2"),
        ("analysis", "root//:step_3"),
        ("action", "root//:step_0"),
        ("action", "root//:step_1"),
        ("action", "root//:step_2"),
        ("action", "root//:step_3"),
        ("materialization", "root//:step_3"),
        ("compute-critical-path", None),
    ]
    assert len(trimmed_critical_path) == len(expected)

    for critical, exp in zip(reversed(trimmed_critical_path), reversed(expected)):
        assert "kind" in critical
        assert critical["kind"] == exp[0]

        if critical["kind"] in (
            "waiting",
            "compute-critical-path",
            "file-watcher-wait",
            "other-command-start-overhead",
            "buckd_command_init",
        ):
            assert "name" not in critical
        else:
            assert "name" in critical
            name = critical["name"].split(" ")[0]
            assert name == exp[1]

        if critical["kind"] == "action":
            assert "execution_kind" in critical
            assert critical["execution_kind"] != ""
            assert critical["execution_kind"] != "ACTION_EXECUTION_NOTSET"
        else:
            assert "execution_kind" not in critical


# Test that verifies the dicekey->node+deps graph that we produce for critical path
# calculations. It can be a lot easier to understand bugs and behavior here than
# only inspecting the final critical path output (like other tests).
@buck_test()
async def test_dynamic_input_events(buck: Buck) -> None:
    with open(buck.cwd / ".buckconfig", "a") as f:
        f.write("[buck2]\n")
        f.write("critical_path_backend2 = logging\n")

    await buck.build("//:check_dynamic_input", "--no-remote-cache")
    events = await filter_events(
        buck,
        "Event",
        "data",
        "Instant",
        "data",
        "UnstableE2eData",
    )

    events = [
        json.loads(ev["data"])
        for ev in events
        if ev["key"] == "critical_path_logging_node"
    ]

    golden(
        output=json.dumps(events, sort_keys=True, indent=2),
        rel_path="events.golden.json",
    )


# Test that we can compute critical paths that include edges as inputs of dynamic_output/actions.
@buck_test()
async def test_dynamic_input(buck: Buck) -> None:
    import json

    await buck.build("//:check_dynamic_input", "--no-remote-cache")
    critical_path = (
        await buck.log("critical-path", "--format", "json")
    ).stdout.splitlines()
    critical_path = [json.loads(e) for e in critical_path]

    assert len(critical_path) > 0
    transformed = []
    for critical in critical_path:
        assert "kind" in critical
        t = critical["kind"]

        if t in ("waiting"):
            continue

        if critical["kind"] in (
            "compute-critical-path",
            "file-watcher-wait",
            "other-command-start-overhead",
            "buckd_command_init",
        ):
            assert "name" not in critical
        else:
            assert "name" in critical
            name = critical["name"].split(" ")[0]
            t = "{} {}".format(t, name)

        if critical["kind"] == "action":
            assert "execution_kind" in critical
            assert critical["execution_kind"] != ""
            assert critical["execution_kind"] != "ACTION_EXECUTION_NOTSET"
        else:
            assert "execution_kind" not in critical

        # there's nondeterminism in critical path here because step1 action
        # depends on both step0 action and step1 analysis, both of those depend
        # on step0 analysis.
        if t in ["analysis root//:step_1", "action root//:step_0"]:
            continue

        transformed.append(t)

    golden(
        output=json.dumps(transformed, indent=2),
        rel_path="dynamic_input.golden.json",
    )


@buck_test()
async def test_critical_path_metadata(buck: Buck) -> None:
    await buck.build(
        "//:step_0",
        "--no-remote-cache",
        "-c",
        "client.id=myclient",
        "--oncall=myoncall",
    )

    build_graph_info = await filter_events(
        buck,
        "Event",
        "data",
        "Instant",
        "data",
        "BuildGraphInfo",
    )

    build_graph_info = build_graph_info[0]
    assert build_graph_info
    assert "username" in build_graph_info["metadata"]
    assert build_graph_info["metadata"]["client"] == "myclient"
    assert build_graph_info["metadata"]["oncall"] == "myoncall"


async def critical_path_helper(buck: Buck) -> typing.List[typing.Dict[str, typing.Any]]:
    critical_path_actions = await filter_events(
        buck,
        "Event",
        "data",
        "Instant",
        "data",
        "BuildGraphInfo",
        "critical_path2",
    )

    assert len(critical_path_actions) == 1
    return critical_path_actions[0]


@buck_test()
async def test_critical_path_execution_kind(buck: Buck) -> None:
    await buck.build("//:step_3", "--no-remote-cache")

    critical_path_actions = await critical_path_helper(buck)

    has_action_execution = False
    for action in critical_path_actions:
        assert action["entry"]
        # Every ActionExecution should have an execution kind and it shouldn't be 0 (default)
        if "ActionExecution" in action["entry"]:
            has_action_execution = True
            assert action["entry"]["ActionExecution"]["execution_kind"]
            assert action["entry"]["ActionExecution"]["execution_kind"] != 0

    # Should have at least 1 ActionExecution or something went wrong
    assert has_action_execution


@buck_test()
async def test_critical_path_rule_type(buck: Buck) -> None:
    await buck.build("//:step_0", "--no-remote-cache")

    critical_path_actions = await critical_path_helper(buck)

    for action in critical_path_actions:
        assert action["entry"]

        if "ActionExecution" in action["entry"]:
            assert action["entry"]["ActionExecution"]["target_rule_type_name"]
            assert (
                action["entry"]["ActionExecution"]["target_rule_type_name"] == "write"
            )


@buck_test()
async def test_critical_path_action_digest(buck: Buck) -> None:
    await buck.build("//:step_3", "--no-remote-cache")

    critical_path_actions = await critical_path_helper(buck)

    has_action_digest = False
    for action in critical_path_actions:
        assert action["entry"]
        if "ActionExecution" in action["entry"]:
            if "action_digest" in action["entry"]["ActionExecution"]:
                has_action_digest = True

    assert has_action_digest


@buck_test()
async def test_critical_path_top_level_targets(buck: Buck) -> None:
    await buck.build("//:step_1", "//:step_2", "//:step_3", "--no-remote-cache")

    build_graph_info = await filter_events(
        buck,
        "Event",
        "data",
        "Instant",
        "data",
        "BuildGraphInfo",
    )

    build_graph_info = build_graph_info[0]
    top_level_targets = build_graph_info["top_level_targets"]

    # Sort by duration.
    top_level_targets = sorted(top_level_targets, key=lambda x: x["duration_us"])
    (t0, t1, t2) = [x["target"]["label"]["name"] for x in top_level_targets]
    (d0, d1, d2) = [x["duration_us"] for x in top_level_targets]

    assert t0 == "step_1"
    assert t1 == "step_2"
    assert t2 == "step_3"
    assert d0 <= d1
    assert d1 <= d2

    # The duration should match the relevant build steps:
    total_duration = sum(
        x["total_duration_us"]
        for x in build_graph_info["critical_path2"]
        if len(
            {"GenericEntry", "ComputeCriticalPath", "FinalMaterialization"}
            & x["entry"].keys()
        )
        == 0
    )
    assert total_duration == d2


@buck_test()
async def test_critical_path_test_entries(buck: Buck) -> None:
    await buck.test(
        "//:long_running_test",
    )

    critical_path_actions = await critical_path_helper(buck)

    # Should have exactly 1 TestListing.
    test_listing_actions = [
        action for action in critical_path_actions if "TestListing" in action["entry"]
    ]
    assert len(test_listing_actions) == 1

    # Assert there is 1 TestExecution with the correct data.
    test_execution_actions = [
        action for action in critical_path_actions if "TestExecution" in action["entry"]
    ]

    assert len(test_execution_actions) == 1
    test_execution_action = test_execution_actions[0]
    assert (
        test_execution_action["entry"]["TestExecution"]["suite"]
        == "root//:long_running_test"
    )
    assert test_execution_action["duration_us"] > 100000  # 100ms
