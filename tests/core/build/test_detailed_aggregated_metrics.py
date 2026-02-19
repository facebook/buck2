# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.


import typing

import pytest
from buck2.tests.e2e_util.api.buck import Buck
from buck2.tests.e2e_util.asserts import expect_failure
from buck2.tests.e2e_util.buck_workspace import buck_test
from buck2.tests.e2e_util.helper.utils import json_get


async def get_detailed_metrics(buck: Buck) -> typing.Any:
    log = (await buck.log("show")).stdout.strip().splitlines()

    for line in log:
        message = json_get(
            line,
            "Event",
            "data",
            "Instant",
            "data",
            "DetailedAggregatedMetrics",
        )
        if message is not None:
            return message

    return None


def parse_metrics(metrics: typing.Any) -> tuple[typing.Any, dict[str, typing.Any]]:
    all_targets = metrics["all_targets_build_metrics"]

    def stringify(t) -> str:
        t = t["label"]
        return f"{t['package']}:{t['name']}"

    per_targets = {
        stringify(v["target"]): v for v in metrics["top_level_target_metrics"]
    }
    return (all_targets, per_targets)


@buck_test()
async def test_disabled(buck: Buck) -> None:
    await buck.build("//:foo4", "-c", "buck2.detailed_aggregated_metrics=false")
    message = await get_detailed_metrics(buck)
    assert message is None


@buck_test()
async def test_enabled(buck: Buck) -> None:
    await buck.build("//:foo4", "-c", "buck2.detailed_aggregated_metrics=true")
    message = await get_detailed_metrics(buck)
    assert message is not None
    all_targets_metrics, per_target_metrics = parse_metrics(message)
    assert [
        all_targets_metrics["action_graph_size"],
        per_target_metrics["root//:foo4"]["action_graph_size"],
        per_target_metrics["root//:foo4"]["metrics"]["declared_actions"],
    ] == [7, 7, pytest.approx(12.0)]


@buck_test()
async def test_incomplete_graph(buck: Buck) -> None:
    await expect_failure(
        buck.build(
            "//:foo4",
            "-c",
            "buck2.detailed_aggregated_metrics=true",
            "-c",
            "user.dyn_input_good=0",
        )
    )
    message = await get_detailed_metrics(buck)
    assert message is not None
    all_targets_metrics, per_target_metrics = parse_metrics(message)
    assert [
        all_targets_metrics["action_graph_size"],
        per_target_metrics["root//:foo4"]["action_graph_size"],
        per_target_metrics["root//:foo4"]["metrics"]["declared_actions"],
    ] == [None, None, pytest.approx(12.0)]


@buck_test()
async def test_amortization(buck: Buck) -> None:
    await buck.build(
        "//:foo4", "//:foo5", "-c", "buck2.detailed_aggregated_metrics=true"
    )
    message = await get_detailed_metrics(buck)
    assert message is not None
    all_targets_metrics, per_target_metrics = parse_metrics(message)
    assert [
        all_targets_metrics["action_graph_size"],
        all_targets_metrics["metrics"]["declared_actions"],
        per_target_metrics["root//:foo4"]["action_graph_size"],
        per_target_metrics["root//:foo4"]["metrics"]["declared_actions"],
        per_target_metrics["root//:foo4"]["amortized_metrics"]["declared_actions"],
        per_target_metrics["root//:foo5"]["action_graph_size"],
        per_target_metrics["root//:foo5"]["metrics"]["declared_actions"],
        per_target_metrics["root//:foo5"]["amortized_metrics"]["declared_actions"],
    ] == [
        11,
        pytest.approx(15.0),
        7,
        pytest.approx(12.0),
        pytest.approx(7.5),
        7,
        pytest.approx(12.0),
        pytest.approx(7.5),
    ]
