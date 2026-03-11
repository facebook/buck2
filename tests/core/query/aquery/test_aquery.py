# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

# pyre-strict


from buck2.tests.e2e_util.api.buck import Buck
from buck2.tests.e2e_util.buck_workspace import buck_test
from buck2.tests.e2e_util.helper.golden import golden


@buck_test()
async def test_target(buck: Buck) -> None:
    stdout = (await buck.aquery("//:test", "-a", "identifier")).stdout

    golden(
        output=stdout,
        rel_path="target.golden.json",
    )


@buck_test()
async def test_all_outputs(buck: Buck) -> None:
    stdout = (await buck.aquery("all_outputs(//:test)", "-a", "identifier")).stdout

    golden(
        output=stdout,
        rel_path="all_outputs.golden.json",
    )


@buck_test()
async def test_all_actions(buck: Buck) -> None:
    stdout = (await buck.aquery("all_actions(//:test)", "-a", "identifier")).stdout

    golden(
        output=stdout,
        rel_path="all_actions.golden.json",
    )


@buck_test()
async def test_all_outputs_subtarget(buck: Buck) -> None:
    stdout = (
        await buck.aquery("all_outputs('//:test[sub]')", "-a", "identifier")
    ).stdout

    golden(
        output=stdout,
        rel_path="all_outputs_subtarget.golden.json",
    )


@buck_test()
async def test_filter(buck: Buck) -> None:
    stdout = (
        await buck.aquery(
            "attrfilter('identifier', 'other', all_actions('//:test[sub]'))",
            "-a",
            "identifier",
        )
    ).stdout

    golden(
        output=stdout,
        rel_path="filter.golden.json",
    )


@buck_test()
async def test_deps(buck: Buck) -> None:
    stdout = (await buck.aquery("deps(//:test)", "-a", "identifier")).stdout

    golden(
        output=stdout,
        rel_path="deps.golden.json",
    )


@buck_test()
async def test_bxl_aquery_target(buck: Buck) -> None:
    stdout = (await buck.bxl("//:aquery.bxl:target")).stdout
    golden(
        output=stdout,
        rel_path="bxl_target.golden.json",
    )


@buck_test()
async def test_bxl_aquery_all_outputs(buck: Buck) -> None:
    stdout = (await buck.bxl("//:aquery.bxl:all_outputs")).stdout

    golden(
        output=stdout,
        rel_path="bxl_all_outputs.golden.json",
    )


@buck_test()
async def test_bxl_aquery_all_actions(buck: Buck) -> None:
    stdout = (await buck.bxl("//:aquery.bxl:all_actions")).stdout

    golden(
        output=stdout,
        rel_path="bxl_all_actions.golden.json",
    )


@buck_test()
async def test_bxl_aquery_all_outputs_subtarget(buck: Buck) -> None:
    stdout = (await buck.bxl("//:aquery.bxl:all_outputs_subtarget")).stdout

    golden(
        output=stdout,
        rel_path="bxl_all_outputs_subtarget.golden.json",
    )


@buck_test()
async def test_bxl_aquery_attrfilter(buck: Buck) -> None:
    stdout = (await buck.bxl("//:aquery.bxl:attrfilter")).stdout

    golden(
        output=stdout,
        rel_path="bxl_filter.golden.json",
    )


@buck_test()
async def test_bxl_aquery_deps(buck: Buck) -> None:
    stdout = (await buck.bxl("//:aquery.bxl:deps")).stdout

    golden(
        output=stdout,
        rel_path="bxl_deps.golden.json",
    )


@buck_test()
async def test_bxl_aquery_eval(buck: Buck) -> None:
    stdout = (await buck.bxl("//:aquery.bxl:eval")).stdout

    golden(
        output=stdout,
        rel_path="bxl_eval.golden.json",
    )


@buck_test()
async def test_bxl_aquery_action_query_node(buck: Buck) -> None:
    await buck.bxl("//:aquery.bxl:action_query_node")


# Tests for bxl.Action support in aquery operations
@buck_test()
async def test_bxl_action_deps_0(buck: Buck) -> None:
    """Test passing a bxl.Action to aquery.deps() with depth=0 returns itself"""
    await buck.bxl("//:aquery.bxl:action_deps_0")


@buck_test()
async def test_bxl_action_deps(buck: Buck) -> None:
    """Test that you can isolate the deps of one action by itself"""
    await buck.bxl("//:aquery.bxl:action_deps")
