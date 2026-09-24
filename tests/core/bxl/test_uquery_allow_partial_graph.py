# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

from buck2.tests.e2e_util.api.buck import Buck
from buck2.tests.e2e_util.asserts import expect_failure
from buck2.tests.e2e_util.buck_workspace import buck_test

BXL = "//:allow_partial_graph.bxl:"


@buck_test()
async def test_deps_defaults_to_strict(buck: Buck) -> None:
    await expect_failure(
        buck.bxl(BXL + "deps_default"),
        stderr_regex="intentional parse error",
    )


@buck_test()
async def test_deps_allow_partial_graph_skips_broken_edge(buck: Buck) -> None:
    await buck.bxl(BXL + "deps_partial")


@buck_test()
async def test_rdeps_defaults_to_strict(buck: Buck) -> None:
    await expect_failure(
        buck.bxl(BXL + "rdeps_default"),
        stderr_regex="intentional parse error",
    )


@buck_test()
async def test_rdeps_allow_partial_graph_skips_broken_package(buck: Buck) -> None:
    await buck.bxl(BXL + "rdeps_partial")


@buck_test()
async def test_recursive_pattern_as_typed_arg_is_not_covered(buck: Buck) -> None:
    await buck.bxl(BXL + "rdeps_partial_typed_pattern")


@buck_test()
async def test_eval_recursive_defaults_to_strict(buck: Buck) -> None:
    await expect_failure(
        buck.bxl(BXL + "eval_recursive_default"),
        stderr_regex="intentional parse error",
    )


@buck_test()
async def test_eval_recursive_allow_partial_graph_skips_broken_package(
    buck: Buck,
) -> None:
    await buck.bxl(BXL + "eval_recursive_partial")


@buck_test()
async def test_explicit_missing_target_fails_with_allow_partial_graph(
    buck: Buck,
) -> None:
    await expect_failure(
        buck.bxl(BXL + "explicit_missing_target"),
        stderr_regex="Unknown target `nonexistent`",
    )


@buck_test()
async def test_explicit_broken_target_fails_with_allow_partial_graph(
    buck: Buck,
) -> None:
    await expect_failure(
        buck.bxl(BXL + "explicit_broken_target"),
        stderr_regex="intentional parse error",
    )


@buck_test()
async def test_lazy_deps_defaults_to_strict(buck: Buck) -> None:
    await buck.bxl(BXL + "lazy_deps_default")


@buck_test()
async def test_lazy_deps_allow_partial_graph_skips_broken_edge(buck: Buck) -> None:
    await buck.bxl(BXL + "lazy_deps_partial")


@buck_test()
async def test_lazy_eval_recursive_defaults_to_strict(buck: Buck) -> None:
    await buck.bxl(BXL + "lazy_eval_recursive_default")


@buck_test()
async def test_lazy_eval_recursive_allow_partial_graph_skips_broken_package(
    buck: Buck,
) -> None:
    await buck.bxl(BXL + "lazy_eval_recursive_partial")


@buck_test()
async def test_lazy_explicit_broken_target_fails_with_allow_partial_graph(
    buck: Buck,
) -> None:
    await buck.bxl(BXL + "lazy_explicit_broken_target")
