# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

# pyre-strict


from buck2.tests.e2e_util.api.buck import Buck
from buck2.tests.e2e_util.asserts import expect_failure
from buck2.tests.e2e_util.buck_workspace import buck_test


@buck_test(inplace=False)
async def test_bare_select_typecheck(buck: Buck) -> None:
    """Bare Select type annotations should pass type checking."""
    await buck.starlark("typecheck", "select_types.bzl")


@buck_test(inplace=False)
async def test_select_union_accepts_both(buck: Buck) -> None:
    """Select[str] | str accepts both select expressions and plain strings."""
    await buck.starlark("typecheck", "select_union_good.bzl")


@buck_test(inplace=False)
async def test_select_rejects_plain_value(buck: Buck) -> None:
    """Select[str] rejects a plain str argument during type checking."""
    await expect_failure(
        buck.starlark("typecheck", "select_union_bad.bzl"),
        stderr_regex="Detected 2 errors",
    )


@buck_test(inplace=False)
async def test_nested_type_params(buck: Buck) -> None:
    """Nested type parameters like Select[list[str]] pass type checking."""
    await buck.starlark("typecheck", "select_nested_good.bzl")


@buck_test(inplace=False)
async def test_nested_type_params_reject_plain(buck: Buck) -> None:
    """Select[list[str]] rejects plain values during type checking."""
    await expect_failure(
        buck.starlark("typecheck", "select_nested_bad.bzl"),
        stderr_regex="Detected 2 errors",
    )


@buck_test(inplace=False)
async def test_optional_type_params(buck: Buck) -> None:
    """Select[str | None] accepts select expressions resolving to str or None."""
    await buck.starlark("typecheck", "select_optional_good.bzl")


@buck_test(inplace=False)
async def test_optional_type_params_reject_plain(buck: Buck) -> None:
    """Select[str | None] rejects plain values during type checking."""
    await expect_failure(
        buck.starlark("typecheck", "select_optional_bad.bzl"),
        stderr_regex="Detected 2 errors",
    )


@buck_test(inplace=False)
async def test_select_bool_parameterization(buck: Buck) -> None:
    """Select[bool] accepts select expressions with boolean branches."""
    await buck.starlark("typecheck", "select_bool_good.bzl")


@buck_test(inplace=False)
async def test_select_bool_rejects_plain(buck: Buck) -> None:
    """Select[bool] rejects plain boolean and string values."""
    await expect_failure(
        buck.starlark("typecheck", "select_bool_bad.bzl"),
        stderr_regex="Detected 2 errors",
    )


@buck_test(inplace=False)
async def test_select_str_not_equal_to_union(buck: Buck) -> None:
    """Select[str] alone accepts select expressions but not plain values."""
    await buck.starlark("typecheck", "select_semantics_good.bzl")


@buck_test(inplace=False)
async def test_select_str_rejects_plain_proving_not_union(buck: Buck) -> None:
    """Select[str] is not Select[str] | str: plain values are rejected."""
    await expect_failure(
        buck.starlark("typecheck", "select_semantics_bad.bzl"),
        stderr_regex="Detected 2 errors",
    )


@buck_test(inplace=False)
async def test_branch_type_matching(buck: Buck) -> None:
    """select() branch values matching the type parameter pass type checking."""
    await buck.starlark("typecheck", "select_branch_good.bzl")


@buck_test(inplace=False)
async def test_branch_type_mismatch(buck: Buck) -> None:
    """select() branch values not matching the type parameter are rejected."""
    await expect_failure(
        buck.starlark("typecheck", "select_branch_bad.bzl"),
        stderr_regex="Detected 2 errors",
    )


@buck_test(inplace=False)
async def test_backward_compat_bare_select(buck: Buck) -> None:
    """Bare Select type accepts any select() regardless of branch value types."""
    await buck.starlark("typecheck", "select_compat_good.bzl")


@buck_test(inplace=False)
async def test_rbin_op_list_plus_select(buck: Buck) -> None:
    """list + Select[T] type-checks via rbin_op dispatch on custom types."""
    await buck.starlark("typecheck", "select_rbin_op_good.bzl")


@buck_test(inplace=False)
async def test_select_or_list_union_accepts_both(buck: Buck) -> None:
    """Select[str] | list[str] accepts both select expressions and plain lists."""
    await buck.starlark("typecheck", "select_union_param_good.bzl")


@buck_test(inplace=False)
async def test_select_or_list_union_rejects_wrong_type(buck: Buck) -> None:
    """Select[str] | list[str] rejects plain int and str values."""
    await expect_failure(
        buck.starlark("typecheck", "select_union_param_bad.bzl"),
        stderr_regex="Detected 2 errors",
    )


@buck_test(inplace=False)
async def test_select_intersect_accepts_matching(buck: Buck) -> None:
    """Select[str] | Select[int] accepts select expressions with str or int branches."""
    await buck.starlark("typecheck", "select_intersect_good.bzl")


@buck_test(inplace=False)
async def test_select_intersect_rejects_non_overlapping(buck: Buck) -> None:
    """Select[str] | Select[int] rejects Select[bool] and plain values."""
    await expect_failure(
        buck.starlark("typecheck", "select_intersect_bad.bzl"),
        stderr_regex="Detected 2 errors",
    )
