# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

# pyre-strict


from buck2.tests.e2e_util.api.buck import Buck
from buck2.tests.e2e_util.buck_workspace import buck_test
from buck2.tests.e2e_util.helper.golden import golden


@buck_test()
async def test_visibility_from_package_simple(buck: Buck) -> None:
    result = await buck.uquery(
        "root//simple:", "--output-attribute=visibility|within_view"
    )
    golden(
        output=result.stdout,
        rel_path="simple/golden.uquery.json",
    )


@buck_test()
async def test_visibility_from_package_inherit(buck: Buck) -> None:
    result = await buck.uquery(
        "root//inherit/...", "--output-attribute=visibility|within_view"
    )
    golden(
        output=result.stdout,
        rel_path="inherit/golden.uquery.json",
    )


@buck_test()
async def test_visibility_from_package_override(buck: Buck) -> None:
    result = await buck.uquery(
        "root//override/...", "--output-attribute=visibility|within_view"
    )
    golden(
        output=result.stdout,
        rel_path="override/golden.uquery.json",
    )


@buck_test()
async def test_visibility_from_package_public(buck: Buck) -> None:
    result = await buck.uquery(
        "root//public/...", "--output-attribute=visibility|within_view"
    )
    golden(
        output=result.stdout,
        rel_path="public/golden.uquery.json",
    )
