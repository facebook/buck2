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


@buck_test()
async def test_uquery_allbuildfiles(buck: Buck) -> None:
    await buck.bxl(
        "//:query_buildfiles.bxl:uquery_allbuildfiles",
    )


@buck_test()
async def test_uquery_rbuildfiles(buck: Buck) -> None:
    await buck.bxl(
        "//:query_buildfiles.bxl:uquery_rbuildfiles",
    )


@buck_test()
async def test_cquery_allbuildfiles(buck: Buck) -> None:
    await buck.bxl(
        "//:query_buildfiles.bxl:cquery_allbuildfiles",
    )


@buck_test()
async def test_cquery_rbuildfiles(buck: Buck) -> None:
    await buck.bxl(
        "//:query_buildfiles.bxl:cquery_rbuildfiles",
    )


@buck_test()
async def test_lazy_uquery_allbuildfiles(buck: Buck) -> None:
    await buck.bxl(
        "//:query_buildfiles.bxl:lazy_uquery_allbuildfiles",
    )


@buck_test()
async def test_lazy_uquery_rbuildfiles(buck: Buck) -> None:
    await buck.bxl(
        "//:query_buildfiles.bxl:lazy_uquery_rbuildfiles",
    )
