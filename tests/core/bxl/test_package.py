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
async def test_get_package_path(buck: Buck) -> None:
    await buck.bxl(
        "//package.bxl:get_package_path",
    )


@buck_test()
async def test_read_package_value(buck: Buck) -> None:
    await buck.bxl("//package.bxl:read_package_value")


@buck_test()
async def test_read_package_value_from_string(buck: Buck) -> None:
    await buck.bxl("//package.bxl:read_package_value_from_string")


@buck_test()
async def test_read_override_package_value(buck: Buck) -> None:
    await buck.bxl("//package.bxl:read_override_package_value")


@buck_test()
async def test_read_package_value_not_found(buck: Buck) -> None:
    await buck.bxl("//package.bxl:read_package_value_not_found")
