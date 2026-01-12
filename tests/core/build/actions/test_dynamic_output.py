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


@buck_test(data_dir="everything")
async def test_dynamic_output(buck: Buck) -> None:
    await buck.build("root//:")


@buck_test(data_dir="everything_new")
async def test_dynamic_output_new(buck: Buck) -> None:
    await buck.build("root//:")


@buck_test(data_dir="empty_dynamic_list")
async def test_empty_dynamic_list(buck: Buck) -> None:
    await buck.build("root//:empty_test")


@buck_test(data_dir="artifact_eq_bug")
async def test_artifact_eq_bug(buck: Buck) -> None:
    await buck.build("root//:bug")
