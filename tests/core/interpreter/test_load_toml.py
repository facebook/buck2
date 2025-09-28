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
async def test_load_toml(buck: Buck) -> None:
    result = await buck.targets("root//:")
    assert (
        '{"fruits": [{"name": "apple", "physical": {"color": "red", "shape": "round"}, "varieties": [{"name": "red delicious"}, {"name": "granny smith"}]}, {"name": "banana", "varieties": [{"name": "plantain"}]}]}'
        in result.stderr
    )
