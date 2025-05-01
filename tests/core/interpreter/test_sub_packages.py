# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

# pyre-strict


from buck2.tests.e2e_util.api.buck import Buck
from buck2.tests.e2e_util.buck_workspace import buck_test


@buck_test()
async def test_sub_packages(buck: Buck) -> None:
    res = await buck.targets("root//:")
    assert 'Pkgs: ["cat/x", "dog"]' in res.stderr
    res = await buck.targets("root//dog:")
    assert 'Pkgs: ["y"]' in res.stderr
    res = await buck.targets("root//cat/x:")
    assert "Pkgs: []" in res.stderr
