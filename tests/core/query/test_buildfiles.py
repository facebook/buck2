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
async def test_allbuildfiles(buck: Buck) -> None:
    target1 = "root//load:abc"
    target2 = "root//transitive_load:def"
    target3 = "root//transitive_load:ghi"
    out1 = (await buck.uquery(f"allbuildfiles({target1})")).stdout
    out2 = (await buck.uquery(f"allbuildfiles({target2})")).stdout
    out3 = (await buck.uquery(f"allbuildfiles({target3})")).stdout
    out4 = (await buck.uquery(f"allbuildfiles(set({target1} {target2}))")).stdout

    # First, check that these are the same for cquery
    assert out1 == (await buck.cquery(f"allbuildfiles({target1})")).stdout
    assert out2 == (await buck.cquery(f"allbuildfiles({target2})")).stdout
    assert out3 == (await buck.cquery(f"allbuildfiles({target3})")).stdout
    assert (
        out4 == (await buck.cquery(f"allbuildfiles(set({target1} {target2}))")).stdout
    )

    out1 = [x for x in out1.splitlines() if not x.startswith("nano_prelude/")]
    out1.sort()
    out2 = [x for x in out2.splitlines() if not x.startswith("nano_prelude/")]
    out2.sort()
    out3 = [x for x in out3.splitlines() if not x.startswith("nano_prelude/")]
    out3.sort()
    out4 = [x for x in out4.splitlines() if not x.startswith("nano_prelude/")]
    out4.sort()

    # verify loads
    expected1 = ["load/TARGETS.fixture", "load/a.bzl"]
    assert out1 == expected1

    # verify transitive loads
    expected2 = [
        "transitive_load/TARGETS.fixture",
        "transitive_load/b.bzl",
        "transitive_load/c.bzl",
    ]
    assert out2 == expected2
    assert out3 == expected2

    # correctly handle multiple inputs
    expected4 = expected1 + expected2
    expected4.sort()
    assert out4 == expected4


@buck_test()
async def test_rbuildfiles(buck: Buck) -> None:
    target_file = "transitive_load/TARGETS.fixture"
    out1 = (
        await buck.uquery(f"rbuildfiles({target_file}, transitive_load/c.bzl)")
    ).stdout
    out2 = (await buck.uquery(f"rbuildfiles({target_file}, {target_file})")).stdout

    # Check that these are the same for cquery
    assert (
        out1
        == (
            await buck.cquery(f"rbuildfiles({target_file}, transitive_load/c.bzl)")
        ).stdout
    )
    assert (
        out2 == (await buck.cquery(f"rbuildfiles({target_file}, {target_file})")).stdout
    )

    assert "transitive_load/b.bzl" in out1
    assert "transitive_load/c.bzl" in out1
    assert "transitive_load/TARGETS" in out1

    assert out2 == target_file + "\n"
