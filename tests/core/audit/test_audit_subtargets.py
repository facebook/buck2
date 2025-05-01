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
async def test_audit_subtargets_basic(buck: Buck) -> None:
    result = await buck.audit("subtargets", "//:no_subtargets")
    assert result.stdout == ""

    result = await buck.audit("subtargets", "//:foo")
    assert [
        "root//:foo[bar] (<unspecified>)",
        "root//:foo[baz] (<unspecified>)",
    ] == result.stdout.splitlines()


@buck_test()
async def test_audit_subtargets_of_subtarget(buck: Buck) -> None:
    result = await buck.audit("subtargets", "//:nested[sub1]")
    assert [
        "root//:nested[sub1][sub2] (<unspecified>)",
        "root//:nested[sub1][sub3] (<unspecified>)",
    ] == result.stdout.splitlines()

    result = await buck.audit("subtargets", "//:nested[sub4]")
    assert result.stdout == ""


@buck_test()
async def test_audit_subtargets_shallow(buck: Buck) -> None:
    result = await buck.audit("subtargets", "//:deeply_nested", "--shallow")
    assert [
        "root//:deeply_nested[sub1] (<unspecified>)",
        "root//:deeply_nested[sub2] (<unspecified>)",
    ] == result.stdout.splitlines()

    result = await buck.audit("subtargets", "//:deeply_nested")
    assert [
        "root//:deeply_nested[sub1] (<unspecified>)",
        "root//:deeply_nested[sub2] (<unspecified>)",
        "root//:deeply_nested[sub2][sub3] (<unspecified>)",
        "root//:deeply_nested[sub2][sub3][sub4] (<unspecified>)",
        "root//:deeply_nested[sub2][sub5] (<unspecified>)",
    ] == result.stdout.splitlines()

    result = await buck.audit("subtargets", "//:deeply_nested[sub2]", "--shallow")
    assert [
        "root//:deeply_nested[sub2][sub3] (<unspecified>)",
        "root//:deeply_nested[sub2][sub5] (<unspecified>)",
    ] == result.stdout.splitlines()

    result = await buck.audit("subtargets", "//:deeply_nested[sub2]")
    assert [
        "root//:deeply_nested[sub2][sub3] (<unspecified>)",
        "root//:deeply_nested[sub2][sub3][sub4] (<unspecified>)",
        "root//:deeply_nested[sub2][sub5] (<unspecified>)",
    ] == result.stdout.splitlines()


@buck_test()
async def test_audit_subtargets_json(buck: Buck) -> None:
    result = await buck.audit("subtargets", "//:no_subtargets", "--json")
    golden(output=result.stdout, rel_path="json/golden.has_no_subtargets.json")

    result = await buck.audit("subtargets", "//:foo", "--json")
    golden(output=result.stdout, rel_path="json/golden.basic.json")

    result = await buck.audit("subtargets", "//:nested[sub1]", "--json")
    golden(output=result.stdout, rel_path="json/golden.nested.json")

    result = await buck.audit("subtargets", "//:deeply_nested", "--json")
    golden(output=result.stdout, rel_path="json/golden.deeply_nested_basic.json")

    result = await buck.audit("subtargets", "//:deeply_nested[sub2]", "--json")
    golden(output=result.stdout, rel_path="json/golden.deeply_nested_subs_of_sub.json")
