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
from buck2.tests.e2e_util.helper.golden import golden

# Turns `//lib/greeting` into `//lib/greeting:greeting` during coercion in
# build/bzl files. Off by default.
_ENABLE = "buck2.infer_target_names=true"


@buck_test()
async def test_eponymous_dep_in_build_file_rejected_by_default(buck: Buck) -> None:
    # `//:consumer` depends on the abbreviated `//lib/greeting`. Without the
    # buckconfig this is a coercion error, matching historical behavior.
    res = await expect_failure(
        buck.uquery("deps(root//:consumer)", "--console=none", "-v0"),
    )
    golden(
        output=res.stderr,
        rel_path="golden/eponymous_dep_rejected.golden.stderr",
    )


@buck_test()
async def test_eponymous_dep_in_build_file_inferred_when_enabled(buck: Buck) -> None:
    # With the buckconfig, `//lib/greeting` is inferred to be
    # `//lib/greeting:greeting`, which is a real target, so the query succeeds.
    res = await buck.uquery("deps(root//:consumer)", "-c", _ENABLE)
    assert "root//lib/greeting:greeting" in res.stdout


@buck_test()
async def test_eponymous_attr_default_in_bzl_rejected_by_default(buck: Buck) -> None:
    # The `//bzl_default` rule declares `attrs.dep(default = "//lib/greeting")`.
    # The default is coerced while evaluating the .bzl module, so it errors
    # without the buckconfig.
    res = await expect_failure(
        buck.uquery("root//bzl_default:", "--console=none", "-v0"),
    )
    golden(
        output=res.stderr,
        rel_path="golden/eponymous_attr_default_rejected.golden.stderr",
    )


@buck_test()
async def test_eponymous_attr_default_in_bzl_inferred_when_enabled(
    buck: Buck,
) -> None:
    # With the buckconfig, the eponymous attr default resolves and shows up as a
    # dependency of the target.
    res = await buck.uquery("deps(root//bzl_default:target)", "-c", _ENABLE)
    assert "root//lib/greeting:greeting" in res.stdout
