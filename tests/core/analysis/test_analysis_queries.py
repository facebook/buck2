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


async def _test_analysis_query_invalidation_impl(buck: Buck, name: str) -> None:
    linux = await buck.build_without_report(
        ":root", "-c", "test.configuration=linux", "--out=-"
    )
    macos = await buck.build_without_report(
        ":root", "-c", "test.configuration=macos", "--out=-"
    )

    golden(
        output=linux.stdout,
        rel_path=f"{name}/linux.txt.golden",
    )
    golden(
        output=macos.stdout,
        rel_path=f"{name}/macos.txt.golden",
    )

    # Mostly here to really be safe but in practice this fails with an
    # incompatible target earlier if we have a bug.
    assert "linux-select-dep" in linux.stdout
    assert "macos-select-dep" in macos.stdout


@buck_test(data_dir="analysis_query_invalidation")
async def test_analysis_query_invalidation_deps(buck: Buck) -> None:
    """
    This is a regression test for T133069783.
    """
    await _test_analysis_query_invalidation_impl(
        buck, name="analysis_query_invalidation"
    )


@buck_test(
    data_dir="analysis_query_invalidation_classpath",
)
async def test_analysis_query_invalidation_classpath(buck: Buck) -> None:
    """
    Equivalent of T133069783 for `classpath()` instead of `deps()` queries.
    """
    await _test_analysis_query_invalidation_impl(
        buck, name="analysis_query_invalidation_classpath"
    )


@buck_test(data_dir="analysis_query_deps")
async def test_analysis_query_deps(buck: Buck) -> None:
    deps = await buck.build_without_report(":deps", "--out=-")
    golden(
        output=deps.stdout,
        rel_path="analysis_query_deps/deps.txt.golden",
    )
    assert ":foo" in deps.stdout
    assert ":bar" in deps.stdout
    assert ":baz" in deps.stdout
    assert ":qux" in deps.stdout


@buck_test(data_dir="analysis_query_deps")
async def test_analysis_query_deps_with_depth(buck: Buck) -> None:
    deps = await buck.build_without_report(":deps1", "--out=-")
    golden(output=deps.stdout, rel_path="analysis_query_deps/deps1.txt.golden")
    assert ":foo" in deps.stdout
    assert ":bar" in deps.stdout
    assert ":baz" in deps.stdout
    assert ":qux" not in deps.stdout


@buck_test(setup_eden=True, data_dir="analysis_query_deps")
async def test_analysis_query_target_deps(buck: Buck) -> None:
    deps = await buck.build_without_report(":target_deps", "--out=-")
    golden(
        output=deps.stdout,
        rel_path="analysis_query_deps/target_deps.txt.golden",
    )
    assert ":foo" in deps.stdout
    assert ":bar" in deps.stdout
    assert ":baz" not in deps.stdout
    assert ":qux" not in deps.stdout
