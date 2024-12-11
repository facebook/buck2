# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

# pyre-strict


from buck2.tests.e2e.fdb.types import ExecInfo

from buck2.tests.e2e_util.api.buck import Buck
from buck2.tests.e2e_util.buck_workspace import buck_test


@buck_test(inplace=True, skip_for_os=["windows"])
async def test_java_test(buck: Buck) -> None:
    root = (await buck.root("--kind", "project")).stdout.strip("\n")
    result = await buck.bxl(
        "prelude//debugging/fdb.bxl:inspect_target",
        "--",
        "--target",
        "//buck2/tests/targets/rules/java/java_test:simple_junit_test_java11",
    )

    exec_info = ExecInfo.from_buck_result(result)
    classmap = exec_info.read_class_map(root)
    names = [class_ref.name for entry in classmap for class_ref in entry.classes]
    assert names == ["com.example.SimpleJUnitTest"]


@buck_test(inplace=True, skip_for_os=["windows"])
async def test_java_binary(buck: Buck) -> None:
    root = (await buck.root("--kind", "project")).stdout.strip("\n")
    result = await buck.bxl(
        "prelude//debugging/fdb.bxl:inspect_target",
        "--",
        "--target",
        "//buck2/tests/targets/rules/java/good/java_binary_with_native_libs:binary_with_native_lib",
    )
    exec_info: ExecInfo = ExecInfo.from_buck_result(result)
    classmap = exec_info.read_class_map(root)
    names = [class_ref.name for entry in classmap for class_ref in entry.classes]
    assert names == ["JavaBinaryWithNativeLibs"]


@buck_test(inplace=True, skip_for_os=["windows"])
async def test_java_library(buck: Buck) -> None:
    root = (await buck.root("--kind", "project")).stdout.strip("\n")
    result = await buck.bxl(
        "prelude//debugging/fdb.bxl:inspect_target",
        "--",
        "--target",
        "//buck2/tests/targets/rules/java/good/java_binary_with_native_libs:lib",
    )
    exec_info: ExecInfo = ExecInfo.from_buck_result(result)
    classmap = exec_info.read_class_map(root)
    names = [class_ref.name for entry in classmap for class_ref in entry.classes]
    assert names == ["JavaBinaryWithNativeLibs"]


@buck_test(inplace=True, skip_for_os=["windows"])
async def test_kotlin_test(buck: Buck) -> None:
    root = (await buck.root("--kind", "project")).stdout.strip("\n")
    result = await buck.bxl(
        "prelude//debugging/fdb.bxl:inspect_target",
        "--",
        "--target",
        "//buck2/tests/targets/rules/kotlin/kotlin_test:simple_kotlin_test",
    )
    exec_info: ExecInfo = ExecInfo.from_buck_result(result)
    classmap = exec_info.read_class_map(root)
    names = [class_ref.name for entry in classmap for class_ref in entry.classes]
    assert names == ["com.example.SimpleKotlinTest"]


@buck_test(inplace=True, skip_for_os=["windows"])
async def test_kotlin_library(buck: Buck) -> None:
    root = (await buck.root("--kind", "project")).stdout.strip("\n")
    result = await buck.bxl(
        "prelude//debugging/fdb.bxl:inspect_target",
        "--",
        "--target",
        "//buck2/tests/targets/rules/kotlin/kotlin_library:lib_with_source_only_abi_generation",
    )
    exec_info: ExecInfo = ExecInfo.from_buck_result(result)
    classmap = exec_info.read_class_map(root)
    names = [class_ref.name for entry in classmap for class_ref in entry.classes]
    assert names == ["A", "B"]


@buck_test(inplace=True, skip_for_os=["windows"])
async def test_apk_gen_rule(buck: Buck) -> None:
    root = (await buck.root("--kind", "project")).stdout.strip("\n")
    result = await buck.bxl(
        "prelude//debugging/fdb.bxl:inspect_target",
        "--",
        "--target",
        "fbsource//fbandroid/buck2/tests/good/apk:zip_align_basic_apk",
    )
    exec_info: ExecInfo = ExecInfo.from_buck_result(result)
    classmap = exec_info.read_class_map(root)
    names = [class_ref.name for entry in classmap for class_ref in entry.classes]
    assert names == [
        "com.example.sampleapp.MainActivity",
        "com.example.sampleapp.Helper",
        "com.example.sampleapp.Helper$SomeInterface",
    ]


# This is to ensure at least one of the tests is passing on Windows otherwise CI fails
@buck_test(inplace=True)
async def test_noop(buck: Buck) -> None:
    return
