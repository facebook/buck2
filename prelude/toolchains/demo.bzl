# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

load("@prelude//tests:test_toolchain.bzl", "noop_test_toolchain")
load("@prelude//toolchains:android.bzl", "system_android_toolchain")
load("@prelude//toolchains:cxx.bzl", "system_cxx_toolchain")
load("@prelude//toolchains:dex.bzl", "system_noop_dex_toolchain")
load("@prelude//toolchains:genrule.bzl", "system_genrule_toolchain")
load("@prelude//toolchains:go.bzl", "system_go_bootstrap_toolchain", "system_go_toolchain")
load("@prelude//toolchains:haskell.bzl", "system_haskell_toolchain")
load("@prelude//toolchains:java.bzl", "javacd_toolchain", "system_java_bootstrap_toolchain", "system_java_tool", "system_prebuilt_jar_bootstrap_toolchain")
load("@prelude//toolchains:kotlin.bzl", "kotlincd_toolchain", "system_kotlin_bootstrap_toolchain")
load("@prelude//toolchains:ocaml.bzl", "system_ocaml_toolchain")
load("@prelude//toolchains:python.bzl", "system_python_bootstrap_toolchain", "system_python_toolchain")
load("@prelude//toolchains:remote_test_execution.bzl", "remote_test_execution_toolchain")
load("@prelude//toolchains:rust.bzl", "system_rust_toolchain")
load("@prelude//toolchains:zip_file.bzl", "zip_file_toolchain")

def system_demo_toolchains():
    """
    All the default toolchains, suitable for a quick demo or early prototyping.
    Most real projects should copy/paste the implementation to configure them.
    """
    system_android_toolchain(
        name = "android",
        visibility = ["PUBLIC"],
    )

    system_cxx_toolchain(
        name = "cxx",
        visibility = ["PUBLIC"],
    )

    # TODO(ianc) Make this toolchain actually do something
    system_noop_dex_toolchain(
        name = "dex",
        visibility = ["PUBLIC"],
    )

    system_noop_dex_toolchain(
        name = "empty_dex",
        visibility = ["PUBLIC"],
    )

    system_genrule_toolchain(
        name = "genrule",
        visibility = ["PUBLIC"],
    )

    system_go_toolchain(
        name = "go",
        visibility = ["PUBLIC"],
    )

    system_go_bootstrap_toolchain(
        name = "go_bootstrap",
        visibility = ["PUBLIC"],
    )

    system_haskell_toolchain(
        name = "haskell",
        visibility = ["PUBLIC"],
    )

    javacd_toolchain(
        name = "java",
        java = ":java_tool",
        visibility = ["PUBLIC"],
    )

    system_java_bootstrap_toolchain(
        name = "java_bootstrap",
        java = ":java_tool",
        visibility = ["PUBLIC"],
    )

    system_java_bootstrap_toolchain(
        name = "java_graalvm_bootstrap",
        java = ":java_tool",
        visibility = ["PUBLIC"],
    )

    javacd_toolchain(
        name = "java_for_android",
        java = ":java_tool",
        visibility = ["PUBLIC"],
    )

    system_java_tool(
        name = "java_tool",
        tool_name = read_root_config("java", "java_binary", "/usr/local/bin/java"),
        visibility = ["PUBLIC"],
    )

    kotlincd_toolchain(
        name = "kotlin",
        visibility = ["PUBLIC"],
    )

    system_kotlin_bootstrap_toolchain(
        name = "kotlin_bootstrap",
        visibility = ["PUBLIC"],
    )

    kotlincd_toolchain(
        name = "kotlin_for_android",
        visibility = ["PUBLIC"],
    )

    system_ocaml_toolchain(
        name = "ocaml",
        visibility = ["PUBLIC"],
    )

    # TODO(ianc) Make this not a bootstrap toolchain
    system_prebuilt_jar_bootstrap_toolchain(
        name = "prebuilt_jar",
        java = ":java_tool",
        visibility = ["PUBLIC"],
    )

    system_prebuilt_jar_bootstrap_toolchain(
        name = "prebuilt_jar_bootstrap",
        java = ":java_tool",
        visibility = ["PUBLIC"],
    )

    system_prebuilt_jar_bootstrap_toolchain(
        name = "prebuilt_jar_bootstrap_no_snapshot",
        java = ":java_tool",
        visibility = ["PUBLIC"],
    )

    system_python_toolchain(
        name = "python",
        visibility = ["PUBLIC"],
    )

    system_python_bootstrap_toolchain(
        name = "python_bootstrap",
        visibility = ["PUBLIC"],
    )

    system_rust_toolchain(
        name = "rust",
        default_edition = "2021",
        visibility = ["PUBLIC"],
    )

    remote_test_execution_toolchain(
        name = "remote_test_execution",
        visibility = ["PUBLIC"],
    )

    noop_test_toolchain(
        name = "test",
        visibility = ["PUBLIC"],
    )

    zip_file_toolchain(
        name = "zip_file",
        visibility = ["PUBLIC"],
    )
