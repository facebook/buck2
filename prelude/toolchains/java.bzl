# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

load("@prelude//java:java_toolchain.bzl", "JavaPlatformInfo", "JavaToolchainInfo", "PrebuiltJarToolchainInfo")

def system_prebuilt_jar_bootstrap_toolchain(name, visibility = None):
    kwargs = {}

    _prebuilt_jar_toolchain_rule(name = name, visibility = visibility, **kwargs)

def _prebuilt_jar_toolchain_rule_impl(_ctx):
    return [
        DefaultInfo(),
        PrebuiltJarToolchainInfo(
            class_abi_generator = None,
            cp_snapshot_generator = None,
            global_code_config = {},
            is_bootstrap_toolchain = True,
        ),
    ]

_prebuilt_jar_toolchain_rule = rule(
    attrs = {},
    impl = _prebuilt_jar_toolchain_rule_impl,
    is_toolchain_rule = True,
)

def system_java_bootstrap_toolchain(
        name,
        visibility = None):
    kwargs = {}

    _java_toolchain(
        name = name,
        visibility = visibility,
        **kwargs
    )

def _java_toolchain_impl(ctx):
    return [
        DefaultInfo(),
        JavaPlatformInfo(
            name = ctx.attrs.name,
        ),
        JavaToolchainInfo(
            compile_and_package = ctx.attrs.compile_and_package,
            gen_class_to_source_map = ctx.attrs.gen_class_to_source_map,
            gen_class_to_source_map_include_sourceless_compiled_packages = ctx.attrs.gen_class_to_source_map_include_sourceless_compiled_packages,
            gen_class_to_source_map_debuginfo = None,
            fat_jar = None,
            jar = None,
            java = None,
            jar_builder = RunInfo(["java", "-jar", ctx.attrs.jar_builder]),
            zip_scrubber = RunInfo(["java", "-jar", ctx.attrs.zip_scrubber]),
            track_class_usage = False,
            is_bootstrap_toolchain = True,
            class_abi_generator = None,
            cp_snapshot_generator = None,
            fat_jar_main_class_lib = None,
            nullsafe = None,
            nullsafe_extra_args = [],
            nullsafe_signatures = None,
            javac_protocol = "classic",
            dep_files = None,
            javac = "javac",
            global_code_config = {},
        ),
    ]

_java_toolchain = rule(
    impl = _java_toolchain_impl,
    is_toolchain_rule = True,
    attrs = {
        "class_abi_generator": attrs.option(attrs.dep(providers = [RunInfo]), default = None),
        "compile_and_package": attrs.dep(default = "prelude//java/tools:compile_and_package"),
        "gen_class_to_source_map": attrs.exec_dep(
            default = "prelude//java/tools:gen_class_to_source_map",
            providers = [RunInfo],
        ),
        "gen_class_to_source_map_include_sourceless_compiled_packages": attrs.list(attrs.string(), default = [
            "androidx.databinding",
        ]),
        "is_bootstrap_toolchain": attrs.bool(default = False),
        "jar_builder": attrs.source(default = "prelude//toolchains/android/src/com/facebook/buck/util/zip:jar_builder"),
        "zip_scrubber": attrs.source(default = "prelude//toolchains/android/src/com/facebook/buck/util/zip:zip_scrubber"),
    },
)
