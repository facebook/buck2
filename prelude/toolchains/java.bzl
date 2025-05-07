# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

load("@prelude//java:java_toolchain.bzl", "AbiGenerationMode", "JavaPlatformInfo", "JavaToolchainInfo", "JavacProtocol", "PrebuiltJarToolchainInfo")

def _system_java_tool_impl(ctx):
    return [
        DefaultInfo(),
        RunInfo([ctx.attrs.tool_name]),
    ]

system_java_tool = rule(
    impl = _system_java_tool_impl,
    attrs = {
        "tool_name": attrs.string(),
    },
)

def system_prebuilt_jar_bootstrap_toolchain(
        name,
        java,
        visibility = None):
    kwargs = {}

    _prebuilt_jar_toolchain_rule(name = name, java = java, visibility = visibility, **kwargs)

def _prebuilt_jar_toolchain_rule_impl(ctx):
    return [
        DefaultInfo(),
        PrebuiltJarToolchainInfo(
            class_abi_generator = None,
            cp_snapshot_generator = None,
            global_code_config = {},
            is_bootstrap_toolchain = True,
            java = ctx.attrs.java,
        ),
    ]

_prebuilt_jar_toolchain_rule = rule(
    attrs = {
        "java": attrs.dep(),
    },
    impl = _prebuilt_jar_toolchain_rule_impl,
    is_toolchain_rule = True,
)

def javacd_toolchain(
        name,
        java,
        visibility = None):
    _java_toolchain(
        name = name,
        visibility = visibility,
        java = java,
        is_bootstrap_toolchain = False,
        class_abi_generator = "prelude//toolchains/android/src/com/facebook/buck/jvm/java/abi:api-stubber",
        class_loader_bootstrapper = "prelude//toolchains/android/src/com/facebook/buck/cli/bootstrapper:bootstrapper",
        fat_jar_main_class_lib = "prelude//toolchains/android/src/com/facebook/buck/jvm/java/fatjar:fat-jar-main-binary",
        javac = "prelude//toolchains/android/src/com/facebook/buck/jvm/java/stepsbuilder/javacd/main:javacd_tool",
        javac_protocol = "javacd",
        javacd_main_class = "com.facebook.buck.jvm.java.stepsbuilder.javacd.main.JavaCDMain",
    )

def system_java_bootstrap_toolchain(
        name,
        java,
        visibility = None):
    _java_toolchain(
        name = name,
        visibility = visibility,
        java = java,
        is_bootstrap_toolchain = True,
        javac = "javac",
        javac_protocol = "classic",
    )

def _java_toolchain_impl(ctx):
    return [
        DefaultInfo(),
        JavaPlatformInfo(
            name = ctx.attrs.name,
        ),
        JavaToolchainInfo(
            # TODO(navidq) make this configurable via buck config
            abi_generation_mode = AbiGenerationMode("none"),
            compile_and_package = ctx.attrs.compile_and_package,
            class_abi_generator = ctx.attrs.class_abi_generator,
            class_loader_bootstrapper = ctx.attrs.class_loader_bootstrapper,
            cp_snapshot_generator = None,
            dep_files = None,
            fat_jar_main_class_lib = ctx.attrs.fat_jar_main_class_lib,
            gen_class_to_source_map = ctx.attrs.gen_class_to_source_map,
            gen_class_to_source_map_include_sourceless_compiled_packages = ctx.attrs.gen_class_to_source_map_include_sourceless_compiled_packages,
            gen_class_to_source_map_debuginfo = None,
            fat_jar = ctx.attrs.fat_jar,
            is_bootstrap_toolchain = ctx.attrs.is_bootstrap_toolchain,
            jar = None,
            java = ctx.attrs.java,
            javac = ctx.attrs.javac,
            javac_protocol = ctx.attrs.javac_protocol,
            javacd_jvm_args = [],
            javacd_jvm_args_target = [],
            javacd_main_class = ctx.attrs.javacd_main_class,
            jar_builder = RunInfo(cmd_args([ctx.attrs.java[RunInfo], "-jar", ctx.attrs.jar_builder])),
            src_root_elements = [],
            src_root_prefixes = [],
            track_class_usage = False,
            zip_scrubber = RunInfo(cmd_args([ctx.attrs.java[RunInfo], "-jar", ctx.attrs.zip_scrubber])),
            nullsafe = None,
            nullsafe_extra_args = [],
            nullsafe_signatures = None,
            global_code_config = {},
        ),
    ]

_java_toolchain = rule(
    impl = _java_toolchain_impl,
    is_toolchain_rule = True,
    attrs = {
        "class_abi_generator": attrs.option(attrs.dep(providers = [RunInfo]), default = None),
        "class_loader_bootstrapper": attrs.option(attrs.source(), default = None),
        "compile_and_package": attrs.dep(default = "prelude//java/tools:compile_and_package"),
        "fat_jar": attrs.dep(default = "prelude//java/tools:fat_jar"),
        "fat_jar_main_class_lib": attrs.option(attrs.source(), default = None),
        "gen_class_to_source_map": attrs.exec_dep(
            default = "prelude//java/tools:gen_class_to_source_map",
            providers = [RunInfo],
        ),
        "gen_class_to_source_map_include_sourceless_compiled_packages": attrs.list(attrs.string(), default = [
            "androidx.databinding",
        ]),
        "is_bootstrap_toolchain": attrs.bool(default = False),
        "jar_builder": attrs.source(default = "prelude//toolchains/android/src/com/facebook/buck/util/zip:jar_builder"),
        "java": attrs.dep(),
        "javac": attrs.option(attrs.one_of(attrs.dep(), attrs.source(), attrs.string()), default = None),
        "javac_protocol": attrs.enum(JavacProtocol.values()),
        "javacd_main_class": attrs.option(attrs.string(), default = None),
        "zip_scrubber": attrs.source(default = "prelude//toolchains/android/src/com/facebook/buck/util/zip:zip_scrubber"),
    },
)
