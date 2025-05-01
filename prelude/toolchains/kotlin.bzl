# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

load("@prelude//java:java_toolchain.bzl", "DepFiles")
load("@prelude//kotlin:kotlin_toolchain.bzl", "KotlinToolchainInfo", "KotlincProtocol")

def system_kotlin_bootstrap_toolchain(
        name,
        **kwargs):
    kwargs["annotation_processing_jar"] = "prelude//toolchains/android/third-party:kotlin-annotation-processing-embeddable"
    kwargs["compile_kotlin"] = "prelude//kotlin/tools/compile_kotlin:compile_kotlin"
    kwargs["kapt_base64_encoder"] = "prelude//kotlin/tools/kapt_base64_encoder:kapt_base64_encoder"
    kwargs["kotlin_stdlib"] = "prelude//toolchains/android/third-party:kotlin-stdlib"
    kwargs["kotlin_version"] = "2.0.0"

    kwargs["kotlinc"] = "prelude//toolchains/android/third-party:kotlin-compiler-binary"
    kwargs["kotlinc_protocol"] = "classic"
    kwargs["dep_files"] = None

    _kotlin_toolchain_rule(
        name = name,
        **kwargs
    )

def _kotlin_toolchain_rule_impl(ctx):
    return [
        DefaultInfo(),
        KotlinToolchainInfo(
            allow_k2_usage = ctx.attrs.allow_k2_usage,
            annotation_processing_jar = ctx.attrs.annotation_processing_jar,
            class_loader_bootstrapper = ctx.attrs.class_loader_bootstrapper,
            compile_kotlin = ctx.attrs.compile_kotlin,
            dep_files = DepFiles(ctx.attrs.dep_files),
            kapt_base64_encoder = ctx.attrs.kapt_base64_encoder,
            kotlinc = ctx.attrs.kotlinc,
            kotlin_stdlib = ctx.attrs.kotlin_stdlib,
            kotlin_version = ctx.attrs.kotlin_version,
            kotlin_home_libraries = ctx.attrs.kotlin_home_libraries,
            enable_incremental_compilation = ctx.attrs.enable_incremental_compilation,
            kotlinc_protocol = ctx.attrs.kotlinc_protocol,
            kotlinc_run_via_build_tools_api = ctx.attrs.kotlinc_run_via_build_tools_api,
            kosabi_stubs_gen_plugin = ctx.attrs.kosabi_stubs_gen_plugin,
            kosabi_standalone = ctx.attrs.kosabi_standalone,
            kosabi_source_modifier_plugin = ctx.attrs.kosabi_source_modifier_plugin,
            kosabi_applicability_plugin = ctx.attrs.kosabi_applicability_plugin,
            kosabi_jvm_abi_gen_plugin = ctx.attrs.kosabi_jvm_abi_gen_plugin,
            jvm_abi_gen_plugin = ctx.attrs.jvm_abi_gen_plugin,
            kotlincd_debug_port = ctx.attrs.kotlincd_debug_port,
            kotlincd_debug_target = ctx.attrs.kotlincd_debug_target,
            kotlincd_jvm_args = ctx.attrs.kotlincd_jvm_args,
            kotlincd_jvm_args_target = ctx.attrs.kotlincd_jvm_args_target,
            kotlincd_main_class = ctx.attrs.kotlincd_main_class,
            kotlincd_worker = ctx.attrs.kotlincd_worker,
            track_class_usage_plugin = ctx.attrs.track_class_usage_plugin,
            kotlin_error_handler = None,
        ),
    ]

_kotlin_toolchain_rule = rule(
    attrs = {
        "allow_k2_usage": attrs.option(attrs.bool(), default = None),
        "annotation_processing_jar": attrs.dep(),
        "class_loader_bootstrapper": attrs.option(attrs.source(), default = None),
        "compile_kotlin": attrs.dep(providers = [RunInfo]),
        "dep_files": attrs.enum(["none", "per_class", "per_jar"], default = "none"),
        "enable_incremental_compilation": attrs.option(attrs.bool(), default = None),
        "jvm_abi_gen_plugin": attrs.option(attrs.source(), default = None),
        "kapt_base64_encoder": attrs.dep(providers = [RunInfo]),
        "kosabi_applicability_plugin": attrs.option(attrs.source(), default = None),
        "kosabi_jvm_abi_gen_plugin": attrs.option(attrs.source(), default = None),
        "kosabi_source_modifier_plugin": attrs.option(attrs.source(), default = None),
        "kosabi_standalone": attrs.option(attrs.bool(), default = None),
        "kosabi_stubs_gen_plugin": attrs.option(attrs.source(), default = None),
        "kotlin_home_libraries": attrs.list(attrs.source(), default = []),
        "kotlin_stdlib": attrs.dep(),
        "kotlin_version": attrs.string(),
        "kotlinc": attrs.dep(providers = [RunInfo]),
        "kotlinc_protocol": attrs.enum(KotlincProtocol.values(), default = "classic"),
        "kotlinc_run_via_build_tools_api": attrs.option(attrs.bool(), default = None),
        "kotlincd_debug_port": attrs.option(attrs.int(), default = None),
        "kotlincd_debug_target": attrs.option(attrs.label(), default = None),
        "kotlincd_jvm_args": attrs.list(attrs.string(), default = []),
        "kotlincd_jvm_args_target": attrs.list(attrs.label(), default = []),
        "kotlincd_main_class": attrs.option(attrs.string(), default = None),
        "kotlincd_worker": attrs.option(attrs.dep(), default = None),
        "track_class_usage_plugin": attrs.option(attrs.source(), default = None),
    },
    impl = _kotlin_toolchain_rule_impl,
    is_toolchain_rule = True,
)
