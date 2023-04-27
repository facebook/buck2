# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

KotlincProtocol = enum("classic", "kotlincd")

KotlinToolchainInfo = provider(
    doc = "Kotlin toolchain info",
    fields = [
        "annotation_processing_jar",
        "compilation_tracer_plugin",
        "compile_kotlin",
        "dep_files",
        "kapt_base64_encoder",
        "kotlinc",
        "kotlinc_classpath",
        "kotlinc_protocol",
        "kotlincd_debug_port",
        "kotlincd_debug_target",
        "kotlincd_jvm_args",
        "kotlin_stdlib",
        "kotlin_home_libraries",
        "kosabi_stubs_gen_plugin",
        "kosabi_applicability_plugin",
        "kosabi_jvm_abi_gen_plugin",
        "jvm_abi_gen_plugin",
        "should_use_compilation_tracer",
    ],
)
