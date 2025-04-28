# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

load(
    "@prelude//:artifact_tset.bzl",
    "ArtifactInfoTag",
    "ArtifactTSet",
    "make_artifact_tset",
)
load(":swift_toolchain.bzl", "get_swift_toolchain_info")
load(
    ":swift_toolchain_types.bzl",
    "WrappedSdkCompiledModuleInfo",
)

def extract_and_merge_swift_debug_infos(ctx: AnalysisContext, compiled_pcm_deps_providers, artifacts: list[Artifact] = []) -> ArtifactTSet:
    provide_swift_debug_info = get_swift_toolchain_info(ctx).provide_swift_debug_info
    swift_debug_tsets = [
        d[WrappedSdkCompiledModuleInfo].swift_debug_info
        for d in compiled_pcm_deps_providers
        if WrappedSdkCompiledModuleInfo in d and d[WrappedSdkCompiledModuleInfo].swift_debug_info != None
    ] if provide_swift_debug_info else []

    return make_artifact_tset(
        actions = ctx.actions,
        label = ctx.label,
        artifacts = artifacts if provide_swift_debug_info else [],
        children = swift_debug_tsets,
        tags = [ArtifactInfoTag("swiftmodule")],
    )

def extract_and_merge_clang_debug_infos(ctx: AnalysisContext, compiled_pcm_deps_providers, artifacts: list[Artifact] = []) -> ArtifactTSet:
    clang_debug_tsets = [
        d[WrappedSdkCompiledModuleInfo].clang_debug_info
        for d in compiled_pcm_deps_providers
        if WrappedSdkCompiledModuleInfo in d and d[WrappedSdkCompiledModuleInfo].clang_debug_info != None
    ]

    return make_artifact_tset(
        actions = ctx.actions,
        label = ctx.label,
        artifacts = artifacts,
        children = clang_debug_tsets,
        tags = [ArtifactInfoTag("swift_pcm")],
    )
