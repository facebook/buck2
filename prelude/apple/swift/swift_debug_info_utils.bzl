# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

load(
    "@prelude//:artifact_tset.bzl",
    "ArtifactTSet",
    "make_artifact_tset",
)
load(
    ":swift_toolchain_types.bzl",
    "WrappedSdkCompiledModuleInfo",
)

def extract_and_merge_debug_artifacts_tsets(ctx: AnalysisContext, compiled_pcm_deps_providers, artifacts: list[Artifact] = []) -> ArtifactTSet:
    swift_debug_tsets = [
        d[WrappedSdkCompiledModuleInfo].debug_info
        for d in compiled_pcm_deps_providers
        if WrappedSdkCompiledModuleInfo in d and d[WrappedSdkCompiledModuleInfo].debug_info != None
    ]

    return make_artifact_tset(
        actions = ctx.actions,
        label = ctx.label,
        artifacts = artifacts,
        children = swift_debug_tsets,
    )
