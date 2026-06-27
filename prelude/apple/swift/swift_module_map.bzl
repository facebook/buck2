# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

load("@prelude//utils:arglike.bzl", "ArgLike")  # @unused Used as a type
load(
    ":swift_incremental_support.bzl",
    "get_uses_content_based_paths",
)
load(":swift_toolchain_types.bzl", "SwiftCompiledModuleTset", "get_swift_module_map_deduped_entries")

def write_swift_module_map_with_deps(ctx: AnalysisContext, module_name: str, all_deps: SwiftCompiledModuleTset) -> (Artifact, ArgLike):
    uses_content_based_paths = get_uses_content_based_paths(ctx)
    artifact = ctx.actions.declare_output(
        module_name + ".swift_module_map.json",
        has_content_based_path = uses_content_based_paths,
    )
    args = ctx.actions.write_json(
        artifact.as_output(),
        get_swift_module_map_deduped_entries(all_deps),
        pretty = True,
        with_inputs = True,
    )
    return (artifact, args)
