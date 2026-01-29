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
load(":swift_toolchain_types.bzl", "SwiftCompiledModuleTset")

def write_swift_module_map_with_deps(
        ctx: AnalysisContext,
        module_name: str,
        all_deps: SwiftCompiledModuleTset) -> ArgLike:
    uses_experimental_content_based_path_hashing = get_uses_content_based_paths(ctx)
    return ctx.actions.write_json(
        module_name + ".swift_module_map.json",
        all_deps.project_as_json("swift_module_map"),
        pretty = True,
        with_inputs = True,
        uses_experimental_content_based_path_hashing = uses_experimental_content_based_path_hashing,
    )
