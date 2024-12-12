# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

load("@prelude//utils:arglike.bzl", "ArgLike")  # @unused Used as a type
load(":swift_toolchain_types.bzl", "SwiftCompiledModuleTset")

def write_swift_module_map_with_deps(
        ctx: AnalysisContext,
        module_name: str,
        all_deps: SwiftCompiledModuleTset) -> ArgLike:
    return ctx.actions.write_json(
        module_name + ".swift_module_map.json",
        all_deps.project_as_json("swift_module_map"),
        with_inputs = True,
    )
