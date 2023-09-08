# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

load("@prelude//utils:arglike.bzl", "ArgLike")  # @unused Used as a type
load(
    ":swift_toolchain_types.bzl",
    "SwiftCompiledModuleInfo",  # @unused Used as a type
)

def write_swift_module_map(
        ctx: AnalysisContext,
        module_name: str,
        sdk_deps: list[SwiftCompiledModuleInfo]) -> ArgLike:
    return write_swift_module_map_with_swift_deps(ctx, module_name, sdk_deps, [])

def write_swift_module_map_with_swift_deps(
        ctx: AnalysisContext,
        module_name: str,
        sdk_swift_deps: list[SwiftCompiledModuleInfo],
        swift_deps: list[SwiftCompiledModuleInfo]) -> ArgLike:
    deps = {}
    for sdk_dep in sdk_swift_deps + swift_deps:
        if sdk_dep.is_swiftmodule:
            deps[sdk_dep.module_name] = {
                "isFramework": sdk_dep.is_framework,
                "moduleName": sdk_dep.module_name,
                "modulePath": sdk_dep.output_artifact,
            }

    return ctx.actions.write_json(
        module_name + ".swift_module_map.json",
        deps.values(),
        with_inputs = True,
    )
