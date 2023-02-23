# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

load(":swift_toolchain_types.bzl", "WrappedSdkCompiledModuleInfo")

def project_as_hidden(module_info: "SdkCompiledModuleInfo"):
    # NOTE(cjhopman): This would probably be better done by projecting as normal args and the caller putting it in hidden.
    args = cmd_args()
    args.hidden(module_info.output_artifact)
    return args

def project_as_clang_deps(module_info: "SdkCompiledModuleInfo"):
    if module_info.is_swiftmodule:
        return []
    else:
        return [
            "-Xcc",
            cmd_args(["-fmodule-file=", module_info.module_name, "=", module_info.output_artifact], delimiter = ""),
            "-Xcc",
            cmd_args(["-fmodule-map-file=", module_info.input_relative_path], delimiter = ""),
        ]

SDKDepTSet = transitive_set(args_projections = {
    "clang_deps": project_as_clang_deps,
    "hidden": project_as_hidden,
})

def is_sdk_modules_provided(toolchain: "SwiftToolchainInfo") -> bool.type:
    no_swift_modules = toolchain.uncompiled_swift_sdk_modules_deps == None or len(toolchain.uncompiled_swift_sdk_modules_deps) == 0
    no_clang_modules = toolchain.uncompiled_clang_sdk_modules_deps == None or len(toolchain.uncompiled_clang_sdk_modules_deps) == 0
    if no_swift_modules and no_clang_modules:
        return False
    return True

def get_compiled_sdk_deps_tset(ctx: "context", deps_providers: list.type) -> "SDKDepTSet":
    sdk_deps = [
        deps_provider[WrappedSdkCompiledModuleInfo].tset
        for deps_provider in deps_providers
        if WrappedSdkCompiledModuleInfo in deps_provider
    ]
    return ctx.actions.tset(SDKDepTSet, children = sdk_deps)

def get_uncompiled_sdk_deps(
        sdk_modules: [str.type],
        required_modules: [str.type],
        toolchain: "SwiftToolchainInfo") -> ["dependency"]:
    if not is_sdk_modules_provided(toolchain):
        fail("SDK deps are not set for swift_toolchain")

    all_sdk_modules = sdk_modules + required_modules
    all_sdk_modules = dedupe(all_sdk_modules)

    sdk_deps = []

    for sdk_module_dep_name in all_sdk_modules:
        if sdk_module_dep_name in toolchain.uncompiled_swift_sdk_modules_deps:
            sdk_deps.append(toolchain.uncompiled_swift_sdk_modules_deps[sdk_module_dep_name])
        elif sdk_module_dep_name in toolchain.uncompiled_clang_sdk_modules_deps:
            sdk_deps.append(toolchain.uncompiled_clang_sdk_modules_deps[sdk_module_dep_name])

    return sdk_deps
