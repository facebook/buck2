# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

load(":swift_toolchain_types.bzl", "SdkUncompiledModuleInfo", "SwiftObjectFormat", "SwiftToolchainInfo")

def get_swift_toolchain_info(ctx: AnalysisContext) -> SwiftToolchainInfo:
    if hasattr(ctx.attrs, "_apple_toolchain"):
        return ctx.attrs._apple_toolchain[SwiftToolchainInfo]
    else:
        return ctx.attrs._swift_toolchain[SwiftToolchainInfo]

def get_swift_toolchain_info_dep(ctx: AnalysisContext) -> Dependency:
    if hasattr(ctx.attrs, "_apple_toolchain"):
        return ctx.attrs._apple_toolchain
    else:
        return ctx.attrs._swift_toolchain

def _traverse_sdk_modules_graph(
        swift_sdk_module_name_to_deps: dict[str, Dependency],
        clang_sdk_module_name_to_deps: dict[str, Dependency],
        sdk_module_dep: Dependency):
    if SdkUncompiledModuleInfo not in sdk_module_dep:
        return

    uncompiled_sdk_module_info = sdk_module_dep[SdkUncompiledModuleInfo]

    # If input_relative_path is None then this module represents a root node of SDK modules graph.
    # In such case, we need to handle only its deps.
    if uncompiled_sdk_module_info.input_relative_path == None:
        for uncompiled_dep in uncompiled_sdk_module_info.deps:
            _traverse_sdk_modules_graph(swift_sdk_module_name_to_deps, clang_sdk_module_name_to_deps, uncompiled_dep)
        return

    # return if dep is already in dict
    if uncompiled_sdk_module_info.is_swiftmodule and uncompiled_sdk_module_info.module_name in swift_sdk_module_name_to_deps:
        return
    elif not uncompiled_sdk_module_info.is_swiftmodule and uncompiled_sdk_module_info.module_name in clang_sdk_module_name_to_deps:
        return

    for uncompiled_dep in uncompiled_sdk_module_info.deps + uncompiled_sdk_module_info.cxx_deps:
        _traverse_sdk_modules_graph(swift_sdk_module_name_to_deps, clang_sdk_module_name_to_deps, uncompiled_dep)

    if uncompiled_sdk_module_info.is_swiftmodule:
        swift_sdk_module_name_to_deps[uncompiled_sdk_module_info.module_name] = sdk_module_dep
    else:
        clang_sdk_module_name_to_deps[uncompiled_sdk_module_info.module_name] = sdk_module_dep

def compute_sdk_module_graph(sdk_module_deps: list[Dependency]) -> (dict[str, Dependency], dict[str, Dependency]):
    # All Clang's PCMs need to be compiled with cxx flags of the target that imports them,
    # because of that, we expose `dependency`s of SDK modules,
    # which might be accessed from apple_library/apple_test rules and compiled there.
    uncompiled_swift_sdk_modules_deps = {}
    uncompiled_clang_sdk_modules_deps = {}

    for sdk_module_dep in sdk_module_deps:
        _traverse_sdk_modules_graph(
            uncompiled_swift_sdk_modules_deps,
            uncompiled_clang_sdk_modules_deps,
            sdk_module_dep,
        )

    return uncompiled_swift_sdk_modules_deps, uncompiled_clang_sdk_modules_deps

def swift_toolchain_impl(ctx):
    uncompiled_swift_sdk_modules_deps, uncompiled_clang_sdk_modules_deps = compute_sdk_module_graph(ctx.attrs.sdk_modules)

    return [
        DefaultInfo(),
        SwiftToolchainInfo(
            architecture = ctx.attrs.architecture,
            compiler = cmd_args(ctx.attrs._swiftc_wrapper[RunInfo]).add(ctx.attrs.swiftc[RunInfo]),
            compiler_flags = ctx.attrs.swiftc_flags,
            library_interface_uses_swiftinterface = ctx.attrs._library_interface_uses_swiftinterface,
            mk_swift_comp_db = ctx.attrs.make_swift_comp_db[RunInfo],
            mk_swift_interface = cmd_args(ctx.attrs._swiftc_wrapper[RunInfo]).add(ctx.attrs.make_swift_interface[RunInfo]),
            object_format = SwiftObjectFormat(ctx.attrs.object_format) if ctx.attrs.object_format else SwiftObjectFormat("object"),
            platform_path = ctx.attrs.platform_path,
            resource_dir = ctx.attrs.resource_dir,
            sdk_module_path_prefixes = ctx.attrs.sdk_module_path_prefixes,
            sdk_path = ctx.attrs._internal_sdk_path or ctx.attrs.sdk_path,
            supports_explicit_module_debug_serialization = ctx.attrs.supports_explicit_module_debug_serialization,
            supports_relative_resource_dir = ctx.attrs.supports_relative_resource_dir,
            swift_ide_test_tool = ctx.attrs.swift_ide_test_tool[RunInfo] if ctx.attrs.swift_ide_test_tool else None,
            swift_stdlib_tool = ctx.attrs.swift_stdlib_tool[RunInfo],
            swift_stdlib_tool_flags = ctx.attrs.swift_stdlib_tool_flags,
            swift_experimental_features = ctx.attrs.swift_experimental_features,
            swift_upcoming_features = ctx.attrs.swift_upcoming_features,
            uncompiled_clang_sdk_modules_deps = uncompiled_clang_sdk_modules_deps,
            uncompiled_swift_sdk_modules_deps = uncompiled_swift_sdk_modules_deps,
        ),
    ]
