# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

# @starlark-rust: allow_string_literals_in_type_expr

load("@prelude//apple:apple_toolchain_types.bzl", "AppleToolchainInfo")
load("@prelude//apple:apple_utility.bzl", "expand_relative_prefixed_sdk_path", "get_explicit_modules_env_var")
load("@prelude//apple/swift:swift_types.bzl", "SWIFTMODULE_EXTENSION")
load(":swift_module_map.bzl", "write_swift_module_map")
load(":swift_sdk_pcm_compilation.bzl", "get_swift_sdk_pcm_anon_targets")
load(":swift_toolchain_types.bzl", "SdkUncompiledModuleInfo", "SwiftCompiledModuleInfo", "SwiftCompiledModuleTset", "WrappedSdkCompiledModuleInfo")

def get_swift_interface_anon_targets(
        ctx: AnalysisContext,
        uncompiled_sdk_deps: list[Dependency]):
    return [
        (
            _swift_interface_compilation,
            {
                "dep": d,
                "_apple_toolchain": ctx.attrs._apple_toolchain,
            },
        )
        for d in uncompiled_sdk_deps
        if d[SdkUncompiledModuleInfo].is_swiftmodule
    ]

def _swift_interface_compilation_impl(ctx: AnalysisContext) -> [Promise, list[Provider]]:
    def k(sdk_deps_providers) -> list[Provider]:
        uncompiled_sdk_module_info = ctx.attrs.dep[SdkUncompiledModuleInfo]
        uncompiled_module_info_name = uncompiled_sdk_module_info.module_name
        apple_toolchain = ctx.attrs._apple_toolchain[AppleToolchainInfo]
        swift_toolchain = apple_toolchain.swift_toolchain_info
        cmd = cmd_args(swift_toolchain.compiler)
        cmd.add(uncompiled_sdk_module_info.partial_cmd)
        cmd.add(["-sdk", swift_toolchain.sdk_path])

        if swift_toolchain.resource_dir:
            cmd.add([
                "-resource-dir",
                swift_toolchain.resource_dir,
            ])

        # `sdk_deps_providers` contains providers of clang and swift SDK deps.
        # Keep them separated as we need to only pass up the swiftmodule deps.
        clang_dep_children = []
        swift_dep_children = []
        for sdk_dep in sdk_deps_providers:
            tset = sdk_dep[WrappedSdkCompiledModuleInfo].tset
            if tset.value and tset.value.is_swiftmodule:
                swift_dep_children.append(tset)
            else:
                clang_dep_children.append(tset)

        clang_deps_tset = ctx.actions.tset(SwiftCompiledModuleTset, children = clang_dep_children)
        swift_deps_tset = ctx.actions.tset(SwiftCompiledModuleTset, children = swift_dep_children)

        # FIXME: - Get rid of slow traversal here, and unify with two projections below.
        swift_module_map_artifact = write_swift_module_map(ctx, uncompiled_module_info_name, list(swift_deps_tset.traverse()))
        cmd.add([
            "-explicit-swift-module-map-file",
            swift_module_map_artifact,
        ])
        cmd.add(clang_deps_tset.project_as_args("clang_deps"))

        swiftmodule_output = ctx.actions.declare_output(uncompiled_module_info_name + SWIFTMODULE_EXTENSION)
        expanded_swiftinterface_cmd = expand_relative_prefixed_sdk_path(
            cmd_args(swift_toolchain.sdk_path),
            cmd_args(swift_toolchain.resource_dir),
            cmd_args(apple_toolchain.platform_path),
            uncompiled_sdk_module_info.input_relative_path,
        )
        cmd.add([
            "-o",
            swiftmodule_output.as_output(),
            expanded_swiftinterface_cmd,
        ])

        ctx.actions.run(
            cmd,
            env = get_explicit_modules_env_var(True),
            category = "sdk_swiftinterface_compile",
            identifier = uncompiled_module_info_name,
        )

        compiled_sdk = SwiftCompiledModuleInfo(
            is_framework = uncompiled_sdk_module_info.is_framework,
            is_swiftmodule = True,
            module_name = uncompiled_module_info_name,
            output_artifact = swiftmodule_output,
        )

        return [
            DefaultInfo(),
            WrappedSdkCompiledModuleInfo(
                tset = ctx.actions.tset(SwiftCompiledModuleTset, value = compiled_sdk, children = swift_dep_children),
            ),
        ]

    # For each swiftinterface compile its transitive clang deps with the provided target.
    module_info = ctx.attrs.dep[SdkUncompiledModuleInfo]
    clang_module_deps = get_swift_sdk_pcm_anon_targets(
        ctx,
        module_info.deps,
        ["-target", module_info.target],
    )

    # Compile the transitive swiftmodule deps.
    swift_module_deps = get_swift_interface_anon_targets(ctx, module_info.deps)

    return ctx.actions.anon_targets(clang_module_deps + swift_module_deps).map(k)

_swift_interface_compilation = rule(
    impl = _swift_interface_compilation_impl,
    attrs = {
        "dep": attrs.dep(),
        "_apple_toolchain": attrs.dep(),
    },
)
