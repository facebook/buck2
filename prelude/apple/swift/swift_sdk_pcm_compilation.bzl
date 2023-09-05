# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

# @starlark-rust: allow_string_literals_in_type_expr

load("@prelude//apple:apple_toolchain_types.bzl", "AppleToolchainInfo")
load("@prelude//apple:apple_utility.bzl", "expand_relative_prefixed_sdk_path", "get_disable_pch_validation_flags")
load(":apple_sdk_modules_utility.bzl", "SDKDepTSet", "get_compiled_sdk_deps_tset")
load(":swift_toolchain_types.bzl", "SdkCompiledModuleInfo", "SdkTransitiveDepsTset", "SdkUncompiledModuleInfo", "WrappedSdkCompiledModuleInfo")

def get_shared_pcm_compilation_args(module_name: str) -> cmd_args:
    cmd = cmd_args()
    cmd.add([
        "-emit-pcm",
        "-module-name",
        module_name,
        "-Xfrontend",
        "-disable-implicit-swift-modules",
        "-Xcc",
        "-fno-implicit-modules",
        "-Xcc",
        "-fno-implicit-module-maps",
        # Embed all input files into the PCM so we don't need to include module map files when
        # building remotely.
        # https://github.com/apple/llvm-project/commit/fb1e7f7d1aca7bcfc341e9214bda8b554f5ae9b6
        "-Xcc",
        "-Xclang",
        "-Xcc",
        "-fmodules-embed-all-files",
        # Embed all files that were read during compilation into the generated PCM.
        "-Xcc",
        "-Xclang",
        "-Xcc",
        "-fmodule-file-home-is-cwd",
        # Once we have an empty working directory the compiler provided headers such as float.h
        # cannot be found, so add . to the header search paths.
        "-Xcc",
        "-I.",
    ])

    cmd.add(get_disable_pch_validation_flags())

    return cmd

def _remove_path_components_from_right(path: str, count: int):
    path_components = path.split("/")
    removed_path = "/".join(path_components[0:-count])
    return removed_path

def _add_sdk_module_search_path(cmd, uncompiled_sdk_module_info, apple_toolchain):
    modulemap_path = uncompiled_sdk_module_info.input_relative_path

    # If this input is a framework we need to search above the
    # current framework location, otherwise we include the
    # modulemap root.
    if uncompiled_sdk_module_info.is_framework:
        frameworks_dir_path = _remove_path_components_from_right(modulemap_path, 3)
        expanded_path = expand_relative_prefixed_sdk_path(
            cmd_args(apple_toolchain.swift_toolchain_info.sdk_path),
            cmd_args(apple_toolchain.swift_toolchain_info.resource_dir),
            cmd_args(apple_toolchain.platform_path),
            frameworks_dir_path,
        )
    else:
        module_root_path = _remove_path_components_from_right(modulemap_path, 1)
        expanded_path = expand_relative_prefixed_sdk_path(
            cmd_args(apple_toolchain.swift_toolchain_info.sdk_path),
            cmd_args(apple_toolchain.swift_toolchain_info.resource_dir),
            cmd_args(apple_toolchain.platform_path),
            module_root_path,
        )
    cmd.add([
        "-Xcc",
        ("-F" if uncompiled_sdk_module_info.is_framework else "-I"),
        "-Xcc",
        cmd_args(expanded_path),
    ])

def get_swift_sdk_pcm_anon_targets(
        ctx: AnalysisContext,
        uncompiled_sdk_deps: list[Dependency],
        swift_cxx_args: list[str]):
    # First collect the direct clang module deps.
    clang_deps = [
        d
        for d in uncompiled_sdk_deps
        if SdkUncompiledModuleInfo in d and not d[SdkUncompiledModuleInfo].is_swiftmodule
    ]

    # We need to collect the transitive clang module deps across _all_
    # SDK deps, as we need to pass through clang deps of Swift SDK deps.
    # These cannot be propagated through the Swift SDK deps as those
    # use different swift_cxx_args when compiling their clang deps.
    transitive_clang_dep_tset = ctx.actions.tset(SdkTransitiveDepsTset, children = [
        uncompiled_sdk_dep[SdkUncompiledModuleInfo].transitive_clang_deps
        for uncompiled_sdk_dep in uncompiled_sdk_deps
        if SdkUncompiledModuleInfo in uncompiled_sdk_dep
    ])
    clang_deps += list(transitive_clang_dep_tset.traverse())

    return [
        (_swift_sdk_pcm_compilation, {
            "dep": clang_module_dep,
            "swift_cxx_args": swift_cxx_args,
            "_apple_toolchain": ctx.attrs._apple_toolchain,
        })
        for clang_module_dep in clang_deps
    ]

def _swift_sdk_pcm_compilation_impl(ctx: AnalysisContext) -> ["promise", list[Provider]]:
    def k(sdk_pcm_deps_providers) -> list[Provider]:
        uncompiled_sdk_module_info = ctx.attrs.dep[SdkUncompiledModuleInfo]
        module_name = uncompiled_sdk_module_info.module_name
        apple_toolchain = ctx.attrs._apple_toolchain[AppleToolchainInfo]
        swift_toolchain = apple_toolchain.swift_toolchain_info
        cmd = cmd_args(swift_toolchain.compiler)
        cmd.add(uncompiled_sdk_module_info.partial_cmd)
        cmd.add(["-sdk", swift_toolchain.sdk_path])
        cmd.add(swift_toolchain.compiler_flags)

        if swift_toolchain.resource_dir:
            cmd.add([
                "-resource-dir",
                swift_toolchain.resource_dir,
            ])

        sdk_deps_tset = get_compiled_sdk_deps_tset(ctx, sdk_pcm_deps_providers)
        cmd.add(sdk_deps_tset.project_as_args("clang_deps"))

        expanded_modulemap_path_cmd = expand_relative_prefixed_sdk_path(
            cmd_args(swift_toolchain.sdk_path),
            cmd_args(swift_toolchain.resource_dir),
            cmd_args(apple_toolchain.platform_path),
            uncompiled_sdk_module_info.input_relative_path,
        )
        pcm_output = ctx.actions.declare_output(module_name + ".pcm")
        cmd.add([
            "-o",
            pcm_output.as_output(),
            expanded_modulemap_path_cmd,
        ])

        # For SDK modules we need to set a few more args
        cmd.add([
            "-Xcc",
            "-Xclang",
            "-Xcc",
            "-emit-module",
            "-Xcc",
            "-Xclang",
            "-Xcc",
            "-fsystem-module",
        ])

        cmd.add(ctx.attrs.swift_cxx_args)

        _add_sdk_module_search_path(cmd, uncompiled_sdk_module_info, apple_toolchain)

        ctx.actions.run(
            cmd,
            category = "sdk_swift_pcm_compile",
            identifier = module_name,
            # Swift compiler requires unique inodes for all input files.
            unique_input_inodes = True,
        )

        compiled_sdk = SdkCompiledModuleInfo(
            name = uncompiled_sdk_module_info.name,
            module_name = module_name,
            is_framework = uncompiled_sdk_module_info.is_framework,
            output_artifact = pcm_output,
            is_swiftmodule = False,
            input_relative_path = expanded_modulemap_path_cmd,
        )

        return [
            DefaultInfo(),
            WrappedSdkCompiledModuleInfo(
                tset = ctx.actions.tset(SDKDepTSet, value = compiled_sdk, children = [sdk_deps_tset]),
            ),
        ]

    # Compile the transitive clang module deps of this target.
    clang_module_deps = [(_swift_sdk_pcm_compilation, {
        "dep": d,
        "swift_cxx_args": ctx.attrs.swift_cxx_args,
        "_apple_toolchain": ctx.attrs._apple_toolchain,
    }) for d in ctx.attrs.dep[SdkUncompiledModuleInfo].transitive_clang_deps.traverse()]

    return ctx.actions.anon_targets(clang_module_deps).map(k)

_swift_sdk_pcm_compilation = rule(
    impl = _swift_sdk_pcm_compilation_impl,
    attrs = {
        "dep": attrs.dep(),
        "swift_cxx_args": attrs.list(attrs.string(), default = []),
        "_apple_toolchain": attrs.dep(),
    },
)
