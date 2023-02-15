# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

load("@prelude//apple:apple_toolchain_types.bzl", "AppleToolchainInfo")
load(":apple_sdk_modules_utility.bzl", "get_compiled_sdk_deps_tset", "get_uncompiled_sdk_deps")
load(":apple_utility.bzl", "get_explicit_modules_env_var", "get_versioned_target_triple")
load(":swift_pcm_compilation_types.bzl", "SwiftPCMCompiledInfo", "SwiftPCMUncompiledInfo", "WrappedSwiftPCMCompiledInfo")
load(":swift_sdk_pcm_compilation.bzl", "get_shared_pcm_compilation_args", "get_swift_sdk_pcm_anon_targets")
load(":swift_sdk_swiftinterface_compilation.bzl", "get_swift_interface_anon_targets")
load(":swift_toolchain_types.bzl", "WrappedSdkCompiledModuleInfo")

_REQUIRED_SDK_MODULES = ["Foundation"]

def _project_as_clang_deps(value: "SwiftPCMCompiledInfo"):
    return cmd_args([
        "-Xcc",
        cmd_args(["-fmodule-file=", value.name, "=", value.pcm_output], delimiter = ""),
        "-Xcc",
        cmd_args(["-fmodule-map-file=", value.exported_preprocessor.modulemap_path], delimiter = ""),
        "-Xcc",
    ] + value.exported_preprocessor.args).hidden(value.exported_preprocessor.modular_args)

PcmDepTSet = transitive_set(args_projections = {
    "clang_deps": _project_as_clang_deps,
})

def get_compiled_pcm_deps_tset(ctx: "context", pcm_deps_providers: list.type) -> "PcmDepTSet":
    pcm_deps = [
        pcm_deps_provider[WrappedSwiftPCMCompiledInfo].tset
        for pcm_deps_provider in pcm_deps_providers
        if WrappedSwiftPCMCompiledInfo in pcm_deps_provider
    ]
    return ctx.actions.tset(PcmDepTSet, children = pcm_deps)

def get_swift_pcm_anon_targets(
        ctx: "context",
        uncompiled_deps: ["dependency"],
        swift_cxx_args: [str.type]):
    deps = [
        {
            "dep": uncompiled_dep,
            "pcm_name": uncompiled_dep[SwiftPCMUncompiledInfo].name,
            "swift_cxx_args": swift_cxx_args,
            "target_sdk_version": ctx.attrs.target_sdk_version,
            "_apple_toolchain": ctx.attrs._apple_toolchain,
        }
        for uncompiled_dep in uncompiled_deps
        if SwiftPCMUncompiledInfo in uncompiled_dep
    ]
    return [(_swift_pcm_compilation, d) for d in deps]

def _swift_pcm_compilation_impl(ctx: "context") -> ["promise", ["provider"]]:
    def k(pcm_deps_providers) -> ["provider"]:
        uncompiled_pcm_info = ctx.attrs.dep[SwiftPCMUncompiledInfo]

        module_name = ctx.attrs.pcm_name
        modulemap_path = uncompiled_pcm_info.exported_preprocessor.modulemap_path

        swift_toolchain = ctx.attrs._apple_toolchain[AppleToolchainInfo].swift_toolchain_info

        cmd = cmd_args(swift_toolchain.compiler)
        cmd.add(get_shared_pcm_compilation_args(get_versioned_target_triple(ctx), module_name))
        cmd.add(["-sdk", swift_toolchain.sdk_path])
        cmd.add(swift_toolchain.compiler_flags)

        if swift_toolchain.resource_dir:
            cmd.add([
                "-resource-dir",
                swift_toolchain.resource_dir,
            ])

        # `pcm_deps_providers` will contain `WrappedSdkCompiledModuleInfo` providers
        # from direct SDK deps and transitive deps that export sdk deps.
        sdk_deps_tset = get_compiled_sdk_deps_tset(ctx, pcm_deps_providers)
        cmd.add(sdk_deps_tset.project_as_args("clang_deps"))

        # To compile a pcm we only use the exported_deps as those are the only
        # ones that should be transitively exported through public headers
        pcm_deps_tset = get_compiled_pcm_deps_tset(ctx, pcm_deps_providers)
        cmd.add(pcm_deps_tset.project_as_args("clang_deps"))

        pcm_output = ctx.actions.declare_output(module_name + ".pcm")
        cmd.add([
            "-o",
            pcm_output.as_output(),
            modulemap_path,
        ])

        # To correctly resolve modulemap's headers,
        # a search path to the root of modulemap should be passed.
        cmd.add([
            "-Xcc",
            "-I",
            "-Xcc",
            cmd_args(modulemap_path).parent(),
        ])

        cmd.add(ctx.attrs.swift_cxx_args)

        # When compiling pcm files, module's exported pps and inherited pps
        # must be provided to an action like hmaps which are used for headers resolution.
        cmd.add(uncompiled_pcm_info.propagated_preprocessor_args_cmd)

        # Modular deps like `-Swift.h` have to be materialized.
        cmd.hidden(uncompiled_pcm_info.exported_preprocessor.modular_args)

        ctx.actions.run(
            cmd,
            env = get_explicit_modules_env_var(True),
            category = "swift_pcm_compile",
            identifier = module_name,
        )

        compiled_pcm = SwiftPCMCompiledInfo(
            name = module_name,
            pcm_output = pcm_output,
            exported_preprocessor = uncompiled_pcm_info.exported_preprocessor,
        )

        return [
            DefaultInfo(default_outputs = [pcm_output]),
            WrappedSwiftPCMCompiledInfo(
                tset = ctx.actions.tset(PcmDepTSet, value = compiled_pcm, children = [pcm_deps_tset]),
            ),
            WrappedSdkCompiledModuleInfo(
                tset = sdk_deps_tset,
            ),
        ]

    # Skip deps compilations if run not on SdkUncompiledModuleInfo
    if SwiftPCMUncompiledInfo not in ctx.attrs.dep:
        return []

    direct_uncompiled_sdk_deps = get_uncompiled_sdk_deps(
        ctx.attrs.dep[SwiftPCMUncompiledInfo].uncompiled_sdk_modules,
        ctx.attrs.pcm_name,
        _REQUIRED_SDK_MODULES,
        ctx.attrs._apple_toolchain[AppleToolchainInfo].swift_toolchain_info,
    )

    # Recursively compiling SDK's Clang dependencies
    sdk_pcm_deps_anon_targets = get_swift_sdk_pcm_anon_targets(
        ctx,
        direct_uncompiled_sdk_deps,
        ctx.attrs.swift_cxx_args,
    )

    # Recursively compiling SDK's Swift dependencies
    # We need to match BUCK1 behavior, which can't distinguish between Swift and Clang SDK modules,
    # so we pass more SDK deps than is strictly necessary. When BUCK1 is deprecated, we can try to avoid doing that,
    # by passing Clang and Swift deps up separately.
    swift_interface_anon_targets = get_swift_interface_anon_targets(
        ctx,
        direct_uncompiled_sdk_deps,
        ctx.attrs.swift_cxx_args,
    )

    # Recursively compile PCMs of transitevely visible exported_deps
    swift_pcm_anon_targets = get_swift_pcm_anon_targets(
        ctx,
        ctx.attrs.dep[SwiftPCMUncompiledInfo].exported_deps,
        ctx.attrs.swift_cxx_args,
    )
    return ctx.actions.anon_targets(sdk_pcm_deps_anon_targets + swift_pcm_anon_targets + swift_interface_anon_targets).map(k)

_swift_pcm_compilation = rule(
    impl = _swift_pcm_compilation_impl,
    attrs = {
        "dep": attrs.dep(),
        "pcm_name": attrs.string(),
        "swift_cxx_args": attrs.list(attrs.string(), default = []),
        "target_sdk_version": attrs.option(attrs.string(), default = None),
        "_apple_toolchain": attrs.dep(),
    },
)
