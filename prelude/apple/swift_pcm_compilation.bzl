# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

load(":apple_sdk_modules_utility.bzl", "get_sdk_deps_tset")
load(":apple_toolchain_types.bzl", "AppleToolchainInfo")
load(":apple_utility.bzl", "get_module_name", "get_versioned_target_triple")
load(":swift_pcm_compilation_types.bzl", "SwiftPCMCompilationInfo")
load(":swift_sdk_pcm_compilation.bzl", "get_shared_pcm_compilation_args")

_REQUIRED_SDK_MODULES = ["Foundation"]

def _project_as_clang_deps(value: "SwiftPCMCompilationInfo"):
    return [
        "-Xcc",
        cmd_args(["-fmodule-file=", value.name, "=", value.pcm_output], delimiter = ""),
        "-Xcc",
        cmd_args(["-fmodule-map-file=", value.exported_pre.modulemap_path], delimiter = ""),
        "-Xcc",
    ] + value.exported_pre.args

PcmDepTSet = transitive_set(args_projections = {
    "clang_deps": _project_as_clang_deps,
})

def get_pcm_deps_tset(ctx: "context", deps: ["dependency"]) -> "PcmDepTSet":
    pcm_deps = [
        ctx.actions.tset(
            PcmDepTSet,
            value = d[SwiftPCMCompilationInfo],
            children = [d[SwiftPCMCompilationInfo].deps_set],
        )
        for d in deps
        if SwiftPCMCompilationInfo in d
    ]
    return ctx.actions.tset(PcmDepTSet, children = pcm_deps)

def compile_swift_pcm(
        ctx: "context",
        exported_pre: "CPreprocessor",
        propagated_exported_preprocessor_info: ["CPreprocessorInfo", None]) -> ["SwiftPCMCompilationInfo", None]:
    module_name = get_module_name(ctx)
    modulemap_path = exported_pre.modulemap_path

    toolchain = ctx.attrs._apple_toolchain[AppleToolchainInfo].swift_toolchain_info
    cmd = cmd_args(toolchain.compiler)
    cmd.add(get_shared_pcm_compilation_args(get_versioned_target_triple(ctx), module_name))
    cmd.add(["-sdk", toolchain.sdk_path])
    cmd.add(toolchain.compiler_flags)

    sdk_deps_tset = get_sdk_deps_tset(
        ctx,
        module_name,
        ctx.attrs.exported_deps,
        _REQUIRED_SDK_MODULES,
        toolchain,
    )
    cmd.add(sdk_deps_tset.project_as_args("clang_deps"))

    if toolchain.resource_dir:
        cmd.add([
            "-resource-dir",
            toolchain.resource_dir,
        ])

    # To compile a pcm we only use the exported_deps as those are the only
    # ones that should be transitively exported through public headers
    pcm_deps_tset = get_pcm_deps_tset(ctx, ctx.attrs.exported_deps)
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

    # When compiling pcm files, module's exported pps and inherited pps
    # must be provided to an action like hmaps which are used for headers resolution.
    if propagated_exported_preprocessor_info:
        cmd.add(cmd_args(propagated_exported_preprocessor_info.set.project_as_args("args"), prepend = "-Xcc"))

    ctx.actions.run(cmd, category = "swift_pcm_compile", identifier = module_name)

    return SwiftPCMCompilationInfo(
        name = module_name,
        pcm_output = pcm_output,
        exported_pre = exported_pre,
        deps_set = ctx.actions.tset(
            PcmDepTSet,
            children = [pcm_deps_tset],
        ),
        sdk_deps_set = sdk_deps_tset,
    )
