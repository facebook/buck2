# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

load("@prelude//cxx:cxx_toolchain_types.bzl", "CxxToolchainInfo")
load("@prelude//utils:arglike.bzl", "ArgLike")  # @unused Used as a type
load(":debug.bzl", "SplitDebugMode")

def dwp_available(toolchain: CxxToolchainInfo):
    dwp = toolchain.binary_utilities_info.dwp
    split_debug_mode = toolchain.split_debug_mode
    return dwp != None and split_debug_mode != SplitDebugMode("none")

def run_dwp_action(
        ctx: AnalysisContext,
        toolchain: CxxToolchainInfo,
        obj: Artifact,
        identifier: [str, None],
        category_suffix: [str, None],
        referenced_objects: [ArgLike, list[Artifact]],
        dwp_output: Artifact,
        local_only: bool):
    dwp = toolchain.binary_utilities_info.dwp

    args = cmd_args(
        [dwp, "-o", dwp_output.as_output(), "-e", obj] + getattr(ctx.attrs, "extra_dwp_flags", []),
        # All object/dwo files referenced in the library/executable are implicitly
        # processed by dwp.
        hidden = referenced_objects,
    )

    category = "dwp"
    if category_suffix != None:
        category += "_" + category_suffix

    ctx.actions.run(
        args,
        category = category,
        identifier = identifier,
        local_only = local_only,
    )

def dwp(
        ctx: AnalysisContext,
        toolchain: CxxToolchainInfo,
        # Executable/library to extra dwo paths from.
        obj: Artifact,
        # An identifier that will uniquely name this link action in the context of a category. Useful for
        # differentiating multiple link actions in the same rule.
        identifier: [str, None],
        # A category suffix that will be added to the category of the link action that is generated.
        category_suffix: [str, None],
        # All `.o`/`.dwo` paths referenced in `obj`.
        # TODO(T110378122): Ideally, referenced objects are a list of artifacts,
        # but currently we don't track them properly.  So, we just pass in the full
        # link line and extract all inputs from that, which is a bit of an
        # overspecification.
        referenced_objects: [ArgLike, list[Artifact]],
        name_suffix: str = "",
        local_only: bool = False) -> Artifact:
    # gdb/lldb expect to find a file named $file.dwp next to $file.
    output = ctx.actions.declare_output(obj.short_path + name_suffix + ".dwp", has_content_based_path = False)
    run_dwp_action(
        ctx,
        toolchain,
        obj,
        identifier,
        category_suffix,
        referenced_objects,
        output,
        local_only = local_only,
    )
    return output

def _dwp_rule_impl(ctx: AnalysisContext) -> list[Provider]:
    toolchain = ctx.attrs._cxx_toolchain[CxxToolchainInfo]

    exe = ctx.attrs.exe
    output = ctx.actions.declare_output(exe.short_path + ".dwp")

    run_dwp_action(
        ctx = ctx,
        toolchain = toolchain,
        obj = exe,
        identifier = ctx.attrs.name,
        category_suffix = None,
        referenced_objects = ctx.attrs.srcs,
        dwp_output = output,
        local_only = ctx.attrs.local_only,
    )

    return [DefaultInfo(default_output = output)]

dwp_rule = rule(
    impl = _dwp_rule_impl,
    attrs = {
        "exe": attrs.source(doc = "The executable to generate a DWP for"),
        "extra_dwp_flags": attrs.list(attrs.string(), default = [], doc = "Additional flags to pass to the dwp tool"),
        "local_only": attrs.bool(default = False, doc = "Whether to force local execution"),
        "srcs": attrs.list(attrs.source(), default = [], doc = "Referenced objects (debug info / dwo files) as hidden inputs"),
        "_cxx_toolchain": attrs.toolchain_dep(
            default = "toolchains//:cxx",
            providers = [CxxToolchainInfo],
        ),
    },
)
