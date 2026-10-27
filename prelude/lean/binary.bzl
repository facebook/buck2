# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

"""`lean_binary` and `lean_test`: Lean executables.

They compile their sources like `lean_library`, and link the objects, the
dependencies and the toolchain runtime into an executable with the cxx
toolchain. One of the modules must define `main`.
"""

load("@prelude//cxx:cxx_context.bzl", "get_cxx_toolchain_info")
load("@prelude//cxx:link.bzl", "cxx_link_into")
load("@prelude//cxx:link_types.bzl", "CxxLinkResultType", "link_options")
load("@prelude//linking:execution_preference.bzl", "LinkExecutionPreference")
load(
    "@prelude//linking:link_info.bzl",
    "LinkArgs",
    "LinkStrategy",
    "get_link_args_for_strategy",
)
load(":library.bzl", "compile_lean_library", "lean_library_attrs")

def _link_executable(ctx: AnalysisContext) -> Artifact:
    library = compile_lean_library(ctx, link_whole = False)
    cxx = get_cxx_toolchain_info(ctx)
    link_args = get_link_args_for_strategy(
        ctx.actions,
        ctx.label,
        cxx.linker_info,
        [library.merged_link_info],
        LinkStrategy("static_pic"),
        prefer_stripped = False,
        transformation_spec_context = None,
    )
    result = cxx_link_into(
        ctx = ctx,
        output = ctx.actions.declare_output(ctx.label.name, has_content_based_path = False),
        result_type = CxxLinkResultType("executable"),
        opts = link_options(
            links = [
                LinkArgs(flags = cmd_args(cxx.linker_info.binary_linker_flags, ctx.attrs.linker_flags)),
                link_args,
            ],
            link_execution_preference = LinkExecutionPreference("any"),
            category_suffix = "lean_binary",
        ),
    )
    return result.linked_object.output

def _lean_binary_impl(ctx: AnalysisContext) -> list[Provider]:
    exe = _link_executable(ctx)
    return [
        DefaultInfo(default_output = exe),
        RunInfo(args = cmd_args(exe)),
    ]

def _lean_test_impl(ctx: AnalysisContext) -> list[Provider]:
    exe = _link_executable(ctx)
    command = [cmd_args(exe)] + ctx.attrs.args
    return [
        DefaultInfo(default_output = exe),
        RunInfo(args = cmd_args(command)),
        ExternalRunnerTestInfo(
            type = "custom",
            command = command,
            env = ctx.attrs.env,
            labels = ctx.attrs.labels,
            contacts = ctx.attrs.contacts,
        ),
    ]

_binary_attrs = lean_library_attrs | {
    # Read by `cxx_link_into`.
    "bolt_flags": attrs.list(attrs.arg(), default = []),
    "bolt_profile": attrs.option(attrs.source(), default = None),
    "deffile": attrs.option(attrs.source(), default = None),
    "linker_flags": attrs.list(attrs.arg(), default = [], doc = "Extra linker flags."),
    "strip_stapsdt": attrs.bool(default = False),
}

lean_binary = rule(
    impl = _lean_binary_impl,
    attrs = _binary_attrs,
)

lean_test = rule(
    impl = _lean_test_impl,
    attrs = _binary_attrs
    | {
        "args": attrs.list(attrs.arg(), default = [], doc = "Arguments for the test binary."),
        "contacts": attrs.list(attrs.string(), default = []),
        "env": attrs.dict(key = attrs.string(), value = attrs.arg(), sorted = False, default = {}, doc = "Environment of the test binary."),
    },
)
