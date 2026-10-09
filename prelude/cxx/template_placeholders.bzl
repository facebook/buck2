# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

load(
    "@prelude//linking:link_info.bzl",
    "LinkStrategy",
    "MergedLinkInfo",  # @unused Used as a type
    "get_link_args_for_strategy",
    "unpack_link_args",
)
load(":cxx_context.bzl", "get_cxx_toolchain_info")
load(
    ":preprocessor.bzl",
    "CPreprocessorInfo",  # @unused Used as a type
)

def cxx_template_placeholder_info(
        ctx: AnalysisContext,
        propagated_preprocessor: [CPreprocessorInfo, None],
        merged_native_link_info: MergedLinkInfo) -> TemplatePlaceholderInfo:
    templ_vars = {}

    if propagated_preprocessor != None:
        # Some rules, e.g. fbcode//thrift/lib/cpp:thrift-core-module
        # define preprocessor flags as things like: -DTHRIFT_PLATFORM_CONFIG=<thrift/facebook/PlatformConfig.h>
        # and unless they get quoted, they break shell syntax.
        cxx_compiler_info = get_cxx_toolchain_info(ctx).cxx_compiler_info
        cxx_preprocessor_flags = cmd_args(
            cmd_args(cxx_compiler_info.preprocessor_flags or [], quote = "shell"),
            cmd_args(propagated_preprocessor.set.project_as_args("args"), quote = "shell"),
            propagated_preprocessor.set.project_as_args("include_dirs"),
        )
        templ_vars["cxxppflags"] = cxx_preprocessor_flags

        c_compiler_info = get_cxx_toolchain_info(ctx).c_compiler_info
        c_preprocessor_flags = cmd_args(
            cmd_args(c_compiler_info.preprocessor_flags or [], quote = "shell"),
            cmd_args(propagated_preprocessor.set.project_as_args("args"), quote = "shell"),
            propagated_preprocessor.set.project_as_args("include_dirs"),
        )
        templ_vars["cppflags"] = c_preprocessor_flags

    # Add in ldflag macros.
    for link_strategy in (LinkStrategy("static"), LinkStrategy("static_pic")):
        name = "ldflags-" + link_strategy.value.replace("_", "-")
        args = []
        linker_info = get_cxx_toolchain_info(ctx).linker_info
        args.append(linker_info.linker_flags or [])

        # Normally, we call get_link_args_for_strategy for getting the args for our own link from our
        # deps. This case is a bit different as we are effectively trying to get the args for how this library
        # would be represented on a dependent's link line and so it is appropriate to use our own merged_native_link_info.
        link_args = get_link_args_for_strategy(
            ctx.actions,
            ctx.label,
            get_cxx_toolchain_info(ctx).linker_info,
            [merged_native_link_info],
            link_strategy,
            prefer_stripped = False,
            transformation_spec_context = None,
        )
        args.append(unpack_link_args(link_args))
        templ_vars[name] = cmd_args(args)

    # TODO(T110378127): To implement `$(ldflags-shared ...)` properly, we'd need
    # to setup a symink tree rule for all transitive shared libs.  Since this
    # currently would be pretty costly (O(N^2)?), and since it's not that
    # commonly used anyway, just use `static-pic` instead.  Longer-term, once
    # v1 is gone, macros that use `$(ldflags-shared ...)` (e.g. Haskell's
    # hsc2hs) can move to a v2 rules-based API to avoid needing this macro.
    templ_vars["ldflags-shared"] = templ_vars["ldflags-static-pic"]

    return TemplatePlaceholderInfo(keyed_variables = templ_vars)
