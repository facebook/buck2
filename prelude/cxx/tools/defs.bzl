# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

load("@prelude//cxx:cxx_toolchain_types.bzl", "CxxInternalTools", "DistLtoToolsInfo")

def _cxx_internal_tools_impl(ctx: AnalysisContext) -> list[Provider]:
    return [
        DefaultInfo(),
        CxxInternalTools(
            concatenate_diagnostics = ctx.attrs.concatenate_diagnostics[RunInfo],
            dep_file_processor = ctx.attrs.dep_file_processor[RunInfo],
            dist_lto = ctx.attrs.dist_lto[DistLtoToolsInfo],
            hmap_wrapper = ctx.attrs.hmap_wrapper[RunInfo],
            make_comp_db = ctx.attrs.make_comp_db[RunInfo],
            remap_cwd = ctx.attrs.remap_cwd[RunInfo],
            stderr_to_file = ctx.attrs.stderr_to_file[RunInfo],
        ),
    ]

cxx_internal_tools = rule(
    impl = _cxx_internal_tools_impl,
    attrs = {
        "concatenate_diagnostics": attrs.default_only(attrs.dep(providers = [RunInfo], default = "prelude//cxx/tools:concatenate_diagnostics")),
        "dep_file_processor": attrs.default_only(attrs.dep(providers = [RunInfo], default = "prelude//cxx/tools:dep_file_processor")),
        "dist_lto": attrs.default_only(attrs.dep(providers = [DistLtoToolsInfo], default = "prelude//cxx/dist_lto/tools:dist_lto_tools")),
        "hmap_wrapper": attrs.default_only(attrs.dep(providers = [RunInfo], default = "prelude//cxx/tools:hmap_wrapper")),
        "make_comp_db": attrs.default_only(attrs.dep(providers = [RunInfo], default = "prelude//cxx/tools:make_comp_db")),
        "remap_cwd": attrs.default_only(attrs.dep(providers = [RunInfo], default = "prelude//cxx/tools:remap_cwd")),
        "stderr_to_file": attrs.default_only(attrs.dep(providers = [RunInfo], default = "prelude//cxx/tools:stderr_to_file")),
    },
)

def _cxx_hacks_impl(_ctx):
    return [DefaultInfo(), TemplatePlaceholderInfo(
        unkeyed_variables = {
            "cxx-header-tree": "/dev/null/HACK-CXX-HEADER-TREE",
        },
    )]

cxx_hacks = rule(
    impl = _cxx_hacks_impl,
    attrs = {},
)
