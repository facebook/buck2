# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

load("@prelude//cxx:cxx_context.bzl", "get_cxx_toolchain_info")
load("@prelude//linking:link_groups.bzl", "LinkGroupLibInfo")
load("@prelude//linking:link_info.bzl", "LibOutputStyle", "LinkInfo", "LinkInfos", "ObjectsLinkable", "create_merged_link_info")
load("@prelude//linking:linkable_graph.bzl", "create_linkable_graph")
load("@prelude//linking:shared_libraries.bzl", "SharedLibraryInfo")

def windows_resource_impl(ctx: AnalysisContext) -> list[Provider]:
    objects = []

    toolchain = get_cxx_toolchain_info(ctx)
    for src in ctx.attrs.srcs:
        rc_output = ctx.actions.declare_output(
            "__objects__",
            "{}.res".format(src.short_path),
        )
        rc_cmd = cmd_args(toolchain.rc_compiler_info.compiler)
        rc_cmd.add(toolchain.rc_compiler_info.compiler_flags)
        rc_cmd.add(cmd_args(rc_output.as_output(), format = "/fo{}"))
        rc_cmd.add(src)

        ctx.actions.run(
            rc_cmd,
            category = "rc_compile",
        )

        cvtres_output = ctx.actions.declare_output(
            "__objects__",
            "{}.obj".format(src.short_path),
        )
        cvtres_cmd = cmd_args(toolchain.cvtres_compiler_info.compiler)
        cvtres_cmd.add(toolchain.cvtres_compiler_info.compiler_flags)
        cvtres_cmd.add(cmd_args(cvtres_output.as_output(), format = "/OUT:{}"))
        cvtres_cmd.add(rc_output)

        ctx.actions.run(
            cvtres_cmd,
            category = "cvtres_compile",
        )

        objects.append(cvtres_output)

    link = LinkInfo(
        name = ctx.attrs.name,
        linkables = [ObjectsLinkable(
            objects = objects,
            linker_type = toolchain.linker_info.type,
            link_whole = True,
        )],
    )

    providers = [
        DefaultInfo(default_output = None),
        SharedLibraryInfo(set = None),
        LinkGroupLibInfo(libs = {}),
        create_linkable_graph(ctx),
        create_merged_link_info(
            ctx,
            toolchain.pic_behavior,
            {output_style: LinkInfos(default = link) for output_style in LibOutputStyle},
        ),
    ]

    return providers
