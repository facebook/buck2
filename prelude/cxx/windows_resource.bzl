# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

load("@prelude//cxx:cxx_context.bzl", "get_cxx_toolchain_info")
load("@prelude//cxx:headers.bzl", "cxx_get_regular_cxx_headers_layout")
load("@prelude//cxx:preprocessor.bzl", "cxx_merge_cpreprocessors", "cxx_private_preprocessor_info")
load("@prelude//linking:link_groups.bzl", "LinkGroupLibInfo")
load("@prelude//linking:link_info.bzl", "LibOutputStyle", "LinkInfo", "LinkInfos", "ObjectsLinkable", "create_merged_link_info")
load("@prelude//linking:linkable_graph.bzl", "create_linkable_graph")
load("@prelude//linking:shared_libraries.bzl", "SharedLibraryInfo")

def windows_resource_impl(ctx: AnalysisContext) -> list[Provider]:
    (own_non_exported_preprocessor_info, _) = cxx_private_preprocessor_info(
        ctx = ctx,
        headers_layout = cxx_get_regular_cxx_headers_layout(ctx),
        raw_headers = ctx.attrs.raw_headers,
        extra_preprocessors = [],
        non_exported_deps = [],
        is_test = False,
    )

    preprocessor = cxx_merge_cpreprocessors(
        ctx,
        [own_non_exported_preprocessor_info],
        [],
    )

    headers_tag = ctx.actions.artifact_tag()

    objects = []

    toolchain = get_cxx_toolchain_info(ctx)
    for src in ctx.attrs.srcs:
        rc_output = ctx.actions.declare_output(
            "__objects__",
            "{}.res".format(src.short_path),
        )
        rc_cmd = cmd_args(
            toolchain.rc_compiler_info.compiler,
            toolchain.rc_compiler_info.compiler_flags,
            cmd_args(rc_output.as_output(), format = "/fo{}"),
            headers_tag.tag_artifacts(preprocessor.set.project_as_args("args")),
            headers_tag.tag_artifacts(preprocessor.set.project_as_args("include_dirs")),
            src,
        )

        ctx.actions.run(
            rc_cmd,
            category = "rc_compile",
        )

        cvtres_output = ctx.actions.declare_output(
            "__objects__",
            "{}.obj".format(src.short_path),
        )
        cvtres_cmd = cmd_args(
            toolchain.cvtres_compiler_info.compiler,
            toolchain.cvtres_compiler_info.compiler_flags,
            cmd_args(cvtres_output.as_output(), format = "/OUT:{}"),
            rc_output,
        )

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
