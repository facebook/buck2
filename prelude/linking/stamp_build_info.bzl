# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

load("@prelude//:paths.bzl", "paths")
load("@prelude//cxx:cxx_context.bzl", "get_cxx_toolchain_info")

def stamp_build_info(ctx: AnalysisContext, obj: Artifact) -> Artifact:
    """
    If necessary, add fb_build_info section to binary via late-stamping
    """
    if hasattr(ctx.attrs, "_build_info") and ctx.attrs._build_info:
        ctx.attrs._build_info["late_stamping"] = True
        build_info_json = ctx.actions.write_json(obj.short_path + "-build-info.json", ctx.attrs._build_info)
        stem, ext = paths.split_extension(obj.short_path)
        stamped_output = ctx.actions.declare_output(stem + "-stamped" + ext)

        ctx.actions.run(
            cmd_args([
                get_cxx_toolchain_info(ctx).binary_utilities_info.objcopy,
                "--add-section",
                cmd_args(build_info_json, format = "fb_build_info={}"),
                obj,
                stamped_output.as_output(),
            ]),
            identifier = obj.short_path,
            category = "stamp_build_info",
        )
        return stamped_output
    return obj
