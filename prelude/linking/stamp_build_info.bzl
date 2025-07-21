# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

load("@prelude//:paths.bzl", "paths")
load("@prelude//cxx:cxx_context.bzl", "get_cxx_toolchain_info")

PRE_STAMPED_SUFFIX = "-pre_stamped"

def cxx_stamp_build_info(ctx: AnalysisContext) -> bool:
    return hasattr(ctx.attrs, "_build_info") and bool(ctx.attrs._build_info)

def stamp_build_info(ctx: AnalysisContext, obj: Artifact, stamped_output: Artifact | None = None) -> Artifact:
    """
    If necessary, add fb_build_info section to binary via late-stamping
    """
    if cxx_stamp_build_info(ctx):
        ctx.attrs._build_info["late_stamping"] = True
        build_info_json = ctx.actions.write_json(obj.short_path + "-build-info.json", ctx.attrs._build_info)
        stem, ext = paths.split_extension(obj.short_path)
        if not stamped_output:
            name = stem.removesuffix(PRE_STAMPED_SUFFIX) if stem.endswith(PRE_STAMPED_SUFFIX) else stem + "-stamped"
            stamped_output = ctx.actions.declare_output(name + ext)

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
            # This can be run remotely, but it's often cheaper to do this locally for large
            # binaries
            prefer_local = True,
        )
        return stamped_output
    return obj
