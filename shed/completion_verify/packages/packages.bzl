# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

# @oss-disable[end= ]: load("@fbcode_macros//build_defs:platform_utils.bzl", "platform_utils")

def _impl(ctx: AnalysisContext) -> list[Provider]:
    out = ctx.actions.declare_output(ctx.attrs.name, dir = True)
    ctx.actions.run(
        cmd_args(
            ctx.attrs.download_tool,
            ctx.attrs.rpm_name,
            out.as_output(),
        ),
        category = "download_rpm",
        local_only = True,
    )
    return [DefaultInfo(default_output = out)]

download_rpm_impl = rule(
    impl = _impl,
    attrs = {
        "download_tool": attrs.source(),
        "rpm_name": attrs.string(),
    },
)

def download_rpm(**kwargs):
    prelude = native

    platform_utils = None # @oss-enable
    dtp = platform_utils.get_cxx_platform_for_base_path(prelude.package_name()).target_platform if platform_utils else None

    download_rpm_impl(
        download_tool = "download.sh",
        default_target_platform = dtp,
        visibility = ["PUBLIC"],
        **kwargs
    )
