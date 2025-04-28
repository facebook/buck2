# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

load("@prelude//decls:toolchains_common.bzl", "toolchains_common")
load(":zip_file_toolchain.bzl", "ZipFileToolchainInfo")

def _zip_file_impl(ctx: AnalysisContext) -> list[Provider]:
    """
     zip_file() rule implementation

    Args:
        ctx: rule analysis context
    Returns:
        list of created providers
    """

    zip_file_toolchain = ctx.attrs._zip_file_toolchain[ZipFileToolchainInfo]
    create_zip_tool = zip_file_toolchain.create_zip

    zip_output_name = ctx.attrs.out if ctx.attrs.out else "{}.zip".format(ctx.label.name)
    output = ctx.actions.declare_output(zip_output_name)

    on_duplicate_entry = ctx.attrs.on_duplicate_entry
    entries_to_exclude = ctx.attrs.entries_to_exclude
    hardcode_permissions_for_deterministic_output = ctx.attrs.hardcode_permissions_for_deterministic_output
    zip_srcs = ctx.attrs.zip_srcs
    srcs = ctx.attrs.srcs

    create_zip_cmd = [
        create_zip_tool,
        "--output_path",
        output.as_output(),
        "--on_duplicate_entry",
        on_duplicate_entry if on_duplicate_entry else "overwrite",
    ]

    if srcs:
        # add artifact and is_source flag pair
        srcs_file_cmd = cmd_args(
            [
                [src, src.short_path, str(src.is_source)]
                for src in srcs
            ],
        )
        entries_file = ctx.actions.write("entries", srcs_file_cmd)

        create_zip_cmd.append("--entries_file")
        create_zip_cmd.append(entries_file)
        create_zip_cmd.append(cmd_args(hidden = srcs))

    if zip_srcs:
        create_zip_cmd.append("--zip_sources")
        create_zip_cmd.append(zip_srcs)

    if entries_to_exclude:
        create_zip_cmd.append("--entries_to_exclude")
        create_zip_cmd.append(entries_to_exclude)

    if hardcode_permissions_for_deterministic_output:
        create_zip_cmd.append("--hardcode_permissions_for_deterministic_output")

    ctx.actions.run(cmd_args(create_zip_cmd), category = "zip")

    return [DefaultInfo(default_output = output)]

implemented_rules = {
    "zip_file": _zip_file_impl,
}

extra_attributes = {
    "zip_file": {
        "_zip_file_toolchain": toolchains_common.zip_file(),
    },
}
