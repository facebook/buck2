load(":zip_file_toolchain.bzl", "ZipFileToolchainInfo")

def zip_file_impl(ctx: "context") -> ["provider"]:
    """
     zip_file() rule implementation

    Args:
        ctx: rule analysis context
    Returns:
        list of created providers
    """

    zip_file_toolchain = ctx.attr._zip_file_toolchain[ZipFileToolchainInfo]
    create_zip_tool = zip_file_toolchain.create_zip[RunInfo]

    zip_output_name = ctx.attr.out if ctx.attr.out else "{}.zip".format(ctx.label.name)
    output = ctx.actions.declare_output(zip_output_name)

    on_duplicate_entry = ctx.attr.on_duplicate_entry
    entries_to_exclude = ctx.attr.entries_to_exclude
    zip_srcs = ctx.attr.zip_srcs
    srcs = ctx.attr.srcs

    create_zip_cmd = cmd_args([
        create_zip_tool,
        "--output_path",
        output.as_output(),
        "--on_duplicate_entry",
        on_duplicate_entry if on_duplicate_entry else "overwrite",
    ])

    if srcs:
        srcs_file_cmd = cmd_args()

        # add artifact and is_source flag pair
        for src in srcs:
            srcs_file_cmd.add(src)
            srcs_file_cmd.add(src.short_path)
            srcs_file_cmd.add(str(src.is_source))
        entries_file = ctx.actions.write("entries", srcs_file_cmd)

        create_zip_cmd.add("--entries_file")
        create_zip_cmd.add(entries_file)
        create_zip_cmd.hidden(srcs)

    if zip_srcs:
        create_zip_cmd.add("--zip_sources")
        create_zip_cmd.add(zip_srcs)

    if entries_to_exclude:
        create_zip_cmd.add("--entries_to_exclude")
        create_zip_cmd.add(entries_to_exclude)

    ctx.actions.run(create_zip_cmd, category = "zip")

    return [DefaultInfo(default_outputs = [output])]

def _select_zip_file_toolchain():
    return "fbsource//xplat/buck2/platform/zip_file:zip_file"

implemented_rules = {
    "zip_file": zip_file_impl,
}

extra_attributes = {
    "zip_file": {
        "_zip_file_toolchain": attr.exec_dep(
            default = _select_zip_file_toolchain(),
            providers = [
                ZipFileToolchainInfo,
            ],
        ),
    },
}
