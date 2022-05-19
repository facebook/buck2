load("@fbcode//buck2/prelude/utils:utils.bzl", "expect", "value_or")

# Flags to apply to decompress the various types of archives.
_FLAGS = {
    "tar.gz": "-z",
    "tar.zst": "--use-compress-program=unzstd",
}

def _type(ctx: "context") -> str.type:
    typ = value_or(ctx.attr.type, "tar.gz")
    if typ not in ("tar.gz", "tar.zst"):
        fail("unsupported `type`: {}".format(typ))
    return typ

def http_archive_impl(ctx: "context") -> ["provider"]:
    expect(len(ctx.attr.urls) == 1, "multiple `urls` not support: {}".format(ctx.attr.urls))
    expect(ctx.attr.strip_prefix == None, "`strip_prefix` not supported: {}".format(ctx.attr.strip_prefix))

    # The HTTP download is local so it makes little sense to run actions
    # remotely, unless we can defer them.
    local_only = ctx.attr.sha1 == None

    # Download archive.
    archive = ctx.actions.declare_output("archive.tgz")
    url = ctx.attr.urls[0]
    ctx.actions.download_file(url, archive.as_output(), sha1 = ctx.attr.sha1, sha256 = ctx.attr.sha256, is_deferrable = True)

    # Unpack archive to output directory.
    compress_flag = _FLAGS[_type(ctx)]

    exclude_flags = []
    exclude_hidden = []
    if ctx.attr.excludes:
        # Tar excludes files using globs, but we take regexes, so we need to
        # apply our regexes onto the file listing and produce an exclusion list
        # that just has strings.
        exclusions = ctx.actions.declare_output("exclusions")
        create_exclusion_list = [
            ctx.attr._create_exclusion_list[RunInfo],
            "--tar-archive",
            archive,
            cmd_args(compress_flag, format = "--tar-flag={}"),
            "--out",
            exclusions.as_output(),
        ]
        for exclusion in ctx.attr.excludes:
            create_exclusion_list.append(cmd_args(exclusion, format = "--exclude={}"))

        ctx.actions.run(create_exclusion_list, category = "process_exclusions", local_only = local_only)
        exclude_flags.append(cmd_args(exclusions, format = "--exclude-from={}"))
        exclude_hidden.append(exclusions)

    output = ctx.actions.declare_output(value_or(ctx.attr.out, ctx.label.name))
    script, hidden = ctx.actions.write(
        "unpack.sh",
        [
            cmd_args(output, format = "mkdir -p {}"),
            cmd_args(output, format = "cd {}"),
            cmd_args(
                [
                    "tar",
                    compress_flag,
                    "-x",
                    "-f",
                    archive,
                ] + exclude_flags,
                delimiter = " ",
            ).relative_to(output),
        ],
        is_executable = True,
        allow_args = True,
    )
    ctx.actions.run(cmd_args(["/bin/sh", script])
        .hidden(hidden + exclude_hidden + [archive, output.as_output()]), category = "http_archive", local_only = local_only)

    return [DefaultInfo(default_outputs = [output])]
