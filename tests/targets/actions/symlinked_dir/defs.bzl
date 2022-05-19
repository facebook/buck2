def write_file_impl(ctx):
    out = ctx.actions.write(ctx.attr.out, ctx.attr.contents)
    return [DefaultInfo(default_outputs = [out])]

def symlink_files_impl(ctx):
    srcs = {
        src.short_path: src
        for src in ctx.attr.srcs
    }

    # Also make sure that linking to a new location works properly
    srcs.update({
        "subdir/{}.suffix".format(src.short_path): src
        for src in ctx.attr.srcs
    })
    out = ctx.actions.symlinked_dir("out", srcs)
    return [DefaultInfo(default_outputs = [out])]

write_file = rule(
    implementation = write_file_impl,
    attrs = {
        "contents": attr.string(),
        "out": attr.string(),
    },
)

symlink_files = rule(
    implementation = symlink_files_impl,
    attrs = {
        "srcs": attr.list(attr.source()),
    },
)
