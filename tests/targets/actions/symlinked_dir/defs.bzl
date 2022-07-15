def write_file_impl(ctx):
    out = ctx.actions.write(ctx.attrs.out, ctx.attrs.contents)
    return [DefaultInfo(default_outputs = [out])]

def symlink_files_impl(ctx):
    srcs = {
        src.short_path: src
        for src in ctx.attrs.srcs
    }

    # Also make sure that linking to a new location works properly
    srcs.update({
        "subdir/{}.suffix".format(src.short_path): src
        for src in ctx.attrs.srcs
    })
    out = ctx.actions.symlinked_dir("out", srcs)
    return [DefaultInfo(default_outputs = [out])]

write_file = rule(
    impl = write_file_impl,
    attrs = {
        "contents": attrs.string(),
        "out": attrs.string(),
    },
)

symlink_files = rule(
    impl = symlink_files_impl,
    attrs = {
        "srcs": attrs.list(attrs.source()),
    },
)
