def filegroup_impl(ctx):
    """
    Creates a directory that contains links to the list of srcs

    The output is a directory that uses `name` for its name, and each symlink
    is based on the `short_path` for the provided `src`.
    """

    if type(ctx.attrs.srcs) == type({}):
        srcs = ctx.attrs.srcs
    else:
        srcs = {src.short_path: src for src in ctx.attrs.srcs}

    # It seems that buck1 always copies, and that's important for Python rules
    output = ctx.actions.copied_dir(ctx.label.name, srcs)
    return [DefaultInfo(default_outputs = [output])]
