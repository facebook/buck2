def _copy_file_impl(ctx):
    test = ctx.attrs.test or ctx.attrs.name
    if test == "uses_declared_output":
        declared = ctx.actions.declare_output(ctx.attrs.out)
        output = ctx.actions.copy_file(declared, ctx.attrs.src)
        return [DefaultInfo(default_outputs = [output])]
    elif test == "uses_declared_output_as_output":
        declared = ctx.actions.declare_output(ctx.attrs.out)
        output = ctx.actions.copy_file(declared.as_output(), ctx.attrs.src)
        return [DefaultInfo(default_outputs = [output])]
    elif test == "declares_output":
        output = ctx.actions.copy_file(ctx.attrs.out, ctx.attrs.src)
        return [DefaultInfo(default_outputs = [output])]
    elif test == "fails_on_invalid_src":
        ctx.actions.copy_file(ctx.attrs.out, [])
        fail("should fail in copy() function")
    elif test == "fails_on_invalid_dest":
        ctx.actions.copy_file([], ctx.attrs.src)
        fail("should fail in copy() function")
    else:
        fail("invalid test")

copy_file = rule(
    impl = _copy_file_impl,
    attrs = {
        "out": attr.string(),
        "src": attr.source(),
        "test": attr.option(attr.string()),
    },
)
