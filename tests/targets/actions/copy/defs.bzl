def _copy_file_impl(ctx):
    test = ctx.attr.test or ctx.attr.name
    if test == "uses_declared_output":
        declared = ctx.actions.declare_output(ctx.attr.out)
        output = ctx.actions.copy(ctx.attr.src, declared)
        return [DefaultInfo(default_outputs = [output])]
    elif test == "uses_declared_output_as_output":
        declared = ctx.actions.declare_output(ctx.attr.out)
        output = ctx.actions.copy(ctx.attr.src, declared.as_output())
        return [DefaultInfo(default_outputs = [output])]
    elif test == "declares_output":
        output = ctx.actions.copy(ctx.attr.src, ctx.attr.out)
        return [DefaultInfo(default_outputs = [output])]
    elif test == "fails_on_invalid_src":
        ctx.actions.copy([], ctx.attr.out)
        fail("should fail in copy() function")
    elif test == "fails_on_invalid_dest":
        ctx.actions.copy(ctx.attr.src, [])
        fail("should fail in copy() function")
    else:
        fail("invalid test")

copy_file = rule(
    implementation = _copy_file_impl,
    attrs = {
        "out": attr.string(),
        "src": attr.source(),
        "test": attr.option(attr.string()),
    },
)
