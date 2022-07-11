FooInfo = provider(fields = ["args", "out"])

def _simple_write_impl(ctx):
    out = ctx.actions.write("out.txt", "contents")
    args = cmd_args([out])
    return [
        FooInfo(args = args, out = out),
        DefaultInfo(default_outputs = [out]),
    ]

def _write_file_impl(ctx):
    if ctx.attr.name == "uses_declared_output":
        declared = ctx.actions.declare_output(ctx.attr.out)
        output = ctx.actions.write(declared, ctx.attr.content)
        return [DefaultInfo(default_outputs = [output])]
    elif ctx.attr.name == "uses_declared_output_as_output":
        declared = ctx.actions.declare_output(ctx.attr.out)
        output = ctx.actions.write(declared.as_output(), ctx.attr.content)
        return [DefaultInfo(default_outputs = [output])]
    elif ctx.attr.name == "declares_output":
        output = ctx.actions.write(ctx.attr.out, ctx.attr.content)
        return [DefaultInfo(default_outputs = [output])]
    elif ctx.attr.name == "is_executable":
        output = ctx.actions.write(ctx.attr.out, ctx.attr.content, is_executable = True)
    elif ctx.attr.name == "writes_array_of_commands":
        cmd = [ctx.attr.dep[FooInfo].out, ctx.attr.content]
        output = ctx.actions.write(ctx.attr.out, cmd)
    elif ctx.attr.name == "writes_command_lines":
        cmd = [ctx.attr.dep[FooInfo].out, ctx.attr.content]
        output = ctx.actions.write(ctx.attr.out, cmd_args(cmd))
    elif ctx.attr.name == "writes_frozen_command_lines":
        output = ctx.actions.write(ctx.attr.out, ctx.attr.dep[FooInfo].args)
    elif ctx.attr.name == "fails_on_invalid_contents":
        output = ctx.actions.write(ctx.attr.out, {})
    elif ctx.attr.name == "fails_on_invalid_output":
        output = ctx.actions.write([], ctx.attr.content)
    else:
        fail("invalid test")
    return [DefaultInfo(default_outputs = [output])]

write_file = rule(
    impl = _write_file_impl,
    attrs = {
        "content": attr.string(default = "some content"),
        "dep": attr.option(attr.dep(providers = [FooInfo])),
        "exe": attr.bool(default = False),
        "out": attr.string(default = "out.txt"),
    },
)

simple_write = rule(
    impl = _simple_write_impl,
    attrs = {
    },
)
