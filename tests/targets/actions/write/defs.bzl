FooInfo = provider(fields = ["args", "out"])

def _simple_write_impl(ctx):
    out = ctx.actions.write("out.txt", "contents")
    args = cmd_args([out])
    return [
        FooInfo(args = args, out = out),
        DefaultInfo(default_outputs = [out]),
    ]

def _write_file_impl(ctx):
    if ctx.attrs.name == "uses_declared_output":
        declared = ctx.actions.declare_output(ctx.attrs.out)
        output = ctx.actions.write(declared, ctx.attrs.content)
    elif ctx.attrs.name == "uses_declared_output_as_output":
        declared = ctx.actions.declare_output(ctx.attrs.out)
        output = ctx.actions.write(declared.as_output(), ctx.attrs.content)
    elif ctx.attrs.name == "declares_output":
        output = ctx.actions.write(ctx.attrs.out, ctx.attrs.content)
    elif ctx.attrs.name == "is_executable":
        output = ctx.actions.write(ctx.attrs.out, ctx.attrs.content, is_executable = True)
    elif ctx.attrs.name == "writes_array_of_commands":
        cmd = [ctx.attrs.dep[FooInfo].out, ctx.attrs.content]
        output = ctx.actions.write(ctx.attrs.out, cmd)
    elif ctx.attrs.name == "writes_command_lines":
        cmd = [ctx.attrs.dep[FooInfo].out, ctx.attrs.content]
        output = ctx.actions.write(ctx.attrs.out, cmd_args(cmd))
    elif ctx.attrs.name == "writes_frozen_command_lines":
        output = ctx.actions.write(ctx.attrs.out, ctx.attrs.dep[FooInfo].args)
    elif ctx.attrs.name == "with_inputs_and_copy":
        output1 = ctx.actions.write("intermediate.txt", ctx.attrs.content)
        output2 = ctx.actions.declare_output(ctx.attrs.out)

        # Create script with output1 as its associated artifact
        cmd = cmd_args(output1, format = "import sys; fp1=open('{}','r'); all=fp1.read(); fp2=open(sys.argv[1], 'w'); fp2.write(all);")

        # Replace \ with \\ for Windows compatibility
        cmd = cmd.replace_regex("\\\\\\b", "\\\\")
        script = ctx.actions.write(
            "script.py",
            [cmd],
            with_inputs = True,
        )

        # Read output1 and write back into output2. Output1 should be included as an associated artifact here so we do not need to add it as hidden
        cmd = cmd_args(["python3", script, output2.as_output()])
        ctx.actions.run(cmd, category = "test")
        return [DefaultInfo(default_outputs = [output2])]
    elif ctx.attrs.name == "fails_on_invalid_contents":
        output = ctx.actions.write(ctx.attrs.out, {})
    elif ctx.attrs.name == "fails_on_invalid_output":
        output = ctx.actions.write([], ctx.attrs.content)
    else:
        fail("invalid test")
    return [DefaultInfo(default_outputs = [output])]

write_file = rule(
    impl = _write_file_impl,
    attrs = {
        "content": attrs.string(default = "some content"),
        "dep": attrs.option(attrs.dep(providers = [FooInfo])),
        "exe": attrs.bool(default = False),
        "out": attrs.string(default = "out.txt"),
    },
)

simple_write = rule(
    impl = _simple_write_impl,
    attrs = {
    },
)
