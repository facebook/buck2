def _failing_binary_impl(ctx: "context"):
    out = ctx.actions.declare_output("out.txt")
    args = cmd_args(["/bin/sh", "-c"])
    args.add(cmd_args(out.as_output(), format = "touch '{}' && false"))
    if ctx.attr.src != None:
        args.add(ctx.attr.src)
    ctx.actions.run(args, category = "bin_false")
    return [DefaultInfo(default_outputs = [out])]

failing_binary = rule(
    implementation = _failing_binary_impl,
    attrs = {"src": attr.option(attr.source(allow_directory = True))},
)
