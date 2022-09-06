def _failing_binary_impl(ctx: "context"):
    out = ctx.actions.declare_output("out.txt")
    args = cmd_args(["/bin/sh", "-c"])
    args.add(cmd_args(out.as_output(), format = "touch '{}' && false"))
    if ctx.attrs.src != None:
        args.add(ctx.attrs.src)
    ctx.actions.run(args, category = "bin_false")
    return [DefaultInfo(default_outputs = [out])]

failing_binary = rule(
    impl = _failing_binary_impl,
    attrs = {"src": attrs.option(attrs.source(allow_directory = True))},
)
