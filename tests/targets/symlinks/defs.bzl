def _cp_impl(ctx: "context"):
    out = ctx.actions.declare_output("out")
    ctx.actions.run(["cp", cmd_args(ctx.attrs.src, format = "{}/link/file"), out.as_output()], category = "cp")

    return [
        DefaultInfo(default_outputs = [out]),
    ]

cp = rule(
    impl = _cp_impl,
    attrs = {"src": attrs.source(allow_directory = True)},
)
