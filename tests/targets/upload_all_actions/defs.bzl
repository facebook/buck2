def _cp_impl(ctx: "context"):
    out = ctx.actions.declare_output("out")
    ctx.actions.run(["cp", ctx.attrs.src, out.as_output()], category = "cp", local_only = True)

    return [
        DefaultInfo(default_outputs = [out]),
    ]

cp = rule(
    impl = _cp_impl,
    attrs = {"src": attrs.source()},
)
