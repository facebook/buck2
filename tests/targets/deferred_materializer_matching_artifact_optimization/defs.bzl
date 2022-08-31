def _copy_impl(ctx):
    out = ctx.actions.declare_output("action_output")
    ctx.actions.run(
        cmd_args(["cp", ctx.attrs.src, out.as_output()]).hidden(ctx.attrs.hidden),
        category = "cp",
    )

    return [DefaultInfo(default_outputs = [out])]

copy = rule(
    impl = _copy_impl,
    attrs = {
        "hidden": attrs.source(),
        "src": attrs.source(),
    },
)
