def _not_hermetic_action_impl(ctx):
    dep = ctx.attr.dep[DefaultInfo].default_outputs[0]
    sentinel = ctx.attr.sentinel
    out = ctx.actions.declare_output("out")

    ctx.actions.run(
        cmd_args([
            "sh",
            "-c",
            'test -f "$(cat "$1")" && touch "$2"',
            "--",
            sentinel,
            out.as_output(),
        ]).hidden(dep),
        local_only = True,
        category = "test",
    )

    return [DefaultInfo(default_outputs = [out])]

not_hermetic_action = rule(
    impl = _not_hermetic_action_impl,
    attrs = {
        "dep": attr.dep(),
        "sentinel": attr.source(),
    },
)

def _rerun_action_impl(ctx):
    src = ctx.attr.src
    out = ctx.actions.declare_output("out")

    ctx.actions.run(
        cmd_args([
            "touch",
            out.as_output(),
        ]).hidden(src),
        category = "test",
    )

    return [DefaultInfo(default_outputs = [out])]

rerun_action = rule(
    impl = _rerun_action_impl,
    attrs = {
        "src": attr.source(),
    },
)
