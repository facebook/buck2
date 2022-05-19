def _test_impl(ctx):
    app = ctx.actions.declare_output("app")

    ctx.actions.run(
        [
            "touch",
            app.as_output(),
        ],
        category = "test",
    )

    return [
        DefaultInfo(
            default_outputs = [app],
        ),
    ]

test = rule(
    attrs = {
        # NOTE: This gets ignored
        "seed": attr.string(default = ""),
    },
    implementation = _test_impl,
)
