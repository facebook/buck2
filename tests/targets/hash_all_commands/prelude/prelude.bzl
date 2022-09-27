def _echo_test_impl(ctx):
    app = ctx.actions.declare_output("app")

    if ctx.attrs.local_only == "true":
        local_only = True
    elif ctx.attrs.local_only == "false":
        local_only = False
    else:
        fail("Invalid local only: {}".format(ctx.attrs.local_only))

    ctx.actions.run(
        [
            "sh",
            "-c",
            'echo "$1" > "$2"',
            "--",
            ctx.attrs.param,
            app.as_output(),
        ],
        env = {
            "local_only": str(local_only),
        },
        category = ctx.attrs.category,
        local_only = local_only,
    )

    return [
        DefaultInfo(
            default_outputs = [app],
        ),
    ]

echo_test = rule(
    attrs = {
        "category": attrs.string(),
        "local_only": attrs.string(),
        # NOTE: This does NOT get ignored.
        "param": attrs.string(),
        # NOTE: This gets ignored.
        "seed": attrs.string(),
    },
    impl = _echo_test_impl,
)

def _symlink_test_impl(ctx):
    data = ctx.actions.write("data", ctx.attrs.param)
    out = ctx.actions.declare_output("out/symlink")

    # NOTE: "data" and "out" will be next to each other here, hence ../data
    # NOTE: We use local_only since RE actually returns files for symlinks.
    ctx.actions.run(
        cmd_args(["ln", "-s", "../data", out.as_output()]).hidden(data),
        category = ctx.attrs.category,
        local_only = True,
    )

    return [
        DefaultInfo(default_outputs = [out]),
    ]

symlink_test = rule(
    attrs = {
        "category": attrs.string(),
        # NOTE: This gets ignored by the executed action (but not the data it links to)
        "param": attrs.string(),
    },
    impl = _symlink_test_impl,
)
