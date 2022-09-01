def _test_impl(ctx):
    return [
        DefaultInfo(default_outputs = [ctx.actions.write("build", "")]),
        RunInfo(args = cmd_args([
            ctx.actions.write("run", ""),
        ])),
        ExternalRunnerTestInfo(
            type = "custom",
            command = [cmd_args([
                ctx.actions.write("test", ""),
            ])],
            env = {},
            labels = [],
            contacts = [],
        ),
    ]

test = rule(
    impl = _test_impl,
    attrs = {},
)
