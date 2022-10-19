def _simple_impl(ctx):
    out = ctx.actions.declare_output("out")

    ctx.actions.run(
        [
            "python3",
            "-c",
            "import sys; open(sys.argv[1], 'w')",
            out.as_output(),
        ],
        category = "write",
    )

    return [
        DefaultInfo(
            default_outputs = [out],
        ),
    ]

simple = rule(
    attrs = {},
    impl = _simple_impl,
)
