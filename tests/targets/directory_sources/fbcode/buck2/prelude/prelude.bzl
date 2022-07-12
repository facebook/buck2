def _test_impl(ctx):
    return [
        DefaultInfo(default_outputs = [ctx.attrs.src]),
    ]

test = rule(
    impl = _test_impl,
    attrs = {
        "src": attr.source(allow_directory = True),
    },
)
