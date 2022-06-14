def _test_impl(ctx):
    return [
        DefaultInfo(default_outputs = [ctx.attr.src]),
    ]

test = rule(
    implementation = _test_impl,
    attrs = {
        "src": attr.source(allow_directory = True),
    },
)
