def _test_impl(_ctx):
    return [
        DefaultInfo(),
    ]

test = rule(
    implementation = _test_impl,
    attrs = {
    },
)
