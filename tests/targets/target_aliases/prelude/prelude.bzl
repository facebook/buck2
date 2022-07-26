def _test_impl(_ctx):
    return [
        DefaultInfo(),
    ]

test = rule(
    impl = _test_impl,
    attrs = {
    },
)
