def _noop(_ctx):
    return [DefaultInfo()]

noop = rule(
    impl = _noop,
    attrs = {},
)
