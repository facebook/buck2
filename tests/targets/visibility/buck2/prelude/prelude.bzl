def _impl(_ctx):
    return [DefaultInfo()]

foo_target = rule(
    impl = _impl,
    attrs = {
        "actual": attrs.option(attrs.dep()),
    },
)
