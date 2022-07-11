def _impl(_ctx):
    return [DefaultInfo()]

foo_target = rule(
    impl = _impl,
    attrs = {
        "actual": attr.option(attr.dep()),
    },
)
