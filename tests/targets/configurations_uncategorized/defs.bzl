SelectTesterInfo = provider(fields = ["values"])

def _select_tester_impl(ctx):
    return [
        DefaultInfo(),
        SelectTesterInfo(values = ctx.attr.values),
    ]

select_tester = rule(
    impl = _select_tester_impl,
    attrs = {
        "values": attr.dict(attr.string(), attr.string()),
    },
)
