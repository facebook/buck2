SelectTesterInfo = provider(fields = ["values"])

def _select_tester_impl(ctx):
    return [
        DefaultInfo(),
        SelectTesterInfo(values = ctx.attrs.values),
    ]

select_tester = rule(
    impl = _select_tester_impl,
    attrs = {
        "values": attrs.dict(attrs.string(), attrs.string()),
    },
)
