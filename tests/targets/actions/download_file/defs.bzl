def _test_impl(ctx: "context"):
    output = ctx.actions.download_file(ctx.label.name, ctx.attrs.url, sha1 = ctx.attrs.sha1, is_deferrable = False)
    return [
        DefaultInfo(default_outputs = [output]),
    ]

test = rule(
    impl = _test_impl,
    attrs = {
        "sha1": attrs.string(),
        "url": attrs.string(),
    },
)
