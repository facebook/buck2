# @nolint

def _genrule(ctx):
    _ignore = ctx
    fail("Not needed in test")

genrule = rule(
    impl = _genrule,
    attrs = {
        "bash": attrs.arg(),
        "out": attrs.string(),
        "srcs": attrs.list(attrs.source()),
    },
)
