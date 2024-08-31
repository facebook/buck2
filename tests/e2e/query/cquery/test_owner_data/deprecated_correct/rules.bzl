# @nolint

def _genrule(ctx):
    _ignore = ctx
    fail("not needed in this test")

genrule = rule(
    impl = _genrule,
    attrs = {
        "cmd": attrs.arg(),
        "out": attrs.string(),
    },
)

def _sh_binary(ctx):
    _ignore = ctx
    fail("not needed in this test")

sh_binary = rule(
    impl = _sh_binary,
    attrs = {
        "main": attrs.source(),
    },
)
