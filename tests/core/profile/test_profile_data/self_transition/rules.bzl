# @nolint

def _zzz(ctx):
    _ignore = ctx
    return [DefaultInfo()]

def _transition_impl(platform, refs):
    _ignore = (platform, refs)
    return PlatformInfo(
        label = "<fgfgf>",
        configuration = ConfigurationInfo(
            constraints = {},
            values = {},
        ),
    )

_tr = transition(
    impl = _transition_impl,
    refs = {},
)

zzz = rule(
    impl = _zzz,
    attrs = {},
    cfg = _tr,
)
