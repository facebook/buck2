load("@prelude//:cache_mode.bzl", "CacheModeInfo")

def _cache_mode_impl(ctx):
    allow_cache_uploads = all([
        ctx.attrs.cache_mode == "readwrite",
        ctx.attrs.schedule_type in ["continuous", "master"],
        ctx.attrs.sandcastle_alias in [
            "buck2",
            "build_infra_buck2_linux_buckv2-buck",
            "build_infra_buck2_mac-buck",
            "build_infra_buck2_e2e_linux_buckv2-buck",
        ],
    ])

    return [
        DefaultInfo(),
        CacheModeInfo(allow_cache_uploads = allow_cache_uploads),
    ]

cache_mode = rule(
    attrs = {
        "cache_mode": attrs.option(attrs.string()),
        "sandcastle_alias": attrs.option(attrs.string()),
        "schedule_type": attrs.option(attrs.string()),
    },
    impl = _cache_mode_impl,
)
