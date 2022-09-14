load("@prelude//:cache_mode.bzl", "CacheModeInfo")

def _is_apple_sandcastle_alias(sandcastle_alias: [None, str.type]):
    if not sandcastle_alias or not "buck2" in sandcastle_alias:
        return False

    # TODO(T131813120): Support all Apple SC jobs
    sandcastle_job_app_names = ["fbios", "igios", "messenger"]
    for app_name in sandcastle_job_app_names:
        if app_name in sandcastle_alias:
            return True
    return False

def _allow_cache_for_apple(ctx: "context") -> bool.type:
    return all([
        ctx.attrs.cache_mode == "readwrite",
        ctx.attrs.schedule_type in ["continuous", "continuous_stable"],
        _is_apple_sandcastle_alias(ctx.attrs.sandcastle_alias),
    ])

def _allow_cache_for_fbcode(ctx: "context") -> bool.type:
    sandcastle_alias = ctx.attrs.sandcastle_alias or ""

    if not sandcastle_alias.endswith("_buckv2-buck"):
        return False

    return all([
        ctx.attrs.cache_mode == "readwrite",
        ctx.attrs.schedule_type in ["continuous", "master"],
        any([
            ctx.attrs.sandcastle_alias in [
                "build_infra_buck2_linux_buckv2-buck",
                "build_infra_buck2_mac-buck",
                "build_infra_buck2_e2e_linux_buckv2-buck",
            ],
            # Roll out to (1/16)*(5/16) contbuilds. The hash is a hex string so
            # we look at the first 2 chars here.
            sha256(ctx.attrs.sandcastle_alias) < "26",
        ]),
    ])

def _allow_cache_for_android(ctx: "context") -> bool.type:
    return all([
        ctx.attrs.cache_mode == "readwrite",
        ctx.attrs.schedule_type in ["continuous", "continuous_stable"],
        ctx.attrs.sandcastle_alias in [
            "automation_fbandroid_exo_native_buck2",
        ],
    ])

def _cache_mode_impl(ctx):
    allow_cache_uploads = any([
        _allow_cache_for_fbcode(ctx),
        _allow_cache_for_apple(ctx),
        _allow_cache_for_android(ctx),
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
