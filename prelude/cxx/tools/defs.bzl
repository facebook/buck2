load("@prelude//cxx:cxx_toolchain_types.bzl", "CxxPlatformInfo", "CxxToolchainInfo")
load("@prelude//cxx:omnibus.bzl", "OmnibusEnvironment", "create_dummy_omnibus")

def _omnibus_environment_impl(ctx: "context"):
    omnibus = create_dummy_omnibus(ctx)

    return [DefaultInfo(), OmnibusEnvironment(
        dummy_omnibus = omnibus,
        exclusions = {e.raw_target(): None for e in ctx.attrs.exclusions},
        roots = {e.raw_target(): None for e in ctx.attrs.roots},
        enable_explicit_roots = ctx.attrs.enable_explicit_roots,
        prefer_stripped_objects = ctx.attrs.prefer_stripped_native_objects,
        shared_root_ld_flags = ctx.attrs.shared_root_ld_flags,
    )]

omnibus_environment = rule(impl = _omnibus_environment_impl, attrs = {
    "enable_explicit_roots": attrs.bool(),
    "exclusions": attrs.list(attrs.label(), default = []),
    "labels": attrs.list(attrs.string(), default = []),
    # Same name as the Python attr
    "prefer_stripped_native_objects": attrs.bool(),
    "roots": attrs.list(attrs.label(), default = []),
    "shared_root_ld_flags": attrs.list(attrs.arg(), default = []),
    "_cxx_toolchain": attrs.toolchain_dep(default = "toolchains//:cxx", providers = [CxxToolchainInfo, CxxPlatformInfo]),
})
