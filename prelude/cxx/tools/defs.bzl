load("@prelude//cxx:cxx_toolchain_types.bzl", "CxxPlatformInfo", "CxxToolchainInfo")
load("@prelude//cxx:omnibus.bzl", "OmnibusEnvironment")

def _omnibus_environment_impl(ctx: "context"):
    return [DefaultInfo(), OmnibusEnvironment(
        exclusions = {e.raw_target(): None for e in ctx.attrs.exclusions},
        roots = {e.raw_target(): None for e in ctx.attrs.roots},
        enable_explicit_roots = ctx.attrs.enable_explicit_roots,
    )]

omnibus_environment = rule(impl = _omnibus_environment_impl, attrs = {
    "enable_explicit_roots": attrs.bool(),
    "exclusions": attrs.list(attrs.label(), default = []),
    "labels": attrs.list(attrs.string(), default = []),
    "roots": attrs.list(attrs.label(), default = []),
    "_cxx_toolchain": attrs.toolchain_dep(default = "toolchains//:cxx", providers = [CxxToolchainInfo, CxxPlatformInfo]),
})
