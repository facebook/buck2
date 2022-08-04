load("@fbcode//buck2/prelude/apple:apple_toolchain_types.bzl", "AppleToolchainInfo")
load("@fbcode//buck2/prelude/cxx:cxx_toolchain_types.bzl", "CxxPlatformInfo", "CxxToolchainInfo")

# The functions below allow the Cxx rules to find toolchain providers
# from different rule contexts. For example, the Cxx functions are
# re-used by non-`cxx_` rules (e.g., the Apple rules) but the toolchain
# setup on such rules might/would be different.
#
# The functions should be used throughout the Cxx rules to get
# the required providers instead of going via the `_cxx_toolchain`
# field of the `ctx`.
#
# In an ideal world, we would have been injecting all these from
# the top level but as part of the transition to support
# `apple_toolchain`, we want to make progress now.

# A context for declaration of actions requiring Cxx toolchain info
# that doesn't also carry along the attrs of the full context
CxxContext = record(
    actions = "actions",
    label = "label",
    cxx_platform_info = field("CxxPlatformInfo"),
    cxx_toolchain_info = field("CxxToolchainInfo"),

    # Library Attrs
    supported_platforms_regex = field(["string", None], None),
)

def ctx_to_cxx_context(ctx: "context") -> CxxContext.type:
    cxx_context = {
        "actions": ctx.actions,
        "cxx_platform_info": get_cxx_platform_info(ctx),
        "cxx_toolchain_info": get_cxx_toolchain_info(ctx),
        "label": ctx.label,
    }

    cxx_context["supported_platforms_regex"] = getattr(ctx.attrs, "supported_platforms_regex", None)

    return CxxContext(**cxx_context)

def get_cxx_platform_info(ctx: "context") -> CxxPlatformInfo.type:
    apple_toolchain = getattr(ctx.attrs, "_apple_toolchain", None)
    if apple_toolchain:
        return apple_toolchain[AppleToolchainInfo].cxx_platform_info
    return ctx.attrs._cxx_toolchain[CxxPlatformInfo]

def get_cxx_toolchain_info(ctx: "context") -> CxxToolchainInfo.type:
    apple_toolchain = getattr(ctx.attrs, "_apple_toolchain", None)
    if apple_toolchain:
        return apple_toolchain[AppleToolchainInfo].cxx_toolchain_info
    return ctx.attrs._cxx_toolchain[CxxToolchainInfo]
