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

def get_cxx_platform_info(ctx: "context") -> "CxxPlatformInfo":
    apple_toolchain = getattr(ctx.attr, "_apple_toolchain", None)
    if apple_toolchain:
        return apple_toolchain[AppleToolchainInfo].cxx_platform_info
    return ctx.attr._cxx_toolchain[CxxPlatformInfo]

def get_cxx_toolchain_info(ctx: "context") -> "CxxToolchainInfo":
    apple_toolchain = getattr(ctx.attr, "_apple_toolchain", None)
    if apple_toolchain:
        return apple_toolchain[AppleToolchainInfo].cxx_toolchain_info
    return ctx.attr._cxx_toolchain[CxxToolchainInfo]
