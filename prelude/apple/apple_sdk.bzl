load("@fbcode//buck2/prelude/apple:apple_toolchain_types.bzl", "AppleToolchainInfo")

def get_apple_sdk_name(ctx: "context") -> str.type:
    """
    Get the SDK defined on the toolchain.
    Will throw if the `_apple_toolchain` is not present.
    """
    return ctx.attrs._apple_toolchain[AppleToolchainInfo].sdk_name
