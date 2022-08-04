load("@fbcode//buck2/prelude/utils:platform_flavors_util.bzl", "by_platform")
load(":cxx_context.bzl", "get_cxx_platform_info")

def cxx_by_platform(ctx: "context", xs: [(str.type, "_a")]) -> ["_a"]:
    cxx_platform_info = get_cxx_platform_info(ctx)
    return by_platform([cxx_platform_info.name], xs)
