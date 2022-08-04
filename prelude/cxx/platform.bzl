load("@fbcode//buck2/prelude/utils:platform_flavors_util.bzl", "by_platform")
load(
    ":cxx_context.bzl",
    "CxxContext",  # @unused Used as type
)

def cxx_by_platform(cxx_context: CxxContext.type, xs: [(str.type, "_a")]) -> ["_a"]:
    cxx_platform_info = cxx_context.cxx_platform_info
    return by_platform([cxx_platform_info.name], xs)
