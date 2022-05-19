load("@fbcode//buck2/prelude/cxx:cxx_context.bzl", "get_cxx_toolchain_info")

def apple_strip_args(ctx: "context") -> "cmd_args":
    cxx_toolchain_info = get_cxx_toolchain_info(ctx)
    flags = cxx_toolchain_info.strip_flags_info.strip_non_global_flags
    return cmd_args(flags) if flags != None else ctx.action.args(["-x", "-T"])
