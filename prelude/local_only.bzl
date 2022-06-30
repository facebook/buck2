load("@fbcode//buck2/prelude/cxx:cxx_context.bzl", "get_cxx_toolchain_info")

def _is_core_cool(ctx: "context"):
    return "is_core_tool" in ctx.attr.labels

def link_cxx_binary_locally(ctx: "context", cxx_toolchain: ["CxxToolchainInfo", None] = None) -> bool.type:
    if _is_core_cool(ctx):
        return False
    if not cxx_toolchain:
        cxx_toolchain = get_cxx_toolchain_info(ctx)
    return cxx_toolchain.linker_info.link_binaries_locally

def package_python_locally(ctx: "context", python_toolchain: "PythonToolchainInfo") -> bool.type:
    if _is_core_cool(ctx):
        return False
    return python_toolchain.build_standalone_binaries_locally
