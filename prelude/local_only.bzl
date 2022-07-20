load("@fbcode//buck2/prelude/cxx:cxx_context.bzl", "get_cxx_toolchain_info")

def _is_core_tool(ctx: "context"):
    return "is_core_tool" in ctx.attrs.labels

def link_cxx_binary_locally(ctx: "context", cxx_toolchain: ["CxxToolchainInfo", None] = None) -> bool.type:
    # core tools are linked on RE because they are a) small enough to do so and
    # b) don't get build stamping so they do cache correctly.
    if _is_core_tool(ctx):
        return False
    if not cxx_toolchain:
        cxx_toolchain = get_cxx_toolchain_info(ctx)
    return cxx_toolchain.linker_info.link_binaries_locally

def package_python_locally(ctx: "context", python_toolchain: "PythonToolchainInfo") -> bool.type:
    if _is_core_tool(ctx):
        return False
    return python_toolchain.build_standalone_binaries_locally
