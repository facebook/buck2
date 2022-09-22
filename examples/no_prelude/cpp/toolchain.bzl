CxxCompilerInfo = provider(
    doc = "Information about how to invoke the cpp compiler.",
    fields = ["compiler_path", "include_directories", "lib_directories"],
)

def _cpp_local_toolchain_impl(ctx):
    return [DefaultInfo(), CxxCompilerInfo(compiler_path = ctx.attrs.command)]

cpp_local_toolchain = rule(
    impl = _cpp_local_toolchain_impl,
    is_toolchain_rule = True,
    attrs = {
        "command": attrs.string(),
    },
)
