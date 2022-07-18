load("//toolchain.bzl", "CxxCompilerInfo")

CxxLibraryInfo = provider(fields = ["headers", "objects", "include_folders"])

def _cpp_binary_impl(ctx: "context") -> ["provider"]:
    sources = ctx.attr.srcs
    out = ctx.actions.declare_output("main")

    cmd = cmd_args([ctx.attr.toolchain[CxxCompilerInfo].compiler_path, "-o", out.as_output()] + sources)

    ctx.actions.run(cmd, category = "compile")

    return [
        DefaultInfo(default_outputs = [out]),
        RunInfo(args = cmd_args(out)),
    ]

cpp_binary = rule(
    implementation = _cpp_binary_impl,
    attrs = {
        "srcs": attr.list(attr.source()),
        "headers": attr.list(attr.source()),
        "deps": attr.list(attr.dep()),
        "toolchain": attr.dep(),
    },
)

def _cpp_library_impl(ctx: "context") -> ["provider"]:
    sources = ctx.attr.srcs
    headers = ctx.attr.headers
    out = ctx.actions.declare_output("lib.so")

    cmd = cmd_args(["clang++", "-shared", "-undefined", "dynamic_lookup", "-o", out.as_output()] + sources)

    ctx.actions.run(cmd, category = "compile")

    return [DefaultInfo(default_outputs = [out]), CxxLibraryInfo(objects = [out], headers = headers)]

cpp_library = rule(
    implementation = _cpp_library_impl,
    attrs = {
        "srcs": attr.list(attr.source()),
        "headers": attr.list(attr.source()),
        "deps": attr.list(attr.dep()),
    },
)
