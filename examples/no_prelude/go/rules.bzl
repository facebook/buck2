load("//toolchain.bzl", "GoCompilerInfo")

def _go_binary_impl(ctx: "context") -> ["provider"]:
    sources = ctx.attr.srcs
    out = ctx.actions.declare_output("main")

    cmd = cmd_args([ctx.attr.toolchain[GoCompilerInfo].compiler_path, "build", "-o", out.as_output()] + sources)

    ctx.actions.run(cmd, category = "compile")

    return [
        DefaultInfo(default_outputs = [out]),
        RunInfo(args = cmd_args(out)),
    ]

go_binary = rule(
    implementation = _go_binary_impl,
    attrs = {
        "deps": attr.list(attr.dep()),
        "srcs": attr.list(attr.source()),
        "toolchain": attr.dep(),
    },
)
