# TODO(aherrmann) without the `@` I get the error "Unable to parse import spec. Expected format `(@<cell>)//package/name:filename.bzl` or `:filename.bzl`. Got `toolchains//:defs.bzl`"
load("@toolchains//:defs.bzl", "CCompilerInfo")

def _c_binary_impl(ctx: "context") -> ["provider"]:
    sources = ctx.attrs.srcs
    out = ctx.actions.declare_output(ctx.label.name)

    cmd = cmd_args([ctx.attrs.toolchain[CCompilerInfo].compiler_command, "-o", out.as_output()] + sources)

    ctx.actions.run(cmd, category = "compile")

    return [
        DefaultInfo(default_outputs = [out]),
        RunInfo(args = cmd_args(out)),
    ]

c_binary = rule(
    impl = _c_binary_impl,
    attrs = {
        "deps": attrs.list(attrs.dep()),
        "srcs": attrs.list(attrs.source()),
        "toolchain": attrs.toolchain_dep(),
    },
)
