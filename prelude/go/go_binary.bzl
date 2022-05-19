load(":compile.bzl", "compile", "get_filtered_srcs")
load(":link.bzl", "link")

def go_binary_impl(ctx: "context") -> ["provider"]:
    lib = compile(ctx, "main", get_filtered_srcs(ctx, ctx.attr.srcs), deps = ctx.attr.deps)
    bin = link(ctx, lib, deps = ctx.attr.deps)
    return [
        DefaultInfo(default_outputs = [bin]),
        RunInfo(args = cmd_args(bin)),
    ]
