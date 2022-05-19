load("//buck2/prelude/cxx:cxx_toolchain_types.bzl", "DistLtoToolsInfo")

def _impl(ctx):
    return [
        DefaultInfo(),
        DistLtoToolsInfo(
            planner = ctx.attr.planner[RunInfo],
            prepare = ctx.attr.prepare[RunInfo],
            opt = ctx.attr.opt[RunInfo],
            copy = ctx.attr.copy[RunInfo],
        ),
    ]

dist_lto_tools = rule(
    implementation = _impl,
    attrs = {
        "copy": attr.dep(),
        "opt": attr.dep(),
        "planner": attr.dep(),
        "prepare": attr.dep(),
    },
)
