load("@prelude//cxx:cxx_toolchain_types.bzl", "DistLtoToolsInfo")

def _impl(ctx):
    return [
        DefaultInfo(),
        DistLtoToolsInfo(
            planner = ctx.attrs.planner[RunInfo],
            prepare = ctx.attrs.prepare[RunInfo],
            opt = ctx.attrs.opt[RunInfo],
            copy = ctx.attrs.copy[RunInfo],
        ),
    ]

dist_lto_tools = rule(
    impl = _impl,
    attrs = {
        "copy": attrs.dep(),
        "opt": attrs.dep(),
        "planner": attrs.dep(),
        "prepare": attrs.dep(),
    },
)
