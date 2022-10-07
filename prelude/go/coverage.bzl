load(":toolchain.bzl", "GoToolchainInfo")

GoCoverageMode = enum(
    "set",
    "count",
    "atomic",
)

# The result of runnin `go tool cover` on the input sources.
GoCoverResult = record(
    # All sources after annotating non-`_test.go` sources.  This will be a
    # combination of the original `*_test.go` sources and the annotated non-
    # `*_test.go` sources.
    srcs = field(["artifact"], []),
    # Coverage variables we used when annotating non-test sources.
    variables = field({str.type: "artifact"}, {}),
)

def _is_test(src: "artifact") -> bool.type:
    """
    The heuristic Go uses to discern test sources from non-test sources.
    """

    return src.short_path.endswith("_test.go")

def _var(label: "label", src: "artifact"):
    """
    Generate a (likely) unique variable name for the source (ported from v1).
    """

    return "Var_" + str(abs(hash(str(label.raw_target()) + "::" + src.short_path)))

def _cover(ctx: "context", mode: GoCoverageMode.type, src: "artifact") -> "artifact":
    output = ctx.actions.declare_output("__cover__", src.short_path)

    go_toolchain = ctx.attrs._go_toolchain[GoToolchainInfo]
    cmd = cmd_args()
    cmd.add(go_toolchain.cover)
    cmd.add("-mode", mode.value)
    cmd.add("-var", _var(ctx.label, src))
    cmd.add("-o", output.as_output())
    cmd.add(src)
    ctx.actions.run(cmd, category = "go_cover", identifier = src.short_path)

    return output

def cover(ctx: "context", mode: GoCoverageMode.type, srcs: ["artifact"]) -> GoCoverResult.type:
    out_srcs = []
    out_vars = {}

    for src in srcs:
        if _is_test(src):
            out_srcs.append(src)
        else:
            out_srcs.append(_cover(ctx, mode, src))
            out_vars[_var(ctx.label, src)] = src

    return GoCoverResult(srcs = out_srcs, variables = out_vars)
