def _rust_binary_impl(ctx):
    file = ctx.attr.file
    out = ctx.actions.declare_output("main")

    cmd = cmd_args(["rustc", "--crate-type=bin", file, "-o", out.as_output()])

    ctx.actions.run(cmd, category="compile")

    return [DefaultInfo(default_outputs = [out]), RunInfo(args=cmd_args([out]))]


rust_binary = rule(
    implementation = _rust_binary_impl,
    attrs = {
        "file": attr.source()
    },
)
