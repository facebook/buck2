def _not_hermetic_action_impl(ctx):
    dep = ctx.attrs.dep[DefaultInfo].default_outputs[0]
    sentinel = ctx.attrs.sentinel
    out = ctx.actions.declare_output("out")

    ctx.actions.run(
        cmd_args([
            "python3",
            "-c",
            "import sys, os.path; from pathlib import Path;\nif os.path.isfile(sys.argv[1]): Path(sys.argv[2]).touch()",
            sentinel,
            out.as_output(),
        ]).hidden(dep),
        local_only = True,
        category = "test",
    )

    return [DefaultInfo(default_outputs = [out])]

not_hermetic_action = rule(
    impl = _not_hermetic_action_impl,
    attrs = {
        "dep": attrs.dep(),
        "sentinel": attrs.source(),
    },
)

def _rerun_action_impl(ctx):
    src = ctx.attrs.src
    out = ctx.actions.declare_output("out")

    ctx.actions.run(
        cmd_args([
            "touch",
            out.as_output(),
        ]).hidden(src),
        category = "test",
    )

    return [DefaultInfo(default_outputs = [out])]

rerun_action = rule(
    impl = _rerun_action_impl,
    attrs = {
        "src": attrs.source(),
    },
)
