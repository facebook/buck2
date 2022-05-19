def _run_command_impl(ctx):
    test = ctx.attr.name
    if test == "runs_simple_script":
        declared = ctx.actions.declare_output(ctx.attr.out)
        args = [
            ctx.attr.script,
            declared.as_output(),
            "foo",
            ctx.attr.other_src,
            "bar",
        ]
        ctx.actions.run(args, category = "simple_script")
        return [DefaultInfo(default_outputs = [declared])]
    if test in ("runs_script_locally", "runs_script_locally_outputs_symlink"):
        declared = ctx.actions.declare_output(ctx.attr.out)
        ctx.actions.run([ctx.attr.script, declared.as_output()], local_only = True, category = "local")
        return [DefaultInfo(default_outputs = [declared])]
    elif test == "rejects_zero_outputs":
        ctx.actions.run([ctx.attr.script, "foo"], category = "rejects_zero_outputs")
    elif test == "rejects_bad_args":
        ctx.actions.run({}, category = "bad_args")
    else:
        fail("invalid test")
    return None

run_command = rule(
    implementation = _run_command_impl,
    attrs = {
        "other_src": attr.option(attr.source()),
        "out": attr.string(default = "out.txt"),
        "script": attr.source(),
    },
)
