def _platform_args(args):
    if host_info().os.is_windows:
        return ["cmd.exe", "/c"] + args
    else:
        return args

def _run_command_impl(ctx):
    test = ctx.attrs.name
    if test == "runs_simple_script":
        declared = ctx.actions.declare_output(ctx.attrs.out)
        args = [
            ctx.attrs.script,
            declared.as_output(),
            "foo",
            ctx.attrs.other_src,
            "bar",
        ]
        ctx.actions.run(_platform_args(args), category = "simple_script")
        return [DefaultInfo(default_outputs = [declared])]
    if test in ("runs_script_locally", "runs_script_locally_outputs_symlink"):
        declared = ctx.actions.declare_output(ctx.attrs.out)
        ctx.actions.run(_platform_args([ctx.attrs.script, declared.as_output()]), local_only = True, category = "local")
        return [DefaultInfo(default_outputs = [declared])]
    elif test == "rejects_zero_outputs":
        ctx.actions.run(_platform_args([ctx.attrs.script, "foo"]), category = "rejects_zero_outputs")
    elif test == "rejects_bad_args":
        ctx.actions.run({}, category = "bad_args")
    else:
        fail("invalid test")
    return None

run_command = rule(
    impl = _run_command_impl,
    attrs = {
        "other_src": attr.option(attr.source()),
        "out": attr.string(default = "out.txt"),
        "script": attr.source(),
    },
)
