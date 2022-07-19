def _proto_genrule_impl(ctx):
    out_artifact = ctx.actions.declare_output(ctx.attrs.out)
    env_vars = {
        "OUT": cmd_args(out_artifact.as_output()),
    }
    ctx.actions.run(
        cmd_args(["python3", "-c", ctx.attrs.python]),
        env = env_vars,
        category = "genrule",
    )
    return [DefaultInfo(default_outputs = [out_artifact])]

proto_genrule = rule(
    impl = _proto_genrule_impl,
    attrs = {
        "out": attrs.string(),
        "python": attrs.option(attrs.arg(), default = None),
    },
)
