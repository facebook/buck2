def _requires_local(ctx):
    for label in ctx.attrs.labels:
        if label == "buck2_test_local_exec":
            return True
    return False

def _proto_genrule_impl(ctx):
    out_artifact = ctx.actions.declare_output(ctx.attrs.out)
    env_vars = {
        "OUT": cmd_args(out_artifact.as_output()),
    }
    ctx.actions.run(
        cmd_args(["python3", "-c", ctx.attrs.python]),
        env = env_vars,
        prefer_local = _requires_local(ctx),
        category = "genrule",
    )
    return [DefaultInfo(default_outputs = [out_artifact])]

proto_genrule = rule(
    impl = _proto_genrule_impl,
    attrs = {
        "labels": attrs.list(attrs.string(), default = []),
        "out": attrs.string(),
        "python": attrs.option(attrs.arg(), default = None),
    },
)
