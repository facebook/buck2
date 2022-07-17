def _declare_sub_targets(ctx: "context") -> ["provider"]:
    out_dir = ctx.actions.declare_output("out_dir")
    sub_targets = {
        name: [DefaultInfo(default_outputs = [out_dir.project(name)])]
        for name in ctx.attrs.sub_targets
    }
    ctx.actions.run([ctx.attrs.command, out_dir.as_output()], category = "mkdirs")
    return [DefaultInfo(default_outputs = [out_dir], sub_targets = sub_targets)]

declare_sub_targets = rule(
    impl = _declare_sub_targets,
    attrs = {"command": attrs.source(), "sub_targets": attrs.list(attrs.string())},
)

def _exists(ctx: "context") -> ["provider"]:
    out = ctx.actions.declare_output("check")
    ctx.actions.run(
        [ctx.attrs.command, out.as_output(), ctx.attrs.paths],
        category = "check",
        local_only = ctx.attrs.local,
    )
    return [DefaultInfo(default_outputs = [out])]

exists = rule(
    impl = _exists,
    attrs = {"command": attrs.source(), "local": attrs.bool(default = False), "paths": attrs.list(attrs.arg())},
)
