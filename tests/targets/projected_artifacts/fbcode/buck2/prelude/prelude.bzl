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
    attrs = {"command": attr.source(), "sub_targets": attr.list(attr.string())},
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
    attrs = {"command": attr.source(), "local": attr.bool(default = False), "paths": attr.list(attr.arg())},
)
