def _declare_sub_targets(ctx: "context") -> ["provider"]:
    out_dir = ctx.actions.declare_output("out_dir")
    sub_targets = {
        name: [DefaultInfo(default_outputs = [out_dir.project(name)])]
        for name in ctx.attr.sub_targets
    }
    ctx.actions.run([ctx.attr.command, out_dir.as_output()], category = "mkdirs")
    return [DefaultInfo(default_outputs = [out_dir], sub_targets = sub_targets)]

declare_sub_targets = rule(
    implementation = _declare_sub_targets,
    attrs = {"command": attr.source(), "sub_targets": attr.list(attr.string())},
)

def _exists(ctx: "context") -> ["provider"]:
    out = ctx.actions.declare_output("check")
    ctx.actions.run([ctx.attr.command, out.as_output(), ctx.attr.paths], category = "check")
    return [DefaultInfo(default_outputs = [out])]

exists = rule(
    implementation = _exists,
    attrs = {"command": attr.source(), "paths": attr.list(attr.arg())},
)
