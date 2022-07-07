def _remote_text_impl(ctx):
    text = ctx.attr.text

    out = ctx.actions.declare_output("action_output")
    ctx.actions.run(
        cmd_args(["touch", out.as_output()]).hidden(text),
        category = "touch",
    )

    return [DefaultInfo(default_outputs = [out])]

remote_text = rule(
    implementation = _remote_text_impl,
    attrs = {
        "text": attr.source(),
    },
)

def _symlink_dir_impl(ctx):
    remote_text = ctx.attr.remote_text[DefaultInfo].default_outputs[0]
    link = ctx.actions.symlinked_dir(ctx.label.name, {"link": remote_text})
    return [DefaultInfo(default_outputs = [link])]

symlink_dir = rule(
    implementation = _symlink_dir_impl,
    attrs = {
        "remote_text": attr.dep(),
    },
)

def _check_impl(ctx):
    text = ctx.attr.text
    symlink_dir = ctx.attr.symlink_dir[DefaultInfo].default_outputs[0]

    out = ctx.actions.declare_output("out")

    ctx.actions.run(
        cmd_args(
            [
                "cp",
                cmd_args(symlink_dir, format = "{}/link"),
                out.as_output(),
            ],
        ).hidden(text),
        category = "test",
        prefer_local = True,
    )

    return [DefaultInfo(default_outputs = [out])]

check = rule(
    implementation = _check_impl,
    attrs = {
        "symlink_dir": attr.dep(),
        "text": attr.source(),
    },
)
