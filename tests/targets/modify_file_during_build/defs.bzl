def _modify_file_impl(ctx):
    text = ctx.attr.text

    out = ctx.actions.declare_output("out")

    ctx.actions.run([
        "sh",
        "-c",
        'echo REPLACEMENT > "$1" && touch "$2"',
        "--",
        text,
        out.as_output(),
    ], prefer_local = True, category = "test")

    return [DefaultInfo(default_outputs = [out])]

modify_file = rule(
    impl = _modify_file_impl,
    attrs = {
        "text": attr.source(),
    },
)

def _depend_impl(ctx):
    text = ctx.attr.text
    modify_file = ctx.attr.modify_file[DefaultInfo].default_outputs[0]

    out = ctx.actions.declare_output("out")

    ctx.actions.run(
        cmd_args(
            [
                "cp",
                text,
                out.as_output(),
            ],
        ).hidden(modify_file),
        category = "test",
    )

    return [DefaultInfo(default_outputs = [out])]

depend_file = rule(
    impl = _depend_impl,
    attrs = {
        "modify_file": attr.dep(),
        "text": attr.source(),
    },
)
