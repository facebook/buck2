def _c_binary_impl(ctx):
    headers = {
        "{}/{}".format(ctx.label.package, h.short_path): h
        for h in ctx.attrs.headers
    }

    headers_tag = ctx.actions.artifact_tag()

    headers_dir = ctx.actions.symlinked_dir("headers", headers)
    headers_dir = headers_tag.tag_artifacts(headers_dir)

    dep_file = ctx.actions.declare_output("depfile")
    app = ctx.actions.declare_output(ctx.attrs.name)

    cmd = [
        ctx.attrs._cc[RunInfo].args,
        ctx.attrs.main,
        "-I",
        headers_dir,
        "-o",
        app.as_output(),
        "-MMD",
        "-MF",
        headers_tag.tag_artifacts(dep_file.as_output()),
    ]

    ctx.actions.run(
        cmd,
        category = "cxx_link",
        dep_files = {"headers": headers_tag},
    )

    return [
        DefaultInfo(
            default_outputs = [app],
            sub_targets = {"dep_file": [DefaultInfo(default_outputs = [dep_file])]},
        ),
        RunInfo(args = cmd_args(app)),
    ]

c_binary = rule(
    attrs = {
        "headers": attr.list(attr.source()),
        "main": attr.source(),
        "_cc": attr.dep(default = "root//tools:gcc"),
    },
    impl = _c_binary_impl,
)

def _tool_impl(ctx):
    return [DefaultInfo(default_outputs = [ctx.attrs.src]), RunInfo(args = cmd_args(ctx.attrs.src))]

tool = rule(attrs = {"src": attr.source()}, impl = _tool_impl)
