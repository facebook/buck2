# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

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
            default_output = app,
            sub_targets = {"dep_file": [DefaultInfo(default_output = dep_file)]},
        ),
        RunInfo(args = cmd_args(app)),
    ]

c_binary = rule(
    attrs = {
        "headers": attrs.list(attrs.source()),
        "main": attrs.source(),
        "_cc": attrs.dep(default = "root//tools:gcc"),
        "_ignored": attrs.string(default = ""),
    },
    impl = _c_binary_impl,
)

def _tool_impl(ctx):
    return [DefaultInfo(default_output = ctx.attrs.src), RunInfo(args = cmd_args(ctx.attrs.src))]

tool = rule(attrs = {"src": attrs.source()}, impl = _tool_impl)
