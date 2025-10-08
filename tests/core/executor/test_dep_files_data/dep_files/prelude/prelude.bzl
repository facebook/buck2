# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

def _c_binary_impl(ctx):
    headers = {
        "{}/{}".format(ctx.label.package, h.short_path): h
        for h in ctx.attrs.headers
    }

    headers_tag = ctx.actions.artifact_tag()
    use_content_based_paths = ctx.attrs.use_content_based_paths

    headers_dir = ctx.actions.declare_output("headers", has_content_based_path = use_content_based_paths, dir = True)
    headers_dir = ctx.actions.copied_dir(headers_dir, headers)
    headers_dir = headers_tag.tag_artifacts(headers_dir)

    headers_dir_written, _ = ctx.actions.write("headers_dir_written", ctx.attrs.headers_dir_written, has_content_based_path = use_content_based_paths, allow_args = True)
    headers_dir_written = headers_tag.tag_artifacts(headers_dir_written)
    headers_dir_written_with_dep_files_placeholder, _ = ctx.actions.write(
        "headers_dir_written_with_dep_files_placeholder",
        ctx.attrs.headers_dir_written,
        use_dep_files_placeholder_for_content_based_paths = True,
        has_content_based_path = use_content_based_paths,
        allow_args = True,
    )

    dep_file = ctx.actions.declare_output("depfile", has_content_based_path = use_content_based_paths)
    app = ctx.actions.declare_output(ctx.attrs.name)

    cmd = cmd_args([
        ctx.attrs._cc[RunInfo].args,
        ctx.attrs.unused_command_line_param,
        ctx.attrs.main,
        "-I",
        headers_dir,
        "-o",
        app.as_output(),
        "-MMD",
        "-MF",
        headers_tag.tag_artifacts(dep_file.as_output()),
    ], hidden = [headers_dir_written, headers_dir_written_with_dep_files_placeholder])

    unused_wrapping_tag = ctx.actions.artifact_tag()
    cmd = unused_wrapping_tag.tag_artifacts(cmd)

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
        "headers_dir_written": attrs.arg(),
        "main": attrs.source(),
        "unused_command_line_param": attrs.string(),
        "use_content_based_paths": attrs.bool(default = read_config("test", "use_content_based_paths", "true") == "true"),
        "_cc": attrs.dep(default = "root//tools:gcc"),
        "_ignored": attrs.string(default = ""),
    },
    impl = _c_binary_impl,
)

def _tool_impl(ctx):
    return [DefaultInfo(default_output = ctx.attrs.src), RunInfo(args = cmd_args(ctx.attrs.src))]

tool = rule(attrs = {"src": attrs.source()}, impl = _tool_impl)

def _headers_dir_impl(ctx):
    headers = {
        "{}/{}".format(ctx.label.package, h.short_path): h
        for h in ctx.attrs.headers
    }

    headers_dir = ctx.actions.declare_output("headers", has_content_based_path = ctx.attrs.use_content_based_paths, dir = True)
    headers_dir = ctx.actions.copied_dir(headers_dir, headers)

    return [
        DefaultInfo(
            default_output = headers_dir,
        ),
    ]

headers_dir = rule(
    attrs = {
        "headers": attrs.list(attrs.source()),
        "use_content_based_paths": attrs.bool(default = read_config("test", "use_content_based_paths", "true") == "true"),
    },
    impl = _headers_dir_impl,
)
