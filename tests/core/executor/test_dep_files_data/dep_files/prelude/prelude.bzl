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

def _simple_dep_file_impl(ctx):
    has_content_based_path = ctx.attrs.use_content_based_paths
    used_input1 = ctx.actions.write("used_input1", ctx.attrs.used_input1_contents, has_content_based_path = has_content_based_path)
    symlink_to_used_input1 = ctx.actions.symlink_file("symlink_to_used_input1", used_input1, has_content_based_path = has_content_based_path)
    symlink_to_symlink_to_used_input1 = ctx.actions.symlink_file("symlink_to_symlink_to_used_input1", symlink_to_used_input1, has_content_based_path = has_content_based_path)
    used_input2 = ctx.actions.write("used_input2", ctx.attrs.used_input2_contents, has_content_based_path = has_content_based_path)
    unused_input1 = ctx.actions.write("unused_input1", ctx.attrs.unused_input1_contents, has_content_based_path = has_content_based_path)
    unused_input2 = ctx.actions.write("unused_input2", ctx.attrs.unused_input2_contents, has_content_based_path = has_content_based_path)

    dep_file = ctx.actions.declare_output("depfile", has_content_based_path = has_content_based_path)
    out = ctx.actions.declare_output("out", has_content_based_path = has_content_based_path)

    script = ctx.actions.write(
        "script.py",
        [
            "import sys",
            "with open(sys.argv[1], 'w') as f:",
            "  f.write('output')",
            "with open(sys.argv[2], 'w') as dep_file:",
            "  for arg in sys.argv[3:]:",
            "    dep_file.write('{}\\n'.format(arg))",
        ],
        has_content_based_path = has_content_based_path,
    )

    tag = ctx.actions.artifact_tag()
    args = cmd_args(
        [
            "fbpython",
            script,
            out.as_output(),
            tag.tag_artifacts(dep_file.as_output()),
            tag.tag_artifacts(symlink_to_symlink_to_used_input1),
            tag.tag_artifacts(used_input2),
        ],
        hidden = tag.tag_artifacts(cmd_args([unused_input1, unused_input2])),
    )

    ctx.actions.run(args, category = "test_run", dep_files = {"used": tag})

    return [DefaultInfo(default_output = out)]

simple_dep_file = rule(
    impl = _simple_dep_file_impl,
    attrs = {
        "unused_input1_contents": attrs.string(),
        "unused_input2_contents": attrs.string(),
        "use_content_based_paths": attrs.bool(default = read_config("test", "use_content_based_paths", "true") == "true"),
        "used_input1_contents": attrs.string(),
        "used_input2_contents": attrs.string(),
    },
)
