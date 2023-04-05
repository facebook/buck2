# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

GoCompilerInfo = provider(
    doc = "Information about how to invoke the go compiler.",
    fields = ["compiler_path", "GOROOT"],
)

def _go_toolchain_impl(ctx):
    url = "https://go.dev/dl/go" + ctx.attrs.version + "." + ctx.attrs.platform + "." + ctx.attrs.archive_extension

    download = http_archive_impl(ctx, url, ctx.attrs.archive_extension, ctx.attrs.sha1)

    compiler_dst = ctx.actions.declare_output("compiler.exe" if host_info().os.is_windows else "compiler")

    cmd = cmd_args()
    if host_info().os.is_windows:
        compiler_src = cmd_args(download[0].default_outputs[0], format = "{}\\go\\bin\\go.exe")
        cmd.add([ctx.attrs._symlink_bat, compiler_dst.as_output(), compiler_src.relative_to(compiler_dst, parent = 1)])
    else:
        compiler_src = cmd_args(download[0].default_outputs[0], format = "{}/go/bin/go")
        cmd.add(["ln", "-sf", compiler_src.relative_to(compiler_dst, parent = 1), compiler_dst.as_output()])

    ctx.actions.run(cmd, category = "cp_compiler")
    return download + [GoCompilerInfo(compiler_path = compiler_dst, GOROOT = "")]

go_toolchain = rule(
    impl = _go_toolchain_impl,
    attrs = {
        "archive_extension": attrs.string(),
        "platform": attrs.string(),
        "sha1": attrs.string(),
        "version": attrs.string(),
        "_symlink_bat": attrs.default_only(attrs.source(default = "//go:symlink.bat")),
    },
)

def http_archive_impl(ctx: "context", url, archive_extension, sha1) -> ["provider"]:
    # Download archive.
    archive = ctx.actions.declare_output("archive." + archive_extension)
    ctx.actions.download_file(archive.as_output(), url, sha1 = sha1, is_deferrable = True)

    output = ctx.actions.declare_output(ctx.label.name)

    # Unpack archive to output directory.
    compress_flag = "-z"
    script_name = "unpack.bat" if host_info().os.is_windows else "unpack.sh"

    script_content = []
    if host_info().os.is_windows:
        script_content.append(cmd_args(output, format = "mkdir {}"))
    else:
        script_content.append(cmd_args(output, format = "mkdir -p {}"))
    script_content.extend([
        cmd_args(output, format = "cd {}"),
        cmd_args(["tar", compress_flag, "-x", "-f", archive], delimiter = " ").relative_to(output),
    ])
    script, _ = ctx.actions.write(
        script_name,
        script_content,
        is_executable = True,
        allow_args = True,
    )

    cmd = cmd_args()
    if host_info().os.is_windows:
        cmd = cmd_args([script])
    else:
        cmd = cmd_args(["/bin/sh", script])

    ctx.actions.run(cmd.hidden([archive, output.as_output()]), category = "http_archive")

    return [DefaultInfo(default_output = output)]

def _toolchain_config():
    version = "1.18.3"
    os = host_info().os
    if os.is_windows:
        return struct(
            sha1 = "0545d7d9ae308df6fa82c5f06ab740aec2a059a5",
            platform = "windows-amd64",
            archive_extension = "zip",
            version = version,
        )
    if os.is_macos:
        return struct(
            sha1 = "87a634156e4020c2806e8ab57ecdc18e54c914bc",
            platform = "darwin-amd64",
            archive_extension = "tar.gz",
            version = version,
        )

    # Default linux
    return struct(
        sha1 = "3511fcb34e0162abdcdeea0ab532f0264943e3d8",
        platform = "linux-amd64",
        archive_extension = "tar.gz",
        version = version,
    )

toolchain_config = _toolchain_config()
