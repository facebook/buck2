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
    download = _download_toolchain(ctx)

    compiler_dst = ctx.actions.declare_output("compiler.exe" if host_info().os.is_windows else "compiler")

    cmd = cmd_args()
    if host_info().os.is_windows:
        compiler_src = cmd_args(download, format = "{}\\go\\bin\\go.exe")
        cmd.add([ctx.attrs._symlink_bat, compiler_dst.as_output(), compiler_src.relative_to(compiler_dst, parent = 1)])
    else:
        compiler_src = cmd_args(download, format = "{}/go/bin/go")
        cmd.add(["ln", "-sf", compiler_src.relative_to(compiler_dst, parent = 1), compiler_dst.as_output()])

    ctx.actions.run(cmd, category = "cp_compiler")
    return [DefaultInfo(default_output = download), GoCompilerInfo(compiler_path = compiler_dst, GOROOT = "")]

go_toolchain = rule(
    impl = _go_toolchain_impl,
    attrs = {
        "archive_extension": attrs.string(),
        "platform": attrs.string(),
        "sha256": attrs.string(),
        "version": attrs.string(),
        "_symlink_bat": attrs.default_only(attrs.source(default = "toolchains//:symlink.bat")),
    },
)

def _download_toolchain(ctx: AnalysisContext):
    archive_extension = ctx.attrs.archive_extension
    url = "https://go.dev/dl/go" + ctx.attrs.version + "." + ctx.attrs.platform + "." + archive_extension
    sha256 = ctx.attrs.sha256

    # Download archive.
    archive = ctx.actions.declare_output("archive." + archive_extension)
    ctx.actions.download_file(archive.as_output(), url, sha256 = sha256, is_deferrable = True)

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

    if host_info().os.is_windows:
        cmd = cmd_args([script])
    else:
        cmd = cmd_args(["/bin/sh", script])

    ctx.actions.run(cmd.hidden([archive, output.as_output()]), category = "extract_go_toolchain")

    return output

def _toolchain_config():
    version = "1.20.7"
    os = host_info().os
    if os.is_windows:
        return struct(
            sha256 = "736dc6c7fcab1c96b682c8c93e38d7e371e62a17d34cb2c37d451a1147f66af9",
            platform = "windows-amd64",
            archive_extension = "zip",
            version = version,
        )
    if os.is_macos:
        return struct(
            sha256 = "eea1e7e4c2f75c72629050e6a6c7c46c446d64056732a7787fb3ba16ace1982e",
            platform = "darwin-arm64",
            archive_extension = "tar.gz",
            version = version,
        )

    # Default linux
    return struct(
        sha256 = "f0a87f1bcae91c4b69f8dc2bc6d7e6bfcd7524fceec130af525058c0c17b1b44",
        platform = "linux-amd64",
        archive_extension = "tar.gz",
        version = version,
    )

toolchain_config = _toolchain_config()
