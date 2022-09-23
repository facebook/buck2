GoCompilerInfo = provider(
    doc = "Information about how to invoke the go compiler.",
    fields = ["compiler_path", "GOROOT"],
)

def _go_toolchain_impl(ctx):
    url = "https://go.dev/dl/go" + ctx.attrs.version + "." + ctx.attrs.platform + ".tar.gz"

    download = http_archive_impl(ctx, url, ctx.attrs.sha1)

    compiler_dst = ctx.actions.declare_output("compiler")
    compiler_src = cmd_args(download[0].default_outputs[0], format = "{}/go/bin/go")
    ctx.actions.run(["ln", "-srf", compiler_src, compiler_dst.as_output()], category = "cp_compiler")

    return download + [GoCompilerInfo(compiler_path = compiler_dst, GOROOT = "")]

go_toolchain = rule(
    impl = _go_toolchain_impl,
    attrs = {
        "platform": attrs.string(),
        "sha1": attrs.string(),
        "version": attrs.string(),
    },
)

def http_archive_impl(ctx: "context", url, sha1) -> ["provider"]:
    # Download archive.
    archive = ctx.actions.declare_output("archive.tar.gz")
    ctx.actions.download_file(archive.as_output(), url, sha1 = sha1, is_deferrable = True)

    # Unpack archive to output directory.
    compress_flag = "-z"

    output = ctx.actions.declare_output(ctx.label.name)
    script, hidden = ctx.actions.write(
        "unpack.sh",
        [
            cmd_args(output, format = "mkdir -p {}"),
            cmd_args(output, format = "cd {}"),
            cmd_args(["tar", compress_flag, "-x", "-f", archive], delimiter = " ").relative_to(output),
        ],
        is_executable = True,
        allow_args = True,
    )

    ctx.actions.run(cmd_args(["/bin/sh", script])
        .hidden(hidden + [archive, output.as_output()]), category = "http_archive")

    return [DefaultInfo(default_outputs = [output])]
