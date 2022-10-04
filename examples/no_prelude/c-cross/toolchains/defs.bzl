CCompilerInfo = provider(
    doc = "Information about how to invoke the C compiler.",
    fields = ["compiler_command"],
)

def _c_toolchain_impl(ctx):
    url = "https://ziglang.org/download/" + ctx.attrs.version + "/zig-" + ctx.attrs.platform + "-" + ctx.attrs.version + ".tar.xz"

    download = http_archive_impl(ctx, url, ctx.attrs.sha256)

    compiler_dst = ctx.actions.declare_output("zig")
    compiler_src = cmd_args(download[0].default_outputs[0], format = "{}/zig-" + ctx.attrs.platform + "-" + ctx.attrs.version + "/zig")
    ctx.actions.run(["ln", "-srf", compiler_src, compiler_dst.as_output()], category = "cp_compiler")

    return download + [CCompilerInfo(compiler_command = cmd_args([compiler_dst, "cc"]))]

c_toolchain = rule(
    impl = _c_toolchain_impl,
    attrs = {
        "platform": attrs.string(),
        "sha256": attrs.string(),
        "version": attrs.string(),
    },
)

def _c_cross_toolchain_impl(ctx):
    base = ctx.attrs.base
    cmd = cmd_args([base[CCompilerInfo].compiler_command, "-target", ctx.attrs.target])
    return [base[DefaultInfo], CCompilerInfo(compiler_command = cmd)]

c_cross_toolchain = rule(
    impl = _c_cross_toolchain_impl,
    attrs = {
        "base": attrs.dep(),
        "target": attrs.string(),
    },
)

def _inner_toolchain_impl(ctx):
    return ctx.attrs.toolchain.providers

inner_toolchain = rule(
    impl = _inner_toolchain_impl,
    attrs = {
        "toolchain": attrs.dep(),
    },
)

def _outer_toolchain_impl(ctx):
    return ctx.attrs.toolchain.providers

outer_toolchain = rule(
    impl = _outer_toolchain_impl,
    is_toolchain_rule = True,
    attrs = {
        "toolchain": attrs.exec_dep(),
    },
)

def _execution_platform_impl(ctx):
    cfg = ctx.attrs.configuration[ConfigurationInfo]
    name = ctx.label.raw_target()
    platform = ExecutionPlatformInfo(
        label = name,
        configuration = cfg,
        executor_config = CommandExecutorConfig(
            local_enabled = True,
            remote_enabled = False,
            use_windows_path_separators = ctx.attrs.use_windows_path_separators,
        ),
    )
    return [
        DefaultInfo(),
        platform,
        PlatformInfo(label = str(name), configuration = cfg),
        ExecutionPlatformRegistrationInfo(platforms = [platform]),
    ]

execution_platform = rule(
    impl = _execution_platform_impl,
    attrs = {
        "configuration": attrs.dep(),
        "use_windows_path_separators": attrs.bool(),
    },
)

def _target_platform_impl(ctx):
    cfg = ctx.attrs.configuration[ConfigurationInfo]
    name = ctx.label.raw_target()
    return [
        DefaultInfo(),
        PlatformInfo(label = str(name), configuration = cfg),
    ]

target_platform = rule(
    impl = _target_platform_impl,
    attrs = {
        "configuration": attrs.dep(),
        "use_windows_path_separators": attrs.bool(),
    },
)

def host_config():
    if host_info().arch.is_x86_64:
        arch = "x86_64"
    elif host_info().arch.is_arm:
        arch = "arm"
    else:
        fail("Unsupported CPU {}".format(host_info().arch))

    if host_info().os.is_linux:
        os = "linux"
    elif host_info().os.is_windows:
        os = "windows"
    else:
        fail("Unsupported OS {}".format(host_info().os))

    return ":{}-{}".format(arch, os)

def http_archive_impl(ctx: "context", url, sha256) -> ["provider"]:
    # Download archive.
    archive = ctx.actions.declare_output("archive.tar.gz")
    ctx.actions.download_file(archive.as_output(), url, sha256 = sha256, is_deferrable = True)

    # Unpack archive to output directory.
    compress_flag = "--xz"

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
