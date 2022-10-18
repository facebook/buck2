load(
    ":releases.bzl",
    "releases",
)

ProtocReleaseInfo = provider(fields = [
    "version",
    "url",
    "sha256",
])

def _get_protoc_release(
        version: "string",
        platform: "string") -> ProtocReleaseInfo.type:
    if not version in releases:
        fail("Unknown protoc release version '{}'. Available versions: {}".format(
            version,
            ", ".join(releases.keys()),
        ))
    protoc_version = releases[version]
    artifact = "protoc-{}-{}.zip".format(version, platform)
    if not artifact in protoc_version:
        fail("Unsupported platfrom '{}'. Available artifacts: {}".format(
            platform,
            ", ".join(protoc_version.keys()),
        ))
    protoc_artifact = protoc_version[artifact]
    return ProtocReleaseInfo(
        version = version,
        url = protoc_artifact["url"],
        sha256 = protoc_artifact["sha256"],
    )

def _download_protoc_distribution_impl(ctx: "context") -> ["provider"]:
    # TODO Switch to http_archive once that supports zip download.
    #   See https://github.com/facebookincubator/buck2/issues/21
    archive = ctx.actions.declare_output("archive.zip")
    ctx.actions.download_file(archive.as_output(), ctx.attrs.url, sha256 = ctx.attrs.sha256, is_deferrable = True)
    exe = ctx.attrs.exe_extension
    protoc = ctx.actions.declare_output("protoc" + exe)
    google_protobuf = ctx.actions.declare_output("proto.d")
    script, _ = ctx.actions.write(
        "unpack.sh",
        [
            cmd_args(['TMP="$(mktemp -d)"']),
            cmd_args(["trap", '"rm -rf $TMP"', "EXIT"], delimiter = " "),
            cmd_args(["unzip", archive, "-d", "$TMP"], delimiter = " "),
            cmd_args(["cp", "$TMP/bin/protoc" + exe, protoc], delimiter = " "),
            cmd_args(["cp", "-r", "$TMP/include", google_protobuf], delimiter = " "),
        ],
        is_executable = True,
        allow_args = True,
    )
    ctx.actions.run(cmd_args(["/bin/sh", script])
        .hidden([archive, protoc.as_output(), google_protobuf.as_output()]), category = "http_archive")
    return [DefaultInfo(
        sub_targets = {
            "protoc": [DefaultInfo(default_outputs = [protoc]), RunInfo(args = cmd_args([protoc]))],
            "google_protobuf": [DefaultInfo(default_outputs = [google_protobuf])],
        },
    )]

download_protoc_distribution = rule(
    impl = _download_protoc_distribution_impl,
    attrs = {
        "url": attrs.string(),
        "sha256": attrs.string(),
        "exe_extension": attrs.string(),
    },
)

def _host_platform():
    os = host_info().os
    arch = host_info().arch
    if os.is_linux and arch.is_x86_64:
        return "linux-x86_64"
    elif os.is_linux and arch.is_aarch64:
        return "linux-aarch_64"
    elif os.is_macos and arch.is_x86_64:
        return "osx-x86_64"
    elif os.is_macos and arch.is_aarch64:
        return "osx-aarch_64"
    elif os.is_windows and arch.is_x86_64:
        return "win64"

def protoc_distribution(
        name: "string",
        version: "string",
        platform: [None, "string"] = None):
    if platform == None:
        platform = _host_platform()
    exe_extension = ".exe" if platform.startswith("win") else ""
    release = _get_protoc_release(version, platform)
    download_protoc_distribution(
        name = name,
        url = release.url,
        sha256 = release.sha256,
        exe_extension = exe_extension,
    )
