load("@prelude//os_lookup:defs.bzl", "OsLookup")
load(":releases.bzl", "releases")

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
        fail("Unsupported platform '{}'. Available artifacts: {}".format(
            platform,
            ", ".join(protoc_version.keys()),
        ))
    protoc_artifact = protoc_version[artifact]
    return ProtocReleaseInfo(
        version = version,
        url = protoc_artifact["url"],
        sha256 = protoc_artifact["sha256"],
    )

def _turn_http_archive_into_protoc_distribution(
        providers: "provider_collection",
        protoc_filename: str.type) -> ["provider"]:
    downloads = providers[DefaultInfo].sub_targets
    include = downloads["include"][DefaultInfo]
    protoc = downloads[protoc_filename][DefaultInfo]

    return [DefaultInfo(
        sub_targets = {
            "google_protobuf": [include],
            "protoc": [
                protoc,
                RunInfo(args = protoc.default_outputs[0]),
            ],
        },
    )]

def _download_protoc_distribution_impl(ctx: "context") -> "promise":
    protoc_filename = "bin/protoc" + ctx.attrs.exe_extension

    return ctx.actions.anon_target(native.http_archive, {
        "sha256": ctx.attrs.sha256,
        "sub_targets": [
            protoc_filename,
            "include",
        ],
        "urls": [ctx.attrs.url],
        # Anon target hacks.
        "_create_exclusion_list": [],
        "_exec_os_type": [],
        "_override_exec_platform_name": ctx.attrs._exec_os_type[OsLookup].platform,
    }).map(lambda providers: _turn_http_archive_into_protoc_distribution(
        providers = providers,
        protoc_filename = protoc_filename,
    ))

download_protoc_distribution = rule(
    impl = _download_protoc_distribution_impl,
    attrs = {
        "exe_extension": attrs.string(),
        "sha256": attrs.string(),
        "url": attrs.string(),
        "_exec_os_type": attrs.default_only(attrs.exec_dep(default = "prelude//os_lookup/targets:os_lookup")),
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
    else:
        fail("Unknown platform: os={}, arch={}".format(os, arch))

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
