load("@fbcode//buck2/platform/build_mode:defs.bzl", "BuildModeInfo")
load("@fbsource//tools/build_defs:buckconfig.bzl", "read_bool", "read_int")
load("@prelude//:cache_mode.bzl", "CacheModeInfo")

MAC_X86_64_FBSOURCE_XCODE_13_4_PLATFORM_KEY = "x86_64-fbsource"
MAC_X86_64_FBSOURCE_MINIMAL_XCODE_13_4_PLATFORM_KEY = "x86_64_minimal_xcode"
MAC_X86_64_FBSOURCE_XCODE_14_0_PLATFORM_KEY = "x86_64-fbsource-xcode-14"
MAC_X86_64_FBSOURCE_MINIMAL_XCODE_14_0_PLATFORM_KEY = "x86_64_minimal_xcode_14"

FAT_PLATFORM_DEFAULT_MAC_PLATFORM_KEY = MAC_X86_64_FBSOURCE_XCODE_13_4_PLATFORM_KEY

# NB: The platforms' constraints for Xcode must match the values defined
#     by `mac_execution_subplatforms`. Mismatch will be detected as all
#     toolchain actions will fail due to a version mismatch.
mac_execution_base_platforms = {
    MAC_X86_64_FBSOURCE_XCODE_13_4_PLATFORM_KEY: "ovr_config//platform/macos:x86_64-fbsource-xcode-13.4",
    MAC_X86_64_FBSOURCE_MINIMAL_XCODE_13_4_PLATFORM_KEY: "ovr_config//platform/macos:x86_64-fbsource-minimal-xcode-13.4",
    MAC_X86_64_FBSOURCE_XCODE_14_0_PLATFORM_KEY: "ovr_config//platform/macos:x86_64-fbsource-xcode-14.0",
    MAC_X86_64_FBSOURCE_MINIMAL_XCODE_14_0_PLATFORM_KEY: "ovr_config//platform/macos:x86_64-fbsource-minimal-xcode-14.0",
}

mac_execution_subplatforms = {
    MAC_X86_64_FBSOURCE_XCODE_13_4_PLATFORM_KEY: "xcode-13.4",
    MAC_X86_64_FBSOURCE_MINIMAL_XCODE_13_4_PLATFORM_KEY: "xcode-13.4",
    MAC_X86_64_FBSOURCE_XCODE_14_0_PLATFORM_KEY: "xcode-14.0",
    MAC_X86_64_FBSOURCE_MINIMAL_XCODE_14_0_PLATFORM_KEY: "xcode-14.0",
}

linux_execution_base_platforms = {
    "platform009": "ovr_config//platform/linux:x86_64-fbcode-platform009-clang-nosan",
    "platform010": "ovr_config//platform/linux:x86_64-fbcode-platform010-clang-nosan",
    "platform010-aarch64": "ovr_config//platform/linux:aarch64-fbcode-platform010-aarch64-clang-nosan",
}

# Those targets will result in extra data being injected into RE action keys.
# The targets listed here need to expose a BuildModeInfo provider.
remote_execution_action_key_providers = [
    "fbcode//buck2/platform/build_mode:build_mode",
]
allow_hybrid_fallbacks_on_failure = read_bool("remoteexecution", "is_local_fallback_enabled_for_completed_actions")

host_is_mac = host_info().os.is_macos
local_mac_execution = read_bool("build", "enable_local_mac_execution", host_is_mac)
remote_mac_execution = read_bool("build", "enable_remote_mac_execution", True)

def mac_execution_platform(name: str.type, platform_key: str.type):
    return execution_platform(
        name = name,
        base_platform = mac_execution_base_platforms[platform_key],
        local_enabled = local_mac_execution,
        remote_enabled = remote_mac_execution,
        remote_execution_properties = {
            "platform": "mac",
            "subplatform": mac_execution_subplatforms[platform_key],
        },
        remote_execution_action_key_providers = remote_execution_action_key_providers,
        remote_execution_max_input_files_mebibytes = read_int("build", "remote_execution_max_input_files_mebibytes", 60 * 1024),
        allow_hybrid_fallbacks_on_failure = allow_hybrid_fallbacks_on_failure,
    )

def define_mac_execution_platforms():
    return mac_execution_platform(
        name = "macos-minimal",
        platform_key = MAC_X86_64_FBSOURCE_MINIMAL_XCODE_13_4_PLATFORM_KEY,
    ) + mac_execution_platform(
        name = "macos",
        platform_key = MAC_X86_64_FBSOURCE_XCODE_13_4_PLATFORM_KEY,
    ) + mac_execution_platform(
        name = "macos-minimal-xcode-14.0",
        platform_key = MAC_X86_64_FBSOURCE_MINIMAL_XCODE_14_0_PLATFORM_KEY,
    ) + mac_execution_platform(
        name = "macos-xcode-14.0",
        platform_key = MAC_X86_64_FBSOURCE_XCODE_14_0_PLATFORM_KEY,
    )

def _execution_platform_impl(ctx: "context"):
    infos = [p[BuildModeInfo] for p in ctx.attrs.remote_execution_action_key_providers]
    kvs = ["{}={}".format(info.cell, info.mode) for info in infos if info.mode != None]

    remote_execution_action_key = None
    if kvs:
        remote_execution_action_key = " ".join(kvs)

    return [
        DefaultInfo(),
        ExecutionPlatformInfo(
            label = ctx.label.raw_target(),
            configuration = ctx.attrs.platform[PlatformInfo].configuration,
            executor_config = CommandExecutorConfig(
                local_enabled = ctx.attrs.local_enabled,
                remote_enabled = ctx.attrs.remote_enabled,
                remote_execution_properties = ctx.attrs.remote_execution_properties,
                remote_execution_action_key = remote_execution_action_key,
                remote_execution_max_input_files_mebibytes = ctx.attrs.remote_execution_max_input_files_mebibytes,
                remote_execution_use_case = ctx.attrs.remote_execution_use_case,
                allow_limited_hybrid_fallbacks = True,
                use_limited_hybrid = ctx.attrs.use_limited_hybrid,
                allow_hybrid_fallbacks_on_failure = ctx.attrs.allow_hybrid_fallbacks_on_failure,
                use_windows_path_separators = ctx.attrs.use_windows_path_separators,
                allow_cache_uploads = ctx.attrs._cache_mode[CacheModeInfo].allow_cache_uploads,
            ),
        ),
    ]

execution_platform_rule = rule(
    attrs = {
        "allow_hybrid_fallbacks_on_failure": attrs.bool(default = False),
        "local_enabled": attrs.bool(),
        "platform": attrs.dep(providers = [PlatformInfo]),
        "remote_enabled": attrs.bool(),
        "remote_execution_action_key_providers": attrs.list(attrs.dep()),
        "remote_execution_max_input_files_mebibytes": attrs.int(),
        "remote_execution_properties": attrs.dict(key = attrs.string(), value = attrs.string(), default = {}),
        "remote_execution_use_case": attrs.string(default = "buck2-default"),
        "use_limited_hybrid": attrs.bool(default = read_bool("build", "use_limited_hybrid", True)),
        "use_windows_path_separators": attrs.bool(default = False),
        "_cache_mode": attrs.dep(default = "fbsource//xplat/buck2/platform/cache_mode:cache_mode"),
    },
    impl = _execution_platform_impl,
)

def _pkg(s: str.type) -> str.type:
    return "fbcode//buck2/platform/execution:" + s

def execution_platform(name, base_platform, local_enabled, remote_enabled, make_dash_only_platforms = True, **kwargs) -> [str.type]:
    if not local_enabled and not remote_enabled:
        # Some platforms do not support RE, so when running on a non-matching host,
        # neither local nor remote would be enabled.
        return []
    platform_name = name + "-platform"
    platforms = []

    constraint_values = []
    if local_enabled:
        constraint_values.append(_pkg("may_run_local"))
    if remote_enabled:
        constraint_values.append(_pkg("may_run_remote"))

    native.platform(
        name = platform_name,
        deps = [base_platform],
        constraint_values = constraint_values,
    )

    execution_platform_rule(
        name = name,
        platform = ":" + platform_name,
        local_enabled = local_enabled,
        remote_enabled = remote_enabled,
        visibility = ["fbcode//buck2/tests/..."],
        **kwargs
    )

    platforms.append(name)

    if make_dash_only_platforms:
        if local_enabled:
            native.platform(
                name = platform_name + "-local",
                deps = [base_platform],
                constraint_values = [
                    _pkg("may_run_local"),
                    _pkg("runs_only_local"),
                ],
            )
            execution_platform_rule(
                name = name + "-local-only",
                platform = ":" + platform_name + "-local",
                local_enabled = True,
                remote_enabled = False,
                visibility = ["fbcode//buck2/tests/..."],
                **kwargs
            )
            platforms.append(name + "-local-only")

        if remote_enabled:
            native.platform(
                name = platform_name + "-remote",
                deps = [base_platform],
                constraint_values = [
                    _pkg("may_run_remote"),
                    _pkg("runs_only_remote"),
                ],
            )
            execution_platform_rule(
                name = name + "-remote-only",
                platform = ":" + platform_name + "-remote",
                local_enabled = False,
                remote_enabled = True,
                visibility = ["fbcode//buck2/tests/..."],
                **kwargs
            )
            platforms.append(name + "-remote-only")
    return [":" + p for p in platforms]

# Before execution platforms, we'd generally use the host platform as the
# execution platform. We flip these orders on different hosts to roughly
# preserve that behavior.
# TODO(cjhopman): Our long-term goal has to be to remove this. All build
# tooling will first need to be updated to properly specify their execution
# requirements.
def ordered_platforms_by_host_type(windows_platforms: [str.type], mac_platforms: [str.type], linux_platforms: [str.type]) -> [str.type]:
    if host_info().os.is_windows:
        return windows_platforms + linux_platforms + mac_platforms

    elif host_info().os.is_macos:
        return mac_platforms + linux_platforms + windows_platforms
    else:
        return linux_platforms + mac_platforms + windows_platforms

def _execution_platforms_impl(ctx: "context") -> ["provider"]:
    return [
        DefaultInfo(),
        ExecutionPlatformRegistrationInfo(
            platforms = [x[ExecutionPlatformInfo] for x in ctx.attrs.platforms],
        ),
    ]

execution_platforms = rule(
    attrs = {
        "platforms": attrs.list(attrs.dep(providers = [ExecutionPlatformInfo])),
    },
    impl = _execution_platforms_impl,
)

FatPlatformTransitionInfo = provider(
    fields = ("mac", "linux"),
)

def _fat_platforms_transition_helper(ctx: "context") -> ["provider"]:
    return [
        DefaultInfo(),
        FatPlatformTransitionInfo(
            mac = [(k[ConstraintValueInfo], v[PlatformInfo]) for (k, v) in ctx.attrs.mac],
            linux = [(k[ConstraintValueInfo], v[PlatformInfo]) for (k, v) in ctx.attrs.linux],
        ),
    ]

fat_platform_transition_helper = rule(
    impl = _fat_platforms_transition_helper,
    attrs = {
        "linux": attrs.list(attrs.tuple(attrs.dep(providers = [ConstraintValueInfo]), attrs.dep(providers = [PlatformInfo]))),
        "mac": attrs.list(attrs.tuple(attrs.dep(providers = [ConstraintValueInfo]), attrs.dep(providers = [PlatformInfo]))),
    },
)
