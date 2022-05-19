load("@fbcode//buck2/platform/build_mode:defs.bzl", "BuildModeInfo")

def _execution_platform_impl(ctx):
    infos = [p[BuildModeInfo] for p in ctx.attr.remote_execution_action_key_providers]
    kvs = ["{}={}".format(info.cell, info.mode) for info in infos if info.mode != None]

    remote_execution_action_key = None
    if kvs:
        remote_execution_action_key = " ".join(kvs)

    return [
        DefaultInfo(),
        ExecutionPlatformInfo(
            label = ctx.label.raw_target(),
            configuration = ctx.attr.platform[PlatformInfo].configuration,
            local_enabled = ctx.attr.local_enabled,
            remote_enabled = ctx.attr.remote_enabled,
            remote_execution_properties = ctx.attr.remote_execution_properties,
            remote_execution_action_key = remote_execution_action_key,
            remote_execution_max_input_files_mebibytes = ctx.attr.remote_execution_max_input_files_mebibytes,
            use_limited_hybrid = True,
        ),
    ]

execution_platform_rule = rule(
    attrs = {
        "local_enabled": attr.bool(),
        "platform": attr.dep(providers = [PlatformInfo]),
        "remote_enabled": attr.bool(),
        "remote_execution_action_key_providers": attr.list(attr.dep()),
        "remote_execution_max_input_files_mebibytes": attr.int(),
        "remote_execution_properties": attr.dict(key = attr.string(), value = attr.string(), default = {}),
    },
    implementation = _execution_platform_impl,
)

def execution_platform(name, base_platform, local_enabled, remote_enabled, **kwargs):
    platform_name = name + "-platform"
    platforms = []

    constraint_values = []
    if local_enabled:
        constraint_values.append(":may_run_local")
    if remote_enabled:
        constraint_values.append(":may_run_remote")

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
        **kwargs
    )

    platforms.append(name)

    if local_enabled:
        native.platform(
            name = platform_name + "-local",
            deps = [base_platform],
            constraint_values = [
                ":may_run_local",
                ":runs_only_local",
            ],
        )
        execution_platform_rule(
            name = name + "-local-only",
            platform = ":" + platform_name + "-local",
            local_enabled = True,
            remote_enabled = False,
            **kwargs
        )
        platforms.append(name + "-local-only")

    if remote_enabled:
        native.platform(
            name = platform_name + "-remote",
            deps = [base_platform],
            constraint_values = [
                ":may_run_remote",
                ":runs_only_remote",
            ],
        )
        execution_platform_rule(
            name = name + "-remote-only",
            platform = ":" + platform_name + "-remote",
            local_enabled = False,
            remote_enabled = True,
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
def ordered_platforms_by_host_type(windows_platforms, mac_platforms, linux_platforms):
    if host_info().os.is_windows:
        return windows_platforms + linux_platforms + mac_platforms

    elif host_info().os.is_macos:
        return mac_platforms + linux_platforms + windows_platforms
    else:
        return linux_platforms + mac_platforms + windows_platforms

def _execution_platforms_impl(ctx):
    return [
        DefaultInfo(),
        ExecutionPlatformRegistrationInfo(
            platforms = [x[ExecutionPlatformInfo] for x in ctx.attr.platforms],
        ),
    ]

execution_platforms = rule(
    attrs = {
        "platforms": attr.list(attr.dep(providers = [ExecutionPlatformInfo])),
    },
    implementation = _execution_platforms_impl,
)
