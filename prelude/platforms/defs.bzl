def _execution_platform_impl(ctx: "context") -> ["provider"]:
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
        "configuration": attrs.dep(providers = [ConfigurationInfo]),
        "use_windows_path_separators": attrs.bool(),
    },
)
