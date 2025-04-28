# @nolint

def _execution_platform(ctx):
    return [
        DefaultInfo(),
        ExecutionPlatformInfo(
            label = ctx.label.raw_target(),
            configuration = ctx.attrs.os_configuration[ConfigurationInfo],
            executor_config = CommandExecutorConfig(
                local_enabled = True,
                remote_enabled = False,
            ),
        ),
    ]

execution_platform = rule(
    impl = _execution_platform,
    attrs = {
        "os_configuration": attrs.dep(providers = [ConfigurationInfo]),
    },
)

def _execution_platforms(ctx):
    return [
        DefaultInfo(),
        ExecutionPlatformRegistrationInfo(
            platforms = [p[ExecutionPlatformInfo] for p in ctx.attrs.platforms],
        ),
    ]

execution_platforms = rule(
    impl = _execution_platforms,
    attrs = {
        "platforms": attrs.list(attrs.dep(providers = [ExecutionPlatformInfo])),
    },
)