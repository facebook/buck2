def _constraint_setting(ctx):
    return [DefaultInfo(), ConstraintSettingInfo(label = ctx.label.raw_target())]

constraint_setting = rule(
    impl = _constraint_setting,
    attrs = {},
)

def _constraint_value(ctx):
    constraint_value = ConstraintValueInfo(
        setting = ctx.attr.setting[ConstraintSettingInfo],
        label = ctx.label.raw_target(),
    )
    return [
        DefaultInfo(),
        constraint_value,
        # Provide `ConfigurationInfo` from `constraint_value` so it could be used as select key.
        ConfigurationInfo(constraints = {
            constraint_value.setting.label: constraint_value,
        }, values = {}),
    ]

constraint_value = rule(
    impl = _constraint_value,
    attrs = {"setting": attr.dep(providers = [ConstraintSettingInfo])},
)

def _platform(ctx):
    constraints = {}
    values = {}
    for x in ctx.attr.configuration:
        constraints |= x[ConfigurationInfo].constraints
        values |= x[ConfigurationInfo].values
    cfg = ConfigurationInfo(constraints = constraints, values = values)

    platform = ExecutionPlatformInfo(
        label = ctx.label.raw_target(),
        configuration = cfg,
        executor_config = CommandExecutorConfig(
            local_enabled = True,
            remote_enabled = False,
        ),
    )

    return [DefaultInfo(), platform, PlatformInfo(label = str(ctx.label.raw_target()), configuration = cfg)]

platform = rule(
    impl = _platform,
    attrs = {"configuration": attr.list(attr.dep(providers = [ConfigurationInfo]))},
)

def _platforms(ctx):
    return [
        DefaultInfo(),
        ExecutionPlatformRegistrationInfo(
            platforms = [x[ExecutionPlatformInfo] for x in ctx.attr.platforms],
        ),
    ]

platforms = rule(
    impl = _platforms,
    attrs = {"platforms": attr.list(attr.dep(providers = [ExecutionPlatformInfo]))},
)
