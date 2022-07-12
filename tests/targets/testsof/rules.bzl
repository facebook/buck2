def _impl(_ctx):
    return [DefaultInfo()]

test_rule = rule(
    impl = _impl,
    attrs = {
    },
)

def _config_setting_impl(ctx):
    return [DefaultInfo(), ConstraintSettingInfo(label = ctx.label.raw_target())]

config_setting = rule(
    impl = _config_setting_impl,
    attrs = {
    },
)

def _configuration_impl(ctx):
    config_setting = ctx.attrs.config_setting
    value = ConstraintValueInfo(
        setting = config_setting[ConstraintSettingInfo],
        label = ctx.label.raw_target(),
    )

    return [
        DefaultInfo(),
        ConfigurationInfo(constraints = {config_setting.label.raw_target(): value}, values = {}),
    ]

configuration = rule(
    impl = _configuration_impl,
    attrs = {
        "config_setting": attr.configuration_label(),
    },
)

def _platform_impl(ctx):
    return [
        DefaultInfo(),
        PlatformInfo(
            label = str(ctx.label.raw_target()),
            configuration = ctx.attrs.configuration[ConfigurationInfo],
        ),
    ]

platform = rule(
    impl = _platform_impl,
    attrs = {
        "configuration": attr.configuration_label(),
    },
)
