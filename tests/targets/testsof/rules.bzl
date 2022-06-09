def _impl(_ctx):
    return [DefaultInfo()]

test_rule = rule(
    implementation = _impl,
    attrs = {
    },
)

def _config_setting_impl(ctx):
    return [DefaultInfo(), ConstraintSettingInfo(label = ctx.label.raw_target())]

config_setting = rule(
    implementation = _config_setting_impl,
    attrs = {
    },
)

def _configuration_impl(ctx):
    config_setting = ctx.attr.config_setting
    value = ConstraintValueInfo(
        setting = config_setting[ConstraintSettingInfo],
        label = ctx.label.raw_target(),
    )

    return [
        DefaultInfo(),
        ConfigurationInfo(constraints = {config_setting.label.raw_target(): value}, values = {}),
    ]

configuration = rule(
    implementation = _configuration_impl,
    attrs = {
        "config_setting": attr.configuration_label(),
    },
)

def _platform_impl(ctx):
    return [
        DefaultInfo(),
        PlatformInfo(
            label = str(ctx.label.raw_target()),
            configuration = ctx.attr.configuration[ConfigurationInfo],
        ),
    ]

platform = rule(
    implementation = _platform_impl,
    attrs = {
        "configuration": attr.configuration_label(),
    },
)
