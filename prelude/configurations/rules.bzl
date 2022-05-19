load(":util.bzl", "util")

# config_setting() accepts a list of constraint_values and a list of values
# (buckconfig keys + expected values) and matches if all of those match.
#
# This is implemented as forming a single ConfigurationInfo from the union of the
# referenced values and the config keys.
#
# Attributes:
#   "constraint_values": attr.list(attr.configuration_label(), default = []),
#   "values": attr.dict(key = attr.string(), value = attr.string(), sorted = False, default = {}),
def config_setting_impl(ctx):
    subinfos = [util.constraint_values_to_configuration(ctx.attr.constraint_values)]
    subinfos.append(ConfigurationInfo(constraints = {}, values = ctx.attr.values))
    return [DefaultInfo(), util.configuration_info_union(subinfos)]

# constraint_setting() targets just declare the existence of a constraint.
def constraint_setting_impl(ctx):
    return [DefaultInfo(), ConstraintSettingInfo(ctx.label.raw_target())]

# constraint_value() declares a specific value of a constraint_setting.
#
# Attributes:
#  constraint_setting: the target constraint that this is a value of
def constraint_value_impl(ctx):
    constraint_value = ConstraintValueInfo(
        ctx.attr.constraint_setting[ConstraintSettingInfo],
        ctx.label.raw_target(),
    )
    return [
        DefaultInfo(),
        constraint_value,
        # Provide `ConfigurationInfo` from `constraint_value` so it could be used as select key.
        ConfigurationInfo(constraints = {
            constraint_value.setting.label: constraint_value,
        }, values = {}),
    ]

# platform() declares a platform, it is a list of constraint values.
#
# Attributes:
#  constraint_values: list of constraint values that are set for this platform
#  deps: a list of platform target dependencies, the constraints from these platforms will be part of this platform (unless overriden)
def platform_impl(ctx):
    subinfos = (
        [dep[PlatformInfo].configuration for dep in ctx.attr.deps] +
        [util.constraint_values_to_configuration(ctx.attr.constraint_values)]
    )
    return [
        DefaultInfo(),
        PlatformInfo(
            label = str(ctx.label.raw_target()),
            configuration = util.configuration_info_union(subinfos),
        ),
    ]

# TODO(cjhopman): Update the attributes for these ruletypes to declare the types of providers that they expect in their references.

implemented_rules = {
    "config_setting": config_setting_impl,
    "constraint_setting": constraint_setting_impl,
    "constraint_value": constraint_value_impl,
    "platform": platform_impl,
}
