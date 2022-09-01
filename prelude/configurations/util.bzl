def _configuration_info_union(infos):
    if len(infos) == 0:
        return ConfigurationInfo(
            constraints = {},
            values = {},
        )
    if len(infos) == 1:
        return infos[0]
    constraints = {k: v for info in infos for (k, v) in info.constraints.items()}
    values = {k: v for info in infos for (k, v) in info.values.items()}
    return ConfigurationInfo(
        constraints = constraints,
        values = values,
    )

def _constraint_values_to_configuration(values):
    return ConfigurationInfo(constraints = {
        info[ConstraintValueInfo].setting.label: info[ConstraintValueInfo]
        for info in values
    }, values = {})

util = struct(
    configuration_info_union = _configuration_info_union,
    constraint_values_to_configuration = _constraint_values_to_configuration,
)
