def _impl(platform, refs):
    watchos = refs.watchos[ConstraintValueInfo]
    constraints = {
        s: v
        for (s, v) in platform.configuration.constraints.items()
        if s != refs.os[ConstraintSettingInfo].label
    }
    constraints[watchos.setting.label] = watchos
    new_cfg = ConfigurationInfo(
        constraints = constraints,
        values = platform.configuration.values,
    )
    return PlatformInfo(
        label = "<transitioned-to-watch>",
        configuration = new_cfg,
    )

iphone_to_watch_transition = transition(impl = _impl, refs = {
    "os": "fbcode//buck2/tests/targets/configurations/transition/attr:os",
    "watchos": "fbcode//buck2/tests/targets/configurations/transition/attr:watchos",
})
