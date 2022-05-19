def _impl(platform, refs):
    _ = platform
    cpu = refs.cpu
    arm64 = refs.arm64
    arm32 = refs.arm32
    return {
        "arm32": PlatformInfo(label = "arm32", configuration = ConfigurationInfo(constraints = {
            cpu[ConstraintSettingInfo].label: arm32[ConstraintValueInfo],
        }, values = {})),
        "arm64": PlatformInfo(label = "arm64", configuration = ConfigurationInfo(constraints = {
            cpu[ConstraintSettingInfo].label: arm64[ConstraintValueInfo],
        }, values = {})),
    }

cpu_split_transition = transition(implementation = _impl, refs = {
    "arm32": "fbcode//buck2/tests/targets/configurations/transition/attr_split:arm32",
    "arm64": "fbcode//buck2/tests/targets/configurations/transition/attr_split:arm64",
    "cpu": "fbcode//buck2/tests/targets/configurations/transition/attr_split:cpu",
}, split = True)
