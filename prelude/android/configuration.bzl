load("@fbcode//buck2/prelude/utils:utils.bzl", "expect")

def _get_cpu_constraint_value(platform: PlatformInfo.type, refs: struct.type) -> ConstraintValueInfo.type:
    cpu_constraint = platform.configuration.constraints.get(refs.cpu[ConstraintSettingInfo].label)
    expect(cpu_constraint != None, "Android builds must have a cpu constraint specified!")
    return cpu_constraint

def _cpu_split_transition_impl(
        platform: PlatformInfo.type,
        refs: struct.type) -> {str.type: PlatformInfo.type}:
    cpu = refs.cpu
    x86 = refs.x86[ConstraintValueInfo]
    x86_64 = refs.x86_64[ConstraintValueInfo]
    armv7 = refs.armv7[ConstraintValueInfo]
    arm64 = refs.arm64[ConstraintValueInfo]
    x86_32_and_arm32 = refs.x86_32_and_arm32[ConstraintValueInfo]
    x86_64_and_arm64 = refs.x86_64_and_arm64[ConstraintValueInfo]

    cpu_constraint_value = _get_cpu_constraint_value(platform, refs)

    if cpu_constraint_value.label == x86.label:
        return {"x86": platform}
    elif cpu_constraint_value.label == x86_64.label:
        return {"x86_64": platform}
    elif cpu_constraint_value.label == armv7.label:
        return {"armv7": platform}
    elif cpu_constraint_value.label == arm64.label:
        return {"arm64": platform}

    cpu_name_to_cpu_constraint = {}
    if cpu_constraint_value.label == x86_32_and_arm32.label:
        cpu_name_to_cpu_constraint["x86"] = x86
        cpu_name_to_cpu_constraint["armv7"] = armv7
    elif cpu_constraint_value.label == x86_64_and_arm64.label:
        cpu_name_to_cpu_constraint["x86_64"] = x86_64
        cpu_name_to_cpu_constraint["arm64"] = arm64
    else:
        fail("Unexpected cpu_constraint label: {}".format(cpu_constraint_value.label))

    base_constraints = {
        constraint_setting_label: constraint_setting_value
        for (constraint_setting_label, constraint_setting_value) in platform.configuration.constraints.items()
        if constraint_setting_label != cpu[ConstraintSettingInfo].label
    }

    new_configs = {}
    for platform_name, cpu_constraint in cpu_name_to_cpu_constraint.items():
        updated_constraints = dict(base_constraints)
        updated_constraints[refs.cpu[ConstraintSettingInfo].label] = cpu_constraint
        new_configs[platform_name] = PlatformInfo(
            label = platform_name,
            configuration = ConfigurationInfo(
                constraints = updated_constraints,
                values = platform.configuration.values,
            ),
        )

    return new_configs

cpu_split_transition = transition(
    implementation = _cpu_split_transition_impl,
    refs = {
        "arm64": "ovr_config//cpu/constraints:arm64",
        "armv7": "ovr_config//cpu/constraints:arm32",
        "cpu": "ovr_config//cpu/constraints:cpu",
        "x86": "ovr_config//cpu/constraints:x86_32",
        "x86_32_and_arm32": "ovr_config//cpu/constraints:x86_32_and_arm32",
        "x86_64": "ovr_config//cpu/constraints:x86_64",
        "x86_64_and_arm64": "ovr_config//cpu/constraints:x86_64_and_arm64",
    },
    split = True,
)

def get_deps_by_platform(ctx: "context") -> {str.type: ["dependency"]}:
    deps_by_platform = {}
    for dep_dict in ctx.attr.deps:
        for platform, dep in dep_dict.items():
            deps = deps_by_platform.get(platform, [])
            deps.append(dep)
            deps_by_platform[platform] = deps

    return deps_by_platform
