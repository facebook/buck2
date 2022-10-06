load("@prelude//android:android_providers.bzl", "CPU_FILTER_TO_ABI_DIRECTORY")
load("@prelude//android:min_sdk_version.bzl", "get_min_sdk_version_constraint_value_name", "get_min_sdk_version_range")

# FIXME: prelude// should be standalone (not refer to ovr_config//)
_REFS = {
    "arm64": "ovr_config//cpu/constraints:arm64",
    "armv7": "ovr_config//cpu/constraints:arm32",
    "cpu": "ovr_config//cpu/constraints:cpu",
    "min_sdk_version": "fbsource//xplat/buck2/platform/android:min_sdk_version",
    "x86": "ovr_config//cpu/constraints:x86_32",
    "x86_64": "ovr_config//cpu/constraints:x86_64",
}
for min_sdk in get_min_sdk_version_range():
    constraint_value_name = get_min_sdk_version_constraint_value_name(min_sdk)
    _REFS[constraint_value_name] = "fbsource//xplat/buck2/platform/android:{}".format(constraint_value_name)

def _cpu_split_transition_instrumentation_test_apk_impl(
        platform: PlatformInfo.type,
        refs: struct.type,
        attrs: struct.type) -> {str.type: PlatformInfo.type}:
    cpu_filters = attrs.cpu_filters or CPU_FILTER_TO_ABI_DIRECTORY.keys()

    return _cpu_split_transition(platform, refs, cpu_filters, attrs.min_sdk_version)

def _cpu_split_transition_impl(
        platform: PlatformInfo.type,
        refs: struct.type,
        attrs: struct.type) -> {str.type: PlatformInfo.type}:
    cpu_filters = attrs.cpu_filters or CPU_FILTER_TO_ABI_DIRECTORY.keys()

    return _cpu_split_transition(platform, refs, cpu_filters, attrs.min_sdk_version)

def _cpu_split_transition(
        platform: PlatformInfo.type,
        refs: struct.type,
        cpu_filters: [str.type],
        min_sdk_version: [int.type, None]) -> {str.type: PlatformInfo.type}:
    cpu = refs.cpu
    x86 = refs.x86[ConstraintValueInfo]
    x86_64 = refs.x86_64[ConstraintValueInfo]
    armv7 = refs.armv7[ConstraintValueInfo]
    arm64 = refs.arm64[ConstraintValueInfo]

    cpu_name_to_cpu_constraint = {}
    for cpu_filter in cpu_filters:
        if cpu_filter == "x86":
            cpu_name_to_cpu_constraint["x86"] = x86
        elif cpu_filter == "armv7":
            cpu_name_to_cpu_constraint["armv7"] = armv7
        elif cpu_filter == "x86_64":
            cpu_name_to_cpu_constraint["x86_64"] = x86_64
        elif cpu_filter == "arm64":
            cpu_name_to_cpu_constraint["arm64"] = arm64
        else:
            fail("Unexpected cpu_filter: {}".format(cpu_filter))

    base_constraints = {
        constraint_setting_label: constraint_setting_value
        for (constraint_setting_label, constraint_setting_value) in platform.configuration.constraints.items()
        if constraint_setting_label != cpu[ConstraintSettingInfo].label
    }

    if min_sdk_version:
        base_constraints[refs.min_sdk_version[ConstraintSettingInfo].label] = _get_min_sdk_constraint_value(min_sdk_version, refs)

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

def _cpu_transition_impl(
        platform: PlatformInfo.type,
        refs: struct.type,
        attrs: struct.type) -> PlatformInfo.type:
    return _cpu_split_transition_impl(platform, refs, attrs).values()[0]

cpu_split_transition = transition(
    impl = _cpu_split_transition_impl,
    refs = _REFS,
    attrs = [
        "cpu_filters",
        "min_sdk_version",
    ],
    split = True,
)

cpu_split_transition_instrumentation_test_apk = transition(
    impl = _cpu_split_transition_instrumentation_test_apk_impl,
    refs = _REFS,
    attrs = [
        "cpu_filters",
        "min_sdk_version",
    ],
    split = True,
)

# If our deps have been split-transitioned by CPU then we are already analyzing the dependency
# graph using the resulting configurations. If there are any other attributes on the same target
# that also need to analyze the dependency graph, then we want to use one of the configurations
# from the split transition so that we don't end up analyzing the graph again using a different
# configuration. This rule just picks the first configuration from the split-transition.
#
# This is used for the `manifest` attribute of `android_binary`.
cpu_transition = transition(
    impl = _cpu_transition_impl,
    refs = _REFS,
    attrs = [
        "cpu_filters",
        "min_sdk_version",
    ],
)

def get_deps_by_platform(ctx: "context") -> {str.type: ["dependency"]}:
    deps_by_platform = {}
    for dep_dict in ctx.attrs.deps:
        for platform, dep in dep_dict.items():
            deps = deps_by_platform.get(platform, [])
            deps.append(dep)
            deps_by_platform[platform] = deps

    return deps_by_platform

def _get_min_sdk_constraint_value(min_sdk_version: int.type, refs: struct.type) -> ConstraintValueInfo.type:
    constraint_name = get_min_sdk_version_constraint_value_name(min_sdk_version)
    constraint = getattr(refs, constraint_name, None)
    if not constraint:
        fail("Unsupported min_sdk_version {}, please report!".format(min_sdk_version))

    return constraint[ConstraintValueInfo]
