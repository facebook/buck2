load(
    ":defs.bzl",
    "FatPlatformTransitionInfo",
)

def _impl(platform: PlatformInfo.type, refs: struct.type):
    helper = refs._[FatPlatformTransitionInfo]
    splits = {}
    for (key, values) in [("mac", helper.mac), ("linux", helper.linux)]:
        for (constraint, split_platform) in values:
            platform_constraint = platform.configuration.constraints[constraint.setting.label]

            # unfortunately, buck2 internal providers don't properly implement equality
            if platform_constraint and platform_constraint.label == constraint.label:
                splits[key] = split_platform

    return splits

fat_platform_transition = transition(_impl, refs = {"_": "fbcode//buck2/platform/execution:fat_platform_transition_helper"}, split = True)
