# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

load("@prelude//apple:arch.bzl", "AppleArch")
load("@prelude//utils:glob_defs.bzl", "subdir_glob")
load("@prelude//utils:selects.bzl", "selects")

def _generate_targets_and_select_map(prebuilt_xcframework_args, platform_filter, suffix, default_arch, compatible_with, default_target_platform, genrule, prebuilt_apple_framework_rule, **kwargs):
    name = prebuilt_xcframework_args.name
    platforms = prebuilt_xcframework_args.platforms
    xcframework = prebuilt_xcframework_args.xcframework
    framework_name = prebuilt_xcframework_args.framework_name
    framework_platforms_to_dsym_filenames = prebuilt_xcframework_args.framework_platforms_to_dsym_filenames

    select_map = {}
    for platform in platforms:
        if platform_filter(platform):
            bash_cmd = "mkdir $OUT && cp -R $(location {})/{}/{}.framework/* $OUT/".format(xcframework, platform["folder"], framework_name)
            assemble_name = "{}-{}-assemble".format(name, platform["folder"])
            genrule(
                name = assemble_name,
                bash = bash_cmd,
                out = framework_name + ".framework",
                default_target_platform = default_target_platform,
            )

            dsym_artifacts = selects.apply(
                framework_platforms_to_dsym_filenames,
                partial(_generate_dsym_artifacts, name, platform["folder"], xcframework, default_target_platform, genrule),
            )

            for arch in platform["archs"]:
                if arch not in ["arm64", "x86_64"]:
                    fail("Unsupported " + platform["platform"] + " arch: " + arch)
                target = name + "_" + suffix + "_" + arch
                select_map["ovr_config//cpu:" + arch] = ":" + target

                # We want to set DEFAULT if
                #  - We have specified a default_arch and it matches
                #  - We haven't specified a default_arch in which case
                #    we take the first match
                if (default_arch != None and arch == default_arch.value) or select_map.get("DEFAULT") == None:
                    select_map["DEFAULT"] = ":" + target

                prebuilt_apple_framework_rule(
                    name = target,
                    compatible_with = compatible_with,
                    framework = ":" + assemble_name,
                    dsyms = dsym_artifacts,
                    default_target_platform = default_target_platform,
                    **kwargs
                )
    return select_map

def _generate_dsym_artifacts(name, platform_folder, xcframework, default_target_platform, genrule, framework_platforms_to_dsym_filenames):
    return [
        _generate_dsym_artifact(name, platform_folder, xcframework, default_target_platform, dsym_filename, genrule)
        for dsym_filename in framework_platforms_to_dsym_filenames.get(platform_folder, [])
    ]

def _generate_dsym_artifact(name, platform_folder, xcframework, default_target_platform, dsym_filename, genrule):
    bash_cmd_dsyms = "mkdir $OUT && cp -R $(location {})/{}/dSYMs/{}/* $OUT/".format(xcframework, platform_folder, dsym_filename)
    dsym_name = "{}-{}-dsym".format(name, platform_folder)
    genrule(
        name = dsym_name,
        bash = bash_cmd_dsyms,
        out = dsym_filename,
        default_target_platform = default_target_platform,
    )
    return ":" + dsym_name

def _generate_iphonesimulator_targets_and_select_map(prebuilt_xcframework_args, **kwargs):
    return _generate_targets_and_select_map(
        prebuilt_xcframework_args = prebuilt_xcframework_args,
        platform_filter = lambda platform: platform["platform"] == "ios" and platform["isSimulator"] and not platform["isCatalyst"],
        suffix = "iphonesimulator",
        default_arch = AppleArch("arm64"),
        compatible_with = ["ovr_config//os:iphoneos"],
        default_target_platform = "ovr_config//platform/iphoneos:iphonesimulator-arm64",
        **kwargs
    )

def _generate_iphone_targets_and_select_map(prebuilt_xcframework_args, **kwargs):
    return _generate_targets_and_select_map(
        prebuilt_xcframework_args = prebuilt_xcframework_args,
        platform_filter = lambda platform: platform["platform"] == "ios" and not platform["isSimulator"] and not platform["isCatalyst"],
        suffix = "iphone",
        default_arch = None,
        compatible_with = ["ovr_config//os:iphoneos"],
        default_target_platform = "ovr_config//platform/iphoneos:iphoneos-arm64",
        **kwargs
    )

def _generate_mac_targets_and_select_map(prebuilt_xcframework_args, **kwargs):
    return _generate_targets_and_select_map(
        prebuilt_xcframework_args = prebuilt_xcframework_args,
        platform_filter = lambda platform: platform["platform"] == "macos",
        suffix = "mac",
        default_arch = None,
        compatible_with = ["ovr_config//os:macos"],
        default_target_platform = "ovr_config//platform/macos:arm64-fbsource",
        **kwargs
    )

def _generate_maccatalyst_targets_and_select_map(prebuilt_xcframework_args, **kwargs):
    return _generate_targets_and_select_map(
        prebuilt_xcframework_args = prebuilt_xcframework_args,
        platform_filter = lambda platform: platform["platform"] == "ios" and not platform["isSimulator"] and platform["isCatalyst"],
        suffix = "maccatalyst",
        default_arch = None,
        compatible_with = ["ovr_config//os:iphoneos"],
        default_target_platform = "ovr_config//platform/macos:arm64-catalyst",
        **kwargs
    )

def prebuilt_apple_xcframework_macro_impl(alias_rule, filegroup_rule, name, xcframework, framework_name, framework_platforms, framework_platforms_to_dsym_filenames = {}, default_target_platform = None, **kwargs):
    compatible_with = kwargs.pop("compatible_with", None)

    parsed_platforms = [_parse_platform(platform) for platform in framework_platforms]

    # The xcframework is not required to be on disk, so we must make the folder path compatible with getting it from another target
    if ":" in xcframework:
        xcframework_target = xcframework
    else:
        xcframework_target = ":" + xcframework
        filegroup_rule(
            name = xcframework,
            srcs = subdir_glob(
                [
                    (xcframework, "**/*"),
                ],
            ),
        )

    prebuilt_xcframework_args = struct(
        name = name,
        platforms = parsed_platforms,
        xcframework = xcframework_target,
        framework_name = framework_name,
        framework_platforms_to_dsym_filenames = framework_platforms_to_dsym_filenames,
    )

    iphonesimulator_select_map = _generate_iphonesimulator_targets_and_select_map(prebuilt_xcframework_args, **kwargs)
    iphone_select_map = _generate_iphone_targets_and_select_map(prebuilt_xcframework_args, **kwargs)
    mac_select_map = _generate_mac_targets_and_select_map(prebuilt_xcframework_args, **kwargs)
    mac_catalyst_select_map = _generate_maccatalyst_targets_and_select_map(prebuilt_xcframework_args, **kwargs)

    default_select_map = None

    if len(iphonesimulator_select_map) > 0:
        default_select_map = iphonesimulator_select_map
    elif len(iphone_select_map) > 0:
        default_select_map = iphone_select_map
    elif len(mac_select_map) > 0:
        default_select_map = mac_select_map
    elif len(mac_catalyst_select_map) > 0:
        default_select_map = mac_catalyst_select_map

    iphonesimulator_and_maccatalyst_select_map = iphonesimulator_select_map
    if len(mac_catalyst_select_map) > 0:
        if len(iphonesimulator_and_maccatalyst_select_map) > 0:
            iphonesimulator_and_maccatalyst_select_map = {
                "DEFAULT": select(iphonesimulator_select_map),
                "ovr_config//runtime/constraints:maccatalyst": select(mac_catalyst_select_map),
            }
        else:
            iphonesimulator_and_maccatalyst_select_map = mac_catalyst_select_map

    iphone_and_maccatalyst_select_map = iphone_select_map
    if len(mac_catalyst_select_map) > 0:
        if len(iphone_and_maccatalyst_select_map) > 0:
            iphone_and_maccatalyst_select_map = {
                "DEFAULT": select(iphone_select_map),
                "ovr_config//runtime/constraints:maccatalyst": select(mac_catalyst_select_map),
            }
        else:
            iphone_and_maccatalyst_select_map = mac_catalyst_select_map

    # `actual` target found via select process mirroring the select_map from
    # https://www.internalfb.com/code/fbsource/[ae3c2d74615b]/xplat/buck2/platform/apple/toolchain/apple_toolchain_core.bzl?lines=100-118

    select_map = {"DEFAULT": select(default_select_map)}

    if len(iphonesimulator_and_maccatalyst_select_map) > 0:
        select_map.update({"ovr_config//os:iphoneos": select(iphonesimulator_and_maccatalyst_select_map)})
        select_map.update({"ovr_config//os/sdk/apple:iphonesimulator": select(iphonesimulator_and_maccatalyst_select_map)})

    if len(iphone_and_maccatalyst_select_map) > 0:
        select_map.update({"ovr_config//os/sdk/apple:iphoneos": select(iphone_and_maccatalyst_select_map)})

    if len(mac_select_map) > 0:
        select_map.update({"ovr_config//os:macos": select(mac_select_map)})

    alias_rule(
        name = name,
        actual = select(select_map),
        licenses = kwargs.get("licenses", None),
        labels = kwargs.get("labels", None),
        default_target_platform = default_target_platform,
        contacts = kwargs.get("contacts", None),
        visibility = kwargs.get("visibility", None),
        compatible_with = compatible_with,
    )

def _parse_platform(name):
    # parse metadata from conventional xcframework folders
    # produced by xcodebuild -create-xcframework
    #
    # examples:
    #
    # ios-arm64
    # ios-arm64_x86_64-simulator
    # ios-arm64_x86_64-maccatalyst
    # macos-arm64_x86_64
    substrings = name.split("-")
    isCatalyst = False
    isSimulator = False
    if len(substrings) > 2:
        isSimulator = substrings[2] == "simulator"
        isCatalyst = substrings[2] == "maccatalyst"
    archs = []
    for arch in ["arm64", "x86_64", "arm64_32"]:
        if arch in substrings[1]:
            archs.append(arch)
    if not archs:
        fail("Failed to parse architectures for xcframework platform " + name)

    return {
        "archs": archs,
        "folder": name,
        "isCatalyst": isCatalyst,
        "isSimulator": isSimulator,
        "platform": substrings[0],
    }
