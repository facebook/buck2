AppleToolchainRuleType = enum(
    # Represents apple_toolchain()
    "apple",
    # Represents cxx_toolchain()
    "cxx",
)

AppleToolchainType = enum(
    "apple-xcode-current-macos",
    "meta-pika-13.3-linux",
    "meta-pika-13.3-macos",
    "meta-xcode-macos",
)

AppleToolchainUsageType = enum(
    "generic",
    "resources",
)

_APPLE_TOOLCHAIN_RULE_TYPE = AppleToolchainRuleType("apple")
_CXX_TOOLCHAIN_RULE_TYPE = AppleToolchainRuleType("cxx")

_APPLE_XCODE_CURRENT_TOOLCHAIN_TYPE = AppleToolchainType("apple-xcode-current-macos")
_META_PIKA_13_3_LINUX_TOOLCHAIN_TYPE = AppleToolchainType("meta-pika-13.3-linux")
_META_PIKA_13_3_MACOS_TOOLCHAIN_TYPE = AppleToolchainType("meta-pika-13.3-macos")
_META_XCODE_MACOS_TOOLCHAIN_TYPE = AppleToolchainType("meta-xcode-macos")

_GENERIC_USAGE_TYPE = AppleToolchainUsageType("generic")
_RESOURCES_USAGE_TYPE = AppleToolchainUsageType("resources")

def get_apple_cxx_select_map():
    return _get_apple_select_map(include_default = False, rule_type = _CXX_TOOLCHAIN_RULE_TYPE, usage_type = _GENERIC_USAGE_TYPE)

def default_apple_toolchain():
    select_map = _get_apple_select_map(include_default = True, rule_type = _APPLE_TOOLCHAIN_RULE_TYPE, usage_type = _GENERIC_USAGE_TYPE)
    return select(select_map)

def _get_apple_select_map(include_default: bool.type, rule_type: AppleToolchainRuleType.type, usage_type: AppleToolchainUsageType.type):
    default_arch = _get_default_arch_for_macos_and_simulator_targets()
    select_map = {
        # SDK forces OS, `iphoneos` means proper device iOS build
        "ovr_config//os/sdk/apple:iphoneos": _get_iphone_device_toolchain_select_map(rule_type = rule_type, usage_type = usage_type),
        "ovr_config//os/sdk/apple:iphonesimulator": _get_iphone_simulator_toolchain_select_map(rule_type = rule_type, usage_type = usage_type, default_arch = default_arch),
        # SDK forces OS, `watchos` means proper device watchOS build
        "ovr_config//os/sdk/apple:watchos": _get_watchos_device_toolchain_select_map(rule_type = rule_type, usage_type = usage_type),
        "ovr_config//os/sdk/apple:watchsimulator": _get_watch_simulator_toolchain_select_map(rule_type = rule_type, usage_type = usage_type, default_arch = default_arch),
        # `iphoneos` OS constraint allows both device and simulator builds, default to simulator if SDK is not specified
        "ovr_config//os:iphoneos": _get_iphone_simulator_toolchain_select_map(rule_type = rule_type, usage_type = usage_type, default_arch = default_arch),
        "ovr_config//os:macos": _get_apple_macos_select_map(rule_type = rule_type, usage_type = usage_type, default_arch = default_arch),
        # `watchos` OS constraint allows both device and simulator builds, default to simulator if SDK is not specified
        "ovr_config//os:watchos": _get_watch_simulator_toolchain_select_map(rule_type = rule_type, usage_type = usage_type, default_arch = default_arch),
    }

    if include_default:
        select_map["DEFAULT"] = _get_iphone_simulator_toolchain_select_map(rule_type = rule_type, usage_type = usage_type, default_arch = default_arch)

    return select_map

def _get_toolchain_select_config(toolchain_type: AppleToolchainType.type, usage_type: AppleToolchainUsageType.type) -> str.type:
    config_suffix = "-for-resources" if usage_type == _RESOURCES_USAGE_TYPE else ""
    config = toolchain_type.value + config_suffix
    return "fbsource//xplat/buck2/platform/apple/config:{}".format(config)

def _get_iphone_device_toolchain_select_map(rule_type: AppleToolchainRuleType.type, usage_type: AppleToolchainUsageType.type):
    select_map = {
        "DEFAULT": _get_apple_iphone_device_arch_select(rule_type = rule_type),
        _get_toolchain_select_config(toolchain_type = _APPLE_XCODE_CURRENT_TOOLCHAIN_TYPE, usage_type = usage_type): _get_apple_iphone_device_arch_select(rule_type = rule_type),
        _get_toolchain_select_config(toolchain_type = _META_PIKA_13_3_LINUX_TOOLCHAIN_TYPE, usage_type = usage_type): _get_pika_arch_select(rule_type = rule_type, toolchain_name = "pika-13.3", host = "linux", sdk = "iphoneos"),
        _get_toolchain_select_config(toolchain_type = _META_PIKA_13_3_MACOS_TOOLCHAIN_TYPE, usage_type = usage_type): _get_pika_arch_select(rule_type = rule_type, toolchain_name = "pika-13.3", host = "macos", sdk = "iphoneos"),
        _get_toolchain_select_config(toolchain_type = _META_XCODE_MACOS_TOOLCHAIN_TYPE, usage_type = usage_type): _get_pika_arch_select(rule_type = rule_type, toolchain_name = "xcode", host = "macos", sdk = "iphoneos"),
    }

    return select(select_map)

def _get_watch_simulator_toolchain_select_map(rule_type: AppleToolchainRuleType.type, usage_type: AppleToolchainUsageType.type, default_arch: str.type):
    select_map = {
        "DEFAULT": _get_apple_watch_simulator_arch_select(rule_type = rule_type, default_arch = default_arch),
        _get_toolchain_select_config(toolchain_type = _APPLE_XCODE_CURRENT_TOOLCHAIN_TYPE, usage_type = usage_type): _get_apple_watch_simulator_arch_select(rule_type = rule_type, default_arch = default_arch),
        _get_toolchain_select_config(toolchain_type = _META_PIKA_13_3_LINUX_TOOLCHAIN_TYPE, usage_type = usage_type): _get_pika_arch_select(rule_type = rule_type, toolchain_name = "pika-13.3", host = "linux", sdk = "watchsimulator"),
        _get_toolchain_select_config(toolchain_type = _META_PIKA_13_3_MACOS_TOOLCHAIN_TYPE, usage_type = usage_type): _get_pika_arch_select(rule_type = rule_type, toolchain_name = "pika-13.3", host = "macos", sdk = "watchsimulator"),
        _get_toolchain_select_config(toolchain_type = _META_XCODE_MACOS_TOOLCHAIN_TYPE, usage_type = usage_type): _get_pika_arch_select(rule_type = rule_type, toolchain_name = "xcode", host = "macos", sdk = "watchsimulator"),
    }

    return select(select_map)

def _get_watchos_device_toolchain_select_map(rule_type: AppleToolchainRuleType.type, usage_type: AppleToolchainUsageType.type):
    select_map = {
        "DEFAULT": _get_apple_watchos_device_arch_select(rule_type = rule_type),
        _get_toolchain_select_config(toolchain_type = _APPLE_XCODE_CURRENT_TOOLCHAIN_TYPE, usage_type = usage_type): _get_apple_watchos_device_arch_select(rule_type = rule_type),
        _get_toolchain_select_config(toolchain_type = _META_PIKA_13_3_LINUX_TOOLCHAIN_TYPE, usage_type = usage_type): _get_pika_arch_select(rule_type = rule_type, toolchain_name = "pika-13.3", host = "linux", sdk = "watchos"),
        _get_toolchain_select_config(toolchain_type = _META_PIKA_13_3_MACOS_TOOLCHAIN_TYPE, usage_type = usage_type): _get_pika_arch_select(rule_type = rule_type, toolchain_name = "pika-13.3", host = "macos", sdk = "watchos"),
        _get_toolchain_select_config(toolchain_type = _META_XCODE_MACOS_TOOLCHAIN_TYPE, usage_type = usage_type): _get_pika_arch_select(rule_type = rule_type, toolchain_name = "xcode", host = "macos", sdk = "watchos"),
    }

    return select(select_map)

def _get_iphone_simulator_toolchain_select_map(rule_type: AppleToolchainRuleType.type, usage_type: AppleToolchainUsageType.type, default_arch: str.type):
    select_map = {
        "DEFAULT": _get_apple_iphone_simulator_arch_select(rule_type = rule_type, default_arch = default_arch),
        _get_toolchain_select_config(toolchain_type = _APPLE_XCODE_CURRENT_TOOLCHAIN_TYPE, usage_type = usage_type): _get_apple_iphone_simulator_arch_select(rule_type = rule_type, default_arch = default_arch),
        _get_toolchain_select_config(toolchain_type = _META_PIKA_13_3_LINUX_TOOLCHAIN_TYPE, usage_type = usage_type): _get_pika_arch_select(rule_type = rule_type, toolchain_name = "pika-13.3", host = "linux", sdk = "iphonesimulator"),
        _get_toolchain_select_config(toolchain_type = _META_PIKA_13_3_MACOS_TOOLCHAIN_TYPE, usage_type = usage_type): _get_pika_arch_select(rule_type = rule_type, toolchain_name = "pika-13.3", host = "macos", sdk = "iphonesimulator"),
        _get_toolchain_select_config(toolchain_type = _META_XCODE_MACOS_TOOLCHAIN_TYPE, usage_type = usage_type): _get_pika_arch_select(rule_type = rule_type, toolchain_name = "xcode", host = "macos", sdk = "iphonesimulator"),
    }

    return select(select_map)

def _get_apple_macos_select_map(rule_type: AppleToolchainRuleType.type, usage_type: AppleToolchainUsageType.type, default_arch: str.type):
    select_map = {
        # Minimal Xcode takes priority over toolchain selection, so we have nested DEFAULT statements
        "DEFAULT": select({
            "DEFAULT": _get_apple_macosx_arch_select(rule_type = rule_type, default_arch = default_arch),
            _get_toolchain_select_config(toolchain_type = _APPLE_XCODE_CURRENT_TOOLCHAIN_TYPE, usage_type = usage_type): _get_apple_macosx_arch_select(rule_type = rule_type, default_arch = default_arch),
            _get_toolchain_select_config(toolchain_type = _META_PIKA_13_3_LINUX_TOOLCHAIN_TYPE, usage_type = usage_type): _get_pika_arch_select(rule_type = rule_type, toolchain_name = "pika-13.3", host = "linux", sdk = "macosx"),
            _get_toolchain_select_config(toolchain_type = _META_PIKA_13_3_MACOS_TOOLCHAIN_TYPE, usage_type = usage_type): _get_pika_arch_select(rule_type = rule_type, toolchain_name = "pika-13.3", host = "macos", sdk = "macosx"),
            _get_toolchain_select_config(toolchain_type = _META_XCODE_MACOS_TOOLCHAIN_TYPE, usage_type = usage_type): _get_pika_arch_select(rule_type = rule_type, toolchain_name = "xcode", host = "macos", sdk = "macosx"),
        }),
    }

    if rule_type == _CXX_TOOLCHAIN_RULE_TYPE:
        # Minimal Xcode does not provide an apple_toolchain(), only cxx_toolchain()
        select_map["ovr_config//toolchain/fb/constraints:macos-minimal"] = "fbsource//xplat/toolchains/minimal_xcode:macosx-x86_64_minimal_xcode"

    return select(select_map)

def _get_default_arch_for_macos_and_simulator_targets():
    use_default_host_based_target_arch = read_config("apple", "default_host_based_target_arch", True)
    if not use_default_host_based_target_arch:
        return "x86_64"

    if host_info().os.is_macos:
        if host_info().arch.is_x86_64:
            return "x86_64"
        else:
            return "arm64"
    else:
        # On non-macOS hosts, we don't know what the target arch should be,
        # so just default to x86 which can work on both ARM64 (using Rosetta) and x86 Macs.
        return "x86_64"

# apple-xcode-current-macos

def _get_apple_xcode_current_suffix(rule_type: AppleToolchainRuleType.type) -> str.type:
    if rule_type == _APPLE_TOOLCHAIN_RULE_TYPE:
        return "-apple-toolchain"
    elif rule_type == _CXX_TOOLCHAIN_RULE_TYPE:
        return ""
    else:
        fail("Unknown toolchain type: {}".format(rule_type))

def _get_apple_watch_simulator_arch_select(rule_type: AppleToolchainRuleType.type, default_arch: str.type) -> "selector":
    suffix = _get_apple_xcode_current_suffix(rule_type)
    return select({
        "DEFAULT": "fbsource//xplat/buck2/platform/apple:buck2-apple-watchsimulator-" + default_arch + suffix,
        "ovr_config//cpu/constraints:arm64": "fbsource//xplat/buck2/platform/apple:buck2-apple-watchsimulator-arm64" + suffix,
        "ovr_config//cpu/constraints:x86_64": "fbsource//xplat/buck2/platform/apple:buck2-apple-watchsimulator-x86_64" + suffix,
    })

def _get_apple_macosx_arch_select(rule_type: AppleToolchainRuleType.type, default_arch: str.type) -> "selector":
    suffix = _get_apple_xcode_current_suffix(rule_type)
    return select({
        "DEFAULT": "fbsource//xplat/buck2/platform/apple:buck2-apple-macosx-" + default_arch + suffix,
        "ovr_config//cpu/constraints:arm64": "fbsource//xplat/buck2/platform/apple:buck2-apple-macosx-arm64" + suffix,
        "ovr_config//cpu/constraints:x86_64": "fbsource//xplat/buck2/platform/apple:buck2-apple-macosx-x86_64" + suffix,
    })

def _get_apple_iphone_simulator_arch_select(rule_type: AppleToolchainRuleType.type, default_arch: str.type) -> "selector":
    suffix = _get_apple_xcode_current_suffix(rule_type)
    return select({
        "DEFAULT": "fbsource//xplat/buck2/platform/apple:buck2-apple-iphonesimulator-" + default_arch + suffix,
        "ovr_config//cpu/constraints:arm64": "fbsource//xplat/buck2/platform/apple:buck2-apple-iphonesimulator-arm64" + suffix,
        "ovr_config//cpu/constraints:x86_64": "fbsource//xplat/buck2/platform/apple:buck2-apple-iphonesimulator-x86_64" + suffix,
    })

def _get_apple_iphone_device_arch_select(rule_type: AppleToolchainRuleType.type) -> "selector":
    suffix = _get_apple_xcode_current_suffix(rule_type)
    return select({
        "DEFAULT": "fbsource//xplat/buck2/platform/apple:buck2-apple-iphoneos-arm64" + suffix,
        "ovr_config//cpu/constraints:arm64": "fbsource//xplat/buck2/platform/apple:buck2-apple-iphoneos-arm64" + suffix,
    })

def _get_apple_watchos_device_arch_select(rule_type: AppleToolchainRuleType.type) -> "selector":
    suffix = _get_apple_xcode_current_suffix(rule_type)
    return select({
        "DEFAULT": "fbsource//xplat/buck2/platform/apple:buck2-apple-watchos-arm64_32" + suffix,
        "ovr_config//cpu/constraints:arm64": "fbsource//xplat/buck2/platform/apple:buck2-apple-watchos-arm64_32" + suffix,
    })

# Meta Pika

def _get_pika_toolchain_suffix(rule_type: AppleToolchainRuleType.type) -> str.type:
    if rule_type == _APPLE_TOOLCHAIN_RULE_TYPE:
        return "-apple-toolchain"
    elif rule_type == _CXX_TOOLCHAIN_RULE_TYPE:
        return "-cxx-toolchain"
    else:
        fail("Unknown toolchain type: {}".format(rule_type))

def _get_pika_toolchain_target(rule_type: AppleToolchainRuleType.type, toolchain_name: str.type, host: str.type, sdk_name: str.type, arch: str.type) -> str.type:
    suffix = _get_pika_toolchain_suffix(rule_type)
    xbat = (toolchain_name == "xcode")
    if xbat:
        if host != "macos":
            fail("Xcode-backed toolchains are always macOS based")
        return "fbsource//xplat/toolchains/apple:{}-{}-{}".format(toolchain_name, sdk_name, arch) + suffix
    else:
        return "fbsource//xplat/toolchains/jackalope:{}-{}-{}-{}".format(toolchain_name, host, sdk_name, arch) + suffix

def _get_pika_arch_select(rule_type: AppleToolchainRuleType.type, toolchain_name: str.type, host: str.type, sdk: str.type) -> "selector":
    default_arch = _get_default_arch_for_macos_and_simulator_targets()
    sdk_constraint_map = {
        "iphoneos": struct(
            default_arch = "arm64",
            constraint_arch_map = {
                "ovr_config//cpu/constraints:arm64": "arm64",
            },
        ),
        "iphonesimulator": struct(
            default_arch = default_arch,
            constraint_arch_map = {
                "ovr_config//cpu/constraints:arm64": "arm64",
                "ovr_config//cpu/constraints:x86_64": "x86_64",
            },
        ),
        "macosx": struct(
            default_arch = default_arch,
            constraint_arch_map = {
                "ovr_config//cpu/constraints:arm64": "arm64",
                "ovr_config//cpu/constraints:x86_64": "x86_64",
            },
        ),
        "watchos": struct(
            default_arch = "arm64_32",
            constraint_arch_map = {
                "ovr_config//cpu/constraints:arm64": "arm64_32",
            },
        ),
        "watchsimulator": struct(
            default_arch = default_arch,
            constraint_arch_map = {
                "ovr_config//cpu/constraints:arm64": "arm64",
                "ovr_config//cpu/constraints:x86_64": "x86_64",
            },
        ),
    }

    sdk_info = sdk_constraint_map[sdk]
    sdk_constraint_map = {
        "DEFAULT": _get_pika_toolchain_target(rule_type, toolchain_name, host, sdk, sdk_info.default_arch),
    }

    for (constraint_target, arch) in sdk_info.constraint_arch_map.items():
        sdk_constraint_map[constraint_target] = _get_pika_toolchain_target(rule_type, toolchain_name, host, sdk, arch)

    return select(sdk_constraint_map)
