AppleToolchainRuleType = enum(
    # Represents apple_toolchain()
    "apple",
    # Represents cxx_toolchain()
    "cxx",
)

AppleToolchainType = enum(
    "apple-xcode-13.4-macos",
    "apple-xcode-14.0-macos",
    "meta-pika-13.3-linux",
    "meta-pika-13.3-macos",
    "meta-pika-14-linux",
    "meta-pika-14-macos",
    "meta-pika-14-fat",
    "meta-xcode-macos",
)

AppleToolchainUsageType = enum(
    "generic",
    "resources",
)

_APPLE_TOOLCHAIN_RULE_TYPE = AppleToolchainRuleType("apple")
_CXX_TOOLCHAIN_RULE_TYPE = AppleToolchainRuleType("cxx")

_APPLE_XCODE_13_4_TOOLCHAIN_TYPE = AppleToolchainType("apple-xcode-13.4-macos")
_APPLE_XCODE_14_0_TOOLCHAIN_TYPE = AppleToolchainType("apple-xcode-14.0-macos")
_META_PIKA_13_3_LINUX_TOOLCHAIN_TYPE = AppleToolchainType("meta-pika-13.3-linux")
_META_PIKA_13_3_MACOS_TOOLCHAIN_TYPE = AppleToolchainType("meta-pika-13.3-macos")
_META_PIKA_14_LINUX_TOOLCHAIN_TYPE = AppleToolchainType("meta-pika-14-linux")
_META_PIKA_14_MACOS_TOOLCHAIN_TYPE = AppleToolchainType("meta-pika-14-macos")
_META_PIKA_14_FAT_TOOLCHAIN_TYPE = AppleToolchainType("meta-pika-14-fat")
_META_XCODE_MACOS_TOOLCHAIN_TYPE = AppleToolchainType("meta-xcode-macos")

_GENERIC_USAGE_TYPE = AppleToolchainUsageType("generic")
_RESOURCES_USAGE_TYPE = AppleToolchainUsageType("resources")

_DEFAULT_XCODE_VERSION = "14.0"

def get_apple_cxx_select_map():
    return _get_apple_select_map(include_default = False, rule_type = _CXX_TOOLCHAIN_RULE_TYPE, usage_type = _GENERIC_USAGE_TYPE)

def default_apple_toolchain():
    select_map = _get_apple_select_map(include_default = True, rule_type = _APPLE_TOOLCHAIN_RULE_TYPE, usage_type = _GENERIC_USAGE_TYPE)
    return select(select_map)

def resources_apple_toolchain():
    select_map = _get_apple_select_map(include_default = True, rule_type = _APPLE_TOOLCHAIN_RULE_TYPE, usage_type = _RESOURCES_USAGE_TYPE)
    return select(select_map)

def default_apple_xctoolchain_bundle_id():
    return _default_apple_xctoolchain(bundle_id_target = True)

def default_apple_xctoolchain():
    return _default_apple_xctoolchain(bundle_id_target = False)

def _default_apple_xctoolchain(bundle_id_target: bool.type):
    select_map = {"DEFAULT": None}

    toolchain_type_to_name_map = {
        _META_PIKA_13_3_LINUX_TOOLCHAIN_TYPE: "pika-13.3",
        _META_PIKA_13_3_MACOS_TOOLCHAIN_TYPE: "pika-13.3",
        _META_PIKA_14_LINUX_TOOLCHAIN_TYPE: "pika-14",
        _META_PIKA_14_MACOS_TOOLCHAIN_TYPE: "pika-14",
        _META_PIKA_14_FAT_TOOLCHAIN_TYPE: "pika-14",
    }

    for (toolchain_type, toolchain_name) in toolchain_type_to_name_map.items():
        config_key = _get_toolchain_select_config(toolchain_type = toolchain_type, usage_type = _GENERIC_USAGE_TYPE)
        toolchain_target = _get_xctoolchain_target(toolchain_name = toolchain_name, bundle_id_target = bundle_id_target)
        select_map[config_key] = toolchain_target

    return select(select_map)

def _get_xctoolchain_target(toolchain_name: str.type, bundle_id_target: bool.type) -> str.type:
    # xctoolchain is an Xcode-specific toolchain bundle, so we always pick macOS-hosted ones, as Xcode only runs on macOS.
    suffix = "-bundle-id" if bundle_id_target else ""
    return "fbsource//xplat/toolchains/facebook-dt:{}-macos-noasserts-focus-xctoolchain".format(toolchain_name) + suffix

def _get_apple_select_map(include_default: bool.type, rule_type: AppleToolchainRuleType.type, usage_type: AppleToolchainUsageType.type):
    select_map = {
        # SDK forces OS, `iphoneos` means proper device iOS build
        "ovr_config//os/sdk/apple:iphoneos": _get_iphone_device_toolchain_select_map(rule_type = rule_type, usage_type = usage_type),
        "ovr_config//os/sdk/apple:iphonesimulator": _get_iphone_simulator_toolchain_select_map(rule_type = rule_type, usage_type = usage_type),
        # SDK forces OS, `watchos` means proper device watchOS build
        "ovr_config//os/sdk/apple:watchos": _get_watchos_device_toolchain_select_map(rule_type = rule_type, usage_type = usage_type),
        "ovr_config//os/sdk/apple:watchsimulator": _get_watch_simulator_toolchain_select_map(rule_type = rule_type, usage_type = usage_type),
        # `iphoneos` OS constraint allows both device and simulator builds, default to simulator if SDK is not specified
        "ovr_config//os:iphoneos": _get_iphone_simulator_toolchain_select_map(rule_type = rule_type, usage_type = usage_type),
        "ovr_config//os:macos": _get_apple_macos_select_map(rule_type = rule_type, usage_type = usage_type),
        # `watchos` OS constraint allows both device and simulator builds, default to simulator if SDK is not specified
        "ovr_config//os:watchos": _get_watch_simulator_toolchain_select_map(rule_type = rule_type, usage_type = usage_type),
    }

    if include_default:
        select_map["DEFAULT"] = _get_iphone_simulator_toolchain_select_map(rule_type = rule_type, usage_type = usage_type)

    return select_map

def _get_unversioned_xcode_toolchain_select_config(usage_type: AppleToolchainUsageType.type) -> str.type:
    config_suffix = "-for-resources" if usage_type == _RESOURCES_USAGE_TYPE else ""
    return "fbsource//xplat/buck2/platform/apple/config:apple-xcode-unversioned-macos".format(config_suffix)

def _get_toolchain_select_config(toolchain_type: AppleToolchainType.type, usage_type: AppleToolchainUsageType.type) -> str.type:
    config_suffix = "-for-resources" if usage_type == _RESOURCES_USAGE_TYPE else ""
    config = toolchain_type.value + config_suffix
    return "fbsource//xplat/buck2/platform/apple/config:{}".format(config)

def _get_toolchain_select_map_with_xcode(rule_type: AppleToolchainRuleType.type, usage_type: AppleToolchainUsageType.type, sdk: str.type, xcode_based: bool.type) -> {str.type: "selector"}:
    select_map = {}
    for toolchain_type in AppleToolchainType:
        toolchain_info = _APPLE_TOOLCHAINS[toolchain_type]
        if sdk in toolchain_info.sdks and toolchain_info.xcode_based == xcode_based:
            config = _get_toolchain_select_config(toolchain_type = toolchain_type, usage_type = usage_type)
            selector = toolchain_info.selector(rule_type = rule_type, sdk = sdk, **toolchain_info.selector_args)
            select_map[config] = selector

    return select_map

def _get_toolchain_select_map(rule_type: AppleToolchainRuleType.type, usage_type: AppleToolchainUsageType.type, sdk: str.type) -> {str.type: "selector"}:
    xcode_select_map = _get_toolchain_select_map_with_xcode(rule_type = rule_type, usage_type = usage_type, sdk = sdk, xcode_based = True)
    non_xcode_select_map = _get_toolchain_select_map_with_xcode(rule_type = rule_type, usage_type = usage_type, sdk = sdk, xcode_based = False)

    combined_select_map = {}
    combined_select_map.update(non_xcode_select_map)

    unversioned_xcode_select_config = _get_unversioned_xcode_toolchain_select_config(usage_type = usage_type)
    combined_select_map[unversioned_xcode_select_config] = select(xcode_select_map)

    return combined_select_map

def _get_default_global_toolchain_fallback(rule_type: AppleToolchainRuleType.type, sdk: str.type) -> "selector":
    # This is the default toolchain fallback in case _no_ toolchain has been specified at all
    return _get_apple_xcode_arch_select(rule_type = rule_type, sdk = sdk, xcode_version = _DEFAULT_XCODE_VERSION)

def _get_iphone_device_toolchain_select_map(rule_type: AppleToolchainRuleType.type, usage_type: AppleToolchainUsageType.type):
    select_map = _get_toolchain_select_map(rule_type = rule_type, usage_type = usage_type, sdk = "iphoneos")
    select_map["DEFAULT"] = _get_default_global_toolchain_fallback(rule_type = rule_type, sdk = "iphoneos")
    return select(select_map)

def _get_watch_simulator_toolchain_select_map(rule_type: AppleToolchainRuleType.type, usage_type: AppleToolchainUsageType.type):
    select_map = _get_toolchain_select_map(rule_type = rule_type, usage_type = usage_type, sdk = "watchsimulator")
    select_map["DEFAULT"] = _get_default_global_toolchain_fallback(rule_type = rule_type, sdk = "watchsimulator")
    return select(select_map)

def _get_watchos_device_toolchain_select_map(rule_type: AppleToolchainRuleType.type, usage_type: AppleToolchainUsageType.type):
    select_map = _get_toolchain_select_map(rule_type = rule_type, usage_type = usage_type, sdk = "watchos")
    select_map["DEFAULT"] = _get_default_global_toolchain_fallback(rule_type = rule_type, sdk = "watchos")
    return select(select_map)

def _get_iphone_simulator_toolchain_select_map(rule_type: AppleToolchainRuleType.type, usage_type: AppleToolchainUsageType.type):
    select_map = _get_toolchain_select_map(rule_type = rule_type, usage_type = usage_type, sdk = "iphonesimulator")
    select_map["DEFAULT"] = _get_default_global_toolchain_fallback(rule_type = rule_type, sdk = "iphonesimulator")
    return select(select_map)

def _get_apple_macos_select_map(rule_type: AppleToolchainRuleType.type, usage_type: AppleToolchainUsageType.type):
    inner_select_map = _get_toolchain_select_map(rule_type = rule_type, usage_type = usage_type, sdk = "macosx")
    inner_select_map["DEFAULT"] = _get_default_global_toolchain_fallback(rule_type = rule_type, sdk = "macosx")

    select_map = {
        # Minimal Xcode takes priority over toolchain selection, so we have nested DEFAULT statements
        "DEFAULT": select(inner_select_map),
    }

    if rule_type == _CXX_TOOLCHAIN_RULE_TYPE:
        # Minimal Xcode does not provide an apple_toolchain(), only cxx_toolchain()
        select_map["ovr_config//toolchain/fb/constraints:macos-minimal"] = "fbsource//xplat/toolchains/minimal_xcode:macosx-x86_64_minimal_xcode"

    return select(select_map)

def _get_default_arch_for_macos_and_simulator_targets():
    # Due to our test infra being x86, they cannot execute arm64 tests, so default to building x86 artifacts.
    use_default_host_based_target_arch = native.read_config("apple", "default_host_based_target_arch", False)
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

# apple-xcode-13.4-macos

def _get_apple_xcode_suffix(rule_type: AppleToolchainRuleType.type) -> str.type:
    if rule_type == _APPLE_TOOLCHAIN_RULE_TYPE:
        return "-apple-toolchain"
    elif rule_type == _CXX_TOOLCHAIN_RULE_TYPE:
        return ""
    else:
        fail("Unknown toolchain type: {}".format(rule_type))

def _get_apple_xcode_arch_select(rule_type: AppleToolchainRuleType.type, sdk: str.type, xcode_version: str.type) -> "selector":
    return _get_generic_arch_select(rule_type = rule_type, sdk = sdk, generator = _get_apple_xcode_toolchain_target, xcode_version = xcode_version)

def _get_apple_xcode_toolchain_target(rule_type: AppleToolchainRuleType.type, sdk: str.type, arch: str.type, xcode_version: str.type):
    suffix = _get_apple_xcode_suffix(rule_type)
    return "fbsource//xplat/buck2/platform/apple:buck2-apple-xcode-{}-{}-{}".format(xcode_version, sdk, arch) + suffix

# Meta Pika

def _get_pika_toolchain_suffix(rule_type: AppleToolchainRuleType.type) -> str.type:
    if rule_type == _APPLE_TOOLCHAIN_RULE_TYPE:
        return "-apple-toolchain"
    elif rule_type == _CXX_TOOLCHAIN_RULE_TYPE:
        return "-cxx-toolchain"
    else:
        fail("Unknown toolchain type: {}".format(rule_type))

def _get_pika_toolchain_target(rule_type: AppleToolchainRuleType.type, toolchain_name: str.type, host: str.type, sdk: str.type, arch: str.type) -> str.type:
    suffix = _get_pika_toolchain_suffix(rule_type)
    xbat = (toolchain_name == "xcode")
    if xbat:
        if host != "macos":
            fail("Xcode-backed toolchains are always macOS based")
        return "fbsource//xplat/toolchains/apple:{}-{}-{}".format(toolchain_name, sdk, arch) + suffix
    else:
        return "fbsource//xplat/toolchains/jackalope:{}-{}-{}-{}".format(toolchain_name, host, sdk, arch) + suffix

def _get_generic_arch_select(rule_type: AppleToolchainRuleType.type, sdk: str.type, generator: "function", **kwargs) -> "selector":
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
        "DEFAULT": generator(rule_type = rule_type, sdk = sdk, arch = sdk_info.default_arch, **kwargs),
    }

    for (constraint_target, arch) in sdk_info.constraint_arch_map.items():
        sdk_constraint_map[constraint_target] = generator(rule_type = rule_type, sdk = sdk, arch = arch, **kwargs)

    return select(sdk_constraint_map)

def _get_pika_arch_select(rule_type: AppleToolchainRuleType.type, toolchain_name: str.type, host: str.type, sdk: str.type) -> "selector":
    return _get_generic_arch_select(rule_type = rule_type, sdk = sdk, generator = _get_pika_toolchain_target, toolchain_name = toolchain_name, host = host)

# Toolchains need to be defined _after_ the selector function has been defined.
# watchOS targets must always be built with Xcode-based toolchains due to lack
# of upstream LLVM support for the archs.

_APPLE_TOOLCHAINS = {
    _APPLE_XCODE_13_4_TOOLCHAIN_TYPE: struct(
        sdks = ["iphoneos", "iphonesimulator", "macosx", "watchos", "watchsimulator"],
        selector = _get_apple_xcode_arch_select,
        selector_args = {
            "xcode_version": "13.4",
        },
        xcode_based = True,
    ),
    _APPLE_XCODE_14_0_TOOLCHAIN_TYPE: struct(
        sdks = ["iphoneos", "iphonesimulator", "macosx", "watchos", "watchsimulator"],
        selector = _get_apple_xcode_arch_select,
        selector_args = {
            "xcode_version": "14.0",
        },
        xcode_based = True,
    ),
    # Pika (thin)
    _META_PIKA_13_3_LINUX_TOOLCHAIN_TYPE: struct(
        sdks = ["iphoneos", "iphonesimulator", "macosx"],
        selector = _get_pika_arch_select,
        selector_args = {
            "host": "linux",
            "toolchain_name": "pika-13.3",
        },
        xcode_based = False,
    ),
    _META_PIKA_13_3_MACOS_TOOLCHAIN_TYPE: struct(
        sdks = ["iphoneos", "iphonesimulator", "macosx"],
        selector = _get_pika_arch_select,
        selector_args = {
            "host": "macos",
            "toolchain_name": "pika-13.3",
        },
        xcode_based = False,
    ),
    _META_PIKA_14_LINUX_TOOLCHAIN_TYPE: struct(
        sdks = ["iphoneos", "iphonesimulator", "macosx"],
        selector = _get_pika_arch_select,
        selector_args = {
            "host": "linux",
            "toolchain_name": "pika-14",
        },
        xcode_based = False,
    ),
    _META_PIKA_14_MACOS_TOOLCHAIN_TYPE: struct(
        sdks = ["iphoneos", "iphonesimulator", "macosx"],
        selector = _get_pika_arch_select,
        selector_args = {
            "host": "macos",
            "toolchain_name": "pika-14",
        },
        xcode_based = False,
    ),
    # Pika (fat)
    _META_PIKA_14_FAT_TOOLCHAIN_TYPE: struct(
        sdks = ["iphoneos", "iphonesimulator", "macosx"],
        selector = _get_pika_arch_select,
        selector_args = {
            "host": "fat",
            "toolchain_name": "pika-14",
        },
        xcode_based = False,
    ),
    # xbat
    _META_XCODE_MACOS_TOOLCHAIN_TYPE: struct(
        sdks = ["iphoneos", "iphonesimulator", "macosx", "watchos", "watchsimulator"],
        selector = _get_pika_arch_select,
        selector_args = {
            "host": "macos",
            "toolchain_name": "xcode",
        },
        xcode_based = False,
    ),
}
