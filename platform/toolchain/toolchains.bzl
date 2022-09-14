load(":apple_toolchain_setup.bzl", "get_apple_cxx_select_map")

# This module defines the toolchains for languages like C++, Python etc.
# Usually those rules have default attributes that
# point to targets in this package that use these definitions to select the actual toolchain.
# - e.g. `rust_library` has `_rust_toolchain` which points at
# `fbcode//buck2/platform/toolchain:rust that then uses
# `default_rust_toolchain()` to select the actual toolchain.
#
# Generally, this is a legacy structure and we are moving away from it. The recommendation would
# be that the toolchain targets themselves be declared using some rule that
# sets `is_toolchain_rule=True` and that the selects are moved into those
# toolchain definitions themselves. Until we do that, we effectively need to statically define a
# target for each possible toolchain configuration.

def _merge_dictionaries(dicts):
    result = {}
    for d in dicts:
        for key, value in d.items():
            if key in result:
                fail("Duplicate key: '{}' while merging dictionaries".format(key))
            result[key] = value

    return result

# The default platform for fbcode rules that didn't specify it properly
_default_fbcode_platform = native.read_config("fbcode", "platform", "platform010")

def _get_fbcode_select_map(prefix):
    """
    Return a map to use to select fbcode C++ based toolchain definitions.
    """
    supports_aarch64 = prefix != "haskell"
    prefix = "fbcode//buck2/platform:{}-".format(prefix)
    normal = {
        # TODO: Ideally we don't need the `DEFAULT` clause, but `.buckconfig`s
        # set the coarser fbcode platform to work with v1, which leaks into the
        # few rules which don't set a more granular default.
        "DEFAULT": prefix + _default_fbcode_platform + "-clang",
        "ovr_config//toolchain/fb:platform009-clang": prefix + "platform009-clang",
        "ovr_config//toolchain/fb:platform009-clang-nosan": prefix + "platform009-clang-nosan",
        "ovr_config//toolchain/fb:platform009-clang-nosan-split-dwarf": prefix + "platform009-clang-nosan-split-dwarf",
        "ovr_config//toolchain/fb:platform009-clang-split-dwarf": prefix + "platform009-clang-split-dwarf",
        "ovr_config//toolchain/fb:platform009-clang12": prefix + "platform009-clang-12",
        "ovr_config//toolchain/fb:platform009-clang12-nosan": prefix + "platform009-clang-12-nosan",
        "ovr_config//toolchain/fb:platform009-clang12-split-dwarf": prefix + "platform009-clang-12-split-dwarf",
        "ovr_config//toolchain/fb:platform009-gcc": prefix + "platform009-gcc",
        "ovr_config//toolchain/fb:platform010-clang": prefix + "platform010-clang",
        "ovr_config//toolchain/fb:platform010-clang-nosan": prefix + "platform010-clang-nosan",
        "ovr_config//toolchain/fb:platform010-clang-nosan-split-dwarf": prefix + "platform010-clang-nosan-split-dwarf",
        "ovr_config//toolchain/fb:platform010-clang-split-dwarf": prefix + "platform010-clang-split-dwarf",
        "ovr_config//toolchain/fb:platform010-compat-clang": prefix + "platform010-compat-clang",
        "ovr_config//toolchain/fb:platform010-compat-clang-nosan": prefix + "platform010-compat-clang-nosan",
        "ovr_config//toolchain/fb:platform010-compat-clang-nosan-split-dwarf": prefix + "platform010-compat-clang-nosan-split-dwarf",
        "ovr_config//toolchain/fb:platform010-compat-clang-split-dwarf": prefix + "platform010-compat-clang-split-dwarf",
        "ovr_config//toolchain/fb:platform010-compat-gcc": prefix + "platform010-compat-gcc",
        "ovr_config//toolchain/fb:platform010-gcc": prefix + "platform010-gcc",
    }
    aarch64 = {} if not supports_aarch64 else {
        "ovr_config//toolchain/fb:platform010-aarch64-clang": prefix + "platform010-aarch64-clang",
        "ovr_config//toolchain/fb:platform010-aarch64-clang-nosan": prefix + "platform010-aarch64-clang-nosan",
        "ovr_config//toolchain/fb:platform010-aarch64-clang-nosan-split-dwarf": prefix + "platform010-aarch64-clang-nosan-split-dwarf",
        "ovr_config//toolchain/fb:platform010-aarch64-clang-split-dwarf": prefix + "platform010-aarch64-clang-split-dwarf",
        "ovr_config//toolchain/fb:platform010-aarch64-gcc": prefix + "platform010-aarch64-gcc",
    }
    return _merge_dictionaries([normal, aarch64])

def _get_android_cxx_select_map():
    return {
        "ovr_config//runtime:android-host-test-macos-minimal": "fbsource//xplat/toolchains/minimal_xcode:macosx-x86_64_minimal_xcode",
        "ovr_config//toolchain/fb:android-ndk": select({
            "DEFAULT": "fbsource//xplat/toolchains/android/ndk:cxx-toolchain-x86",
            "ovr_config//cpu/constraints:arm32": "fbsource//xplat/toolchains/android/ndk:cxx-toolchain-armv7",
            "ovr_config//cpu/constraints:arm64": "fbsource//xplat/toolchains/android/ndk:cxx-toolchain-arm64",
            "ovr_config//cpu/constraints:x86_32": "fbsource//xplat/toolchains/android/ndk:cxx-toolchain-x86",
            "ovr_config//cpu/constraints:x86_64": "fbsource//xplat/toolchains/android/ndk:cxx-toolchain-x86_64",
        }),
    }

def _get_apple_rust_select_map():
    return {
        "ovr_config//os:macos": "fbcode//buck2/platform:rust-macosx-x86_64_minimal_xcode",
    }

def _get_infer_select_map():
    return {"ovr_config//toolchain/fb:platform010-infer": "fbcode//buck2/platform:buck2-infer"}

def _get_lionhead_select_map():
    return {
        "ovr_config//toolchain/fb:platform010-lionhead-libfuzzer": "fbcode//buck2/platform:lionhead-libfuzzer-platform010-clang",
    }

def _get_sledge_select_map():
    return {"ovr_config//toolchain/fb:platform010-sledge": "fbcode//buck2/platform:buck2-sledge"}

def _get_xtensa_select_map():
    return {"ovr_config//toolchain/xtensa:xtensa": "fbsource//arvr/third-party/toolchains/xtensa:default-toolchain"}

def _get_infra_asic_fw_select_map():
    return {"ovr_config//toolchain/fb/constraints:infra-asic-fw-riscv64": "fbcode//buck2/platform:infra-asic-fw-toolchain"}

def default_cxx_toolchain_inner():
    return select(
        _merge_dictionaries([
            {
                "DEFAULT": "fbcode//buck2/platform:cxx-{}-clang".format(_default_fbcode_platform),
                "ovr_config//toolchain/cxx/constraints:arvr": "fbsource//arvr/tools/buck:default_cxx_toolchain",
            },
            _get_android_cxx_select_map(),
            get_apple_cxx_select_map(),
            _get_infer_select_map(),
            _get_lionhead_select_map(),
            _get_sledge_select_map(),
            _get_xtensa_select_map(),
            _get_infra_asic_fw_select_map(),
        ]),
    )

def default_python_toolchain_inner():
    return select({
        # TODO: Ideally we don't need this, `.buckconfig`s set the coarser fbcode
        # platform to work with v1, which leaks into the few rules which don't
        # set a more granular default.
        "DEFAULT": "fbcode//buck2/platform:py3.8-{}-for-default".format(_default_fbcode_platform),
        "ovr_config//os:macos": "fbcode//buck2/platform:py3.8-macosx-x86_64",
        "ovr_config//os:windows": "fbcode//buck2/platform:py3.8-windows-x86_64",
        "ovr_config//runtime/constraints:platform009": select({
            "ovr_config//third-party/python/constraints:3.8": "fbcode//buck2/platform:py3.8-platform009",
        }),
        "ovr_config//runtime/constraints:platform010": select({
            "ovr_config//third-party/python/constraints:3.10": "fbcode//buck2/platform:py3.10-platform010",
            "ovr_config//third-party/python/constraints:3.10.cinder": "fbcode//buck2/platform:py3.10.cinder-platform010",
            "ovr_config//third-party/python/constraints:3.8": "fbcode//buck2/platform:py3.8-platform010",
            "ovr_config//third-party/python/constraints:cinder.3.8": "fbcode//buck2/platform:cinder_py3.8-platform010",
        }),
        "ovr_config//runtime/constraints:platform010-aarch64": select({
            "ovr_config//third-party/python/constraints:3.8": "fbcode//buck2/platform:py3.8-platform010-aarch64",
        }),
        "ovr_config//runtime/constraints:platform010-compat": select({
            "ovr_config//third-party/python/constraints:3.8": "fbcode//buck2/platform:py3.8-platform010-compat",
        }),
    })

def default_python_bootstrap_toolchain_inner():
    return select({
        "DEFAULT": "fbcode//buck2/platform:bootstrap-py3.8-{}".format(_default_fbcode_platform),
        "ovr_config//runtime/constraints:platform009": "fbcode//buck2/platform:bootstrap-py3.8-platform009",
        "ovr_config//runtime/constraints:platform010": "fbcode//buck2/platform:bootstrap-py3.8-platform010",
        "ovr_config//runtime/constraints:platform010-aarch64": "fbcode//buck2/platform:bootstrap-py3.8-platform010-aarch64",
    })

def default_rust_toolchain_inner():
    return select(_merge_dictionaries([
        _get_apple_rust_select_map(),
        _get_fbcode_select_map("rust"),
    ]))

def default_haskell_toolchain_inner():
    return select(_get_fbcode_select_map("haskell"))
