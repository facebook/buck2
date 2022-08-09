load(":apple_toolchain_setup.bzl", "get_apple_cxx_select_map")

# This module defines the toolchains for languages like C++, Python etc. These are expressed
# as large `select` expressions which are used as default attributes attached to every rule that
# uses them - e.g. `rust_library` has `_rust_toolchain` which points at `default_rust_toolchain()`.
#
# It might be tempting to make targets for these defaults, e.g. `//toolchain:rust` and use that as
# the default, but it's infeasible because:
#
# 1. If the toolchain is an execution dependency, then the `select` statements on `//toolchain:rust`
#    will be resolved with the execution configuration, not the target configuration.
#
# 2. If the toolchain is not an execution dependency, then the execution constraints it supplies
#    won't be applied when picking an execution platform, as execution resolution only considers
#    the direct dependencies of a target.

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
    prefix = "fbcode//buck2/platform:{}-".format(prefix)
    return {
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
        "ovr_config//toolchain/fb:platform010-aarch64-clang": prefix + "platform010-aarch64-clang",
        "ovr_config//toolchain/fb:platform010-aarch64-clang-nosan": prefix + "platform010-aarch64-clang-nosan",
        "ovr_config//toolchain/fb:platform010-aarch64-clang-nosan-split-dwarf": prefix + "platform010-aarch64-clang-nosan-split-dwarf",
        "ovr_config//toolchain/fb:platform010-aarch64-clang-split-dwarf": prefix + "platform010-aarch64-clang-split-dwarf",
        "ovr_config//toolchain/fb:platform010-aarch64-gcc": prefix + "platform010-aarch64-gcc",
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
    return {"ovr_config//toolchain/fb:platform009-infer": "fbcode//buck2/platform:buck2-infer"}

def default_cxx_toolchain_inner():
    return select(
        _merge_dictionaries([
            _get_android_cxx_select_map(),
            get_apple_cxx_select_map(),
            _get_fbcode_select_map("buck2"),
            _get_infer_select_map(),
        ]),
    )

def default_python_toolchain_inner():
    return select({
        # TODO: Ideally we don't need this, `.buckconfig`s set the coarser fbcode
        # platform to work with v1, which leaks into the few rules which don't
        # set a more granular default.
        "DEFAULT": "fbcode//buck2/platform:py3.8-{}".format(_default_fbcode_platform),
        "ovr_config//os:macos": "fbcode//buck2/platform:py3.8-macosx-x86_64",
        "ovr_config//runtime/constraints:platform009": select({
            "ovr_config//third-party/python/constraints:3.8": "fbcode//buck2/platform:py3.8-platform009",
            "ovr_config//third-party/python/constraints:cinder.3.8": "fbcode//buck2/platform:cinder_py3.8-platform009",
        }),
        "ovr_config//runtime/constraints:platform010": select({
            "ovr_config//third-party/python/constraints:3.8": "fbcode//buck2/platform:py3.8-platform010",
            "ovr_config//third-party/python/constraints:cinder.3.8": "fbcode//buck2/platform:cinder_py3.8-platform010",
        }),
        "ovr_config//runtime/constraints:platform010-aarch64": select({
            "ovr_config//third-party/python/constraints:3.8": "fbcode//buck2/platform:py3.8-platform010-aarch64",
        }),
        "ovr_config//runtime/constraints:platform010-compat": select({
            "ovr_config//third-party/python/constraints:3.8": "fbcode//buck2/platform:py3.8-platform010-compat",
            "ovr_config//third-party/python/constraints:cinder.3.8": "fbcode//buck2/platform:cinder_py3.8-platform010-compat",
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
