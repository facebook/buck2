# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

load("@prelude//:genrule.bzl", "genrule_attributes")
load(
    "@prelude//:validation_deps.bzl",
    "VALIDATION_DEPS_ATTR_NAME",
)
load("@prelude//android:cpu_filters.bzl", "ALL_CPU_FILTERS")
load("@prelude//decls:android_rules.bzl", "DuplicateResourceBehaviour")
load("@prelude//decls:common.bzl", "buck")
load("@prelude//decls:core_rules.bzl", "TargetCpuType")
load("@prelude//decls:toolchains_common.bzl", "toolchains_common")
load("@prelude//java:java.bzl", "AbiGenerationMode", "dex_min_sdk_version")
load("@prelude//transitions:constraint_overrides.bzl", "constraint_overrides")
load(":android_aar.bzl", "android_aar_impl")
load(":android_apk.bzl", "android_apk_impl")
load(":android_build_config.bzl", "android_build_config_impl")
load(":android_bundle.bzl", "android_bundle_impl")
load(":android_instrumentation_apk.bzl", "android_instrumentation_apk_impl")
load(":android_instrumentation_test.bzl", "android_instrumentation_test_impl")
load(":android_library.bzl", "android_library_impl")
load(":android_manifest.bzl", "android_manifest_impl")
load(":android_prebuilt_aar.bzl", "android_prebuilt_aar_impl")
load(":android_resource.bzl", "android_resource_impl")
load(":apk_genrule.bzl", "apk_genrule_impl")
load(":build_only_native_code.bzl", "is_build_only_native_code")
load(":configuration.bzl", "cpu_split_transition", "cpu_transition", "is_building_android_binary_attr")
load(":gen_aidl.bzl", "gen_aidl_impl")
load(":prebuilt_native_library.bzl", "prebuilt_native_library_impl")
load(":robolectric_test.bzl", "robolectric_test_impl")
load(":voltron.bzl", "android_app_modularity_impl")

implemented_rules = {
    "android_aar": android_aar_impl,
    "android_app_modularity": android_app_modularity_impl,
    "android_binary": android_apk_impl,
    "android_build_config": android_build_config_impl,
    "android_bundle": android_bundle_impl,
    "android_instrumentation_apk": android_instrumentation_apk_impl,
    "android_instrumentation_test": android_instrumentation_test_impl,
    "android_library": android_library_impl,
    "android_manifest": android_manifest_impl,
    "android_prebuilt_aar": android_prebuilt_aar_impl,
    "android_resource": android_resource_impl,
    "apk_genrule": apk_genrule_impl,
    "gen_aidl": gen_aidl_impl,
    "prebuilt_native_library": prebuilt_native_library_impl,
    "robolectric_test": robolectric_test_impl,
}

# Can't load `read_bool` here because it will cause circular load.
FORCE_SINGLE_CPU = read_root_config("buck2", "android_force_single_cpu") in ("True", "true")
FORCE_SINGLE_DEFAULT_CPU = read_root_config("buck2", "android_force_single_default_cpu") in ("True", "true")
DISABLE_STRIPPING = read_root_config("android", "disable_stripping") in ("True", "true")

# Format is {"ovveride_name": {"re_cap_key": "re_cap_value"}}; for example:
#    {
#        "dynamic-listing": {
#            "platform": "riot",
#            "pool": "EUREKA_POOL",
#        },
#        "test-execution": {
#            "platform": "riot",
#            "pool": "EUREKA_POOL",
#        },
#    }
RE_CAPS = attrs.option(attrs.dict(key = attrs.string(), value = attrs.dict(key = attrs.string(), value = attrs.string())), default = None)

# Format is {"ovveride_name": "re_use_case"}; for example:
#     {
#         "dynamic-listing": "riot",
#         "test-execution": "riot",
#     }
RE_USE_CASE = attrs.option(attrs.dict(key = attrs.string(), value = attrs.string()), default = None)

# Format is {"ovveride_name": {"param_name": param_value}}; for example:
#    {
#        "remote_execution_policy": {"setup_preference_key": "some_json_string"},
#    }
META_INTERNAL_EXTRA_PARAMS = attrs.option(attrs.dict(key = attrs.string(), value = attrs.any()), default = None)

# Common extra attributes shared by android_binary and android_bundle
ANDROID_BINARY_BUNDLE_COMMON_EXTRA_ATTRS = {
    "application_module_blocklist": attrs.option(attrs.list(attrs.transition_dep(cfg = cpu_transition)), default = None),
    "application_module_configs": attrs.dict(key = attrs.string(), value = attrs.list(attrs.transition_dep(cfg = cpu_transition)), sorted = False, default = {}),
    "build_config_values_file": attrs.option(attrs.one_of(attrs.transition_dep(cfg = cpu_transition), attrs.source()), default = None),
    "default_module_manifest_skeleton": attrs.option(attrs.one_of(attrs.transition_dep(cfg = cpu_transition), attrs.source()), default = None),
    "deps": attrs.list(attrs.split_transition_dep(cfg = cpu_split_transition), default = []),
    "duplicate_class_checker_enabled": attrs.bool(default = False),
    "duplicate_resource_behavior": attrs.enum(DuplicateResourceBehaviour, default = "allow_by_default"),  # Match default in V1
    "manifest": attrs.option(attrs.one_of(attrs.transition_dep(cfg = cpu_transition), attrs.source()), default = None),
    "manifest_skeleton": attrs.option(attrs.one_of(attrs.transition_dep(cfg = cpu_transition), attrs.source()), default = None),
    "min_sdk_version": attrs.option(attrs.int(), default = None),
    "native_library_merge_code_generator": attrs.option(attrs.exec_dep(), default = None),
    "native_library_merge_glue": attrs.option(attrs.split_transition_dep(cfg = cpu_split_transition), default = None),
    "native_library_merge_linker_args": attrs.option(attrs.dict(key = attrs.string(), value = attrs.list(attrs.arg())), default = None),
    "package_validators": attrs.list(
        attrs.tuple(
            attrs.exec_dep(providers = [RunInfo]),
            attrs.list(attrs.arg(), default = []),
        ),
        default = [],
    ),
    "relinker_extra_deps": attrs.list(attrs.split_transition_dep(cfg = cpu_split_transition), default = []),
    "_android_toolchain": toolchains_common.android(),
    "_cxx_toolchain": attrs.split_transition_dep(cfg = cpu_split_transition, default = "toolchains//:android-hack"),
    "_dex_toolchain": toolchains_common.dex(),
    "_exec_os_type": buck.exec_os_type_arg(),
    "_is_building_android_binary": attrs.default_only(attrs.bool(default = True)),
    "_is_force_single_cpu": attrs.default_only(attrs.bool(default = FORCE_SINGLE_CPU)),
    "_is_force_single_default_cpu": attrs.default_only(attrs.bool(default = FORCE_SINGLE_DEFAULT_CPU)),
    "_java_toolchain": toolchains_common.java_for_android(),
    VALIDATION_DEPS_ATTR_NAME: attrs.set(attrs.transition_dep(cfg = cpu_transition), sorted = True, default = []),
}

# android_binary specific extra attributes
ANDROID_BINARY_EXTRA_ATTRS = {
    "strip_libraries": attrs.bool(default = not DISABLE_STRIPPING),
} | constraint_overrides.attributes

# android_bundle specific extra attributes
ANDROID_BUNDLE_EXTRA_ATTRS = {
    "use_derived_apk": attrs.bool(default = False),
}

extra_attributes = {
    "android_aar": {
        "abi_generation_mode": attrs.option(attrs.enum(AbiGenerationMode), default = None),
        "compress_asset_libraries": attrs.default_only(attrs.bool(default = False)),
        "cpu_filters": attrs.list(attrs.enum(TargetCpuType), default = ALL_CPU_FILTERS),
        "deps": attrs.list(attrs.split_transition_dep(cfg = cpu_split_transition), default = []),
        "min_sdk_version": attrs.option(attrs.int(), default = None),
        "native_library_merge_glue": attrs.option(attrs.split_transition_dep(cfg = cpu_split_transition), default = None),
        "native_library_merge_linker_args": attrs.option(attrs.dict(key = attrs.string(), value = attrs.list(attrs.arg())), default = None),
        "package_asset_libraries": attrs.bool(default = True),
        "package_resources": attrs.bool(default = True),
        "relinker_extra_deps": attrs.list(attrs.split_transition_dep(cfg = cpu_split_transition), default = []),
        "resources_root": attrs.option(attrs.string(), default = None),
        "strip_libraries": attrs.default_only(attrs.bool(default = not DISABLE_STRIPPING)),
        "_android_toolchain": toolchains_common.android(),
        "_cxx_toolchain": attrs.split_transition_dep(cfg = cpu_split_transition, default = "toolchains//:android-hack"),
        "_is_building_android_binary": attrs.default_only(attrs.bool(default = True)),
        "_is_force_single_cpu": attrs.default_only(attrs.bool(default = FORCE_SINGLE_CPU)),
        "_is_force_single_default_cpu": attrs.default_only(attrs.bool(default = FORCE_SINGLE_DEFAULT_CPU)),
        "_java_toolchain": toolchains_common.java_for_android(),
    },
    "android_app_modularity": {
        "_android_toolchain": toolchains_common.android(),
        "_build_only_native_code": attrs.default_only(attrs.bool(default = is_build_only_native_code())),
    },
    "android_binary": ANDROID_BINARY_BUNDLE_COMMON_EXTRA_ATTRS | ANDROID_BINARY_EXTRA_ATTRS,
    "android_build_config": {
        "_android_toolchain": toolchains_common.android(),
        "_build_only_native_code": attrs.default_only(attrs.bool(default = is_build_only_native_code())),
        "_is_building_android_binary": is_building_android_binary_attr(),
        "_java_toolchain": toolchains_common.java_for_android(),
    },
    "android_bundle": ANDROID_BINARY_BUNDLE_COMMON_EXTRA_ATTRS | ANDROID_BUNDLE_EXTRA_ATTRS,
    "android_instrumentation_apk": {
        "apk": attrs.dep(),
        "cpu_filters": attrs.list(attrs.enum(TargetCpuType), default = []),
        "deps": attrs.list(attrs.split_transition_dep(cfg = cpu_split_transition), default = []),
        "is_self_instrumenting": attrs.bool(default = False),
        "manifest": attrs.option(attrs.one_of(attrs.transition_dep(cfg = cpu_transition), attrs.source()), default = None),
        "manifest_skeleton": attrs.option(attrs.one_of(attrs.transition_dep(cfg = cpu_transition), attrs.source()), default = None),
        "min_sdk_version": attrs.option(attrs.int(), default = None),
        "_android_toolchain": toolchains_common.android(),
        "_cxx_toolchain": attrs.split_transition_dep(cfg = cpu_split_transition, default = "toolchains//:android-hack"),
        "_dex_toolchain": toolchains_common.dex(),
        "_exec_os_type": buck.exec_os_type_arg(),
        "_is_building_android_binary": attrs.default_only(attrs.bool(default = True)),
        "_is_force_single_cpu": attrs.default_only(attrs.bool(default = FORCE_SINGLE_CPU)),
        "_is_force_single_default_cpu": attrs.default_only(attrs.bool(default = FORCE_SINGLE_DEFAULT_CPU)),
        "_java_toolchain": toolchains_common.java_for_android(),
    },
    "android_instrumentation_test": {
        "extra_instrumentation_args": attrs.option(attrs.dict(key = attrs.string(), value = attrs.arg()), default = None),
        "instrumentation_test_listener": attrs.option(attrs.exec_dep(), default = None),
        "instrumentation_test_listener_class": attrs.option(attrs.string(), default = None),
        "is_self_instrumenting": attrs.bool(default = False),
        "meta_internal_extra_params": META_INTERNAL_EXTRA_PARAMS,
        "re_caps": RE_CAPS,
        "re_use_case": RE_USE_CASE,
        "_android_toolchain": toolchains_common.android(),
        "_exec_os_type": buck.exec_os_type_arg(),
        "_java_test_toolchain": toolchains_common.java_for_host_test(),
        "_java_toolchain": toolchains_common.java_for_android(),
    },
    "android_library": {
        "abi_generation_mode": attrs.option(attrs.enum(AbiGenerationMode), default = None),
        "android_optional_jars": attrs.option(attrs.list(attrs.dep()), default = None),
        "resources_root": attrs.option(attrs.string(), default = None),
        VALIDATION_DEPS_ATTR_NAME: attrs.set(attrs.dep(), sorted = True, default = []),
        "_android_toolchain": toolchains_common.android(),
        "_build_only_native_code": attrs.default_only(attrs.bool(default = is_build_only_native_code())),
        "_dex_min_sdk_version": attrs.default_only(attrs.option(attrs.int(), default = dex_min_sdk_version())),
        "_dex_toolchain": toolchains_common.dex(),
        "_exec_os_type": buck.exec_os_type_arg(),
        "_is_building_android_binary": is_building_android_binary_attr(),
        "_java_toolchain": toolchains_common.java_for_android(),
        "_kotlin_toolchain": toolchains_common.kotlin_for_android(),
    },
    "android_manifest": {
        "_android_toolchain": toolchains_common.android(),
    },
    "android_prebuilt_aar": {
        # Prebuilt jars are quick to build, and often contain third-party code, which in turn is
        # often a source of annotations and constants. To ease migration to ABI generation from
        # source without deps, we have them present during ABI gen by default.
        "required_for_source_only_abi": attrs.bool(default = True),
        "_android_toolchain": toolchains_common.android(),
        "_build_only_native_code": attrs.default_only(attrs.bool(default = is_build_only_native_code())),
        "_dex_min_sdk_version": attrs.default_only(attrs.option(attrs.int(), default = dex_min_sdk_version())),
        "_dex_toolchain": toolchains_common.dex(),
        "_exec_os_type": buck.exec_os_type_arg(),
        "_is_building_android_binary": attrs.default_only(attrs.bool(default = False)),
        "_java_toolchain": toolchains_common.java_for_android(),
    },
    "android_resource": {
        "assets": attrs.option(attrs.one_of(attrs.source(allow_directory = True), attrs.dict(key = attrs.string(), value = attrs.source(), sorted = True)), default = None),
        "project_assets": attrs.option(attrs.source(allow_directory = True), default = None),
        "project_res": attrs.option(attrs.source(allow_directory = True), default = None),
        "res": attrs.option(attrs.one_of(attrs.source(allow_directory = True), attrs.dict(key = attrs.string(), value = attrs.source(), sorted = True)), default = None),
        "_android_toolchain": toolchains_common.android(),
        "_build_only_native_code": attrs.default_only(attrs.bool(default = is_build_only_native_code())),
        "_is_building_android_binary": attrs.default_only(attrs.bool(default = False)),
        "_java_toolchain": toolchains_common.java_for_android(),
    },
    "apk_genrule": genrule_attributes() | {
        "default_outs": attrs.option(attrs.set(attrs.string(), sorted = False), default = None),
        "outs": attrs.option(attrs.dict(key = attrs.string(), value = attrs.set(attrs.string(), sorted = False), sorted = False), default = None),
        "type": attrs.string(default = "apk"),
        "use_derived_apk": attrs.bool(default = False),
        "_android_toolchain": toolchains_common.android(),
        "_exec_os_type": buck.exec_os_type_arg(),
        "_java_toolchain": toolchains_common.java_for_android(),
    },
    "gen_aidl": {
        "import_paths": attrs.list(attrs.arg(), default = []),
        "_android_toolchain": toolchains_common.android(),
        "_exec_os_type": buck.exec_os_type_arg(),
        "_java_toolchain": toolchains_common.java_for_android(),
    },
    "prebuilt_native_library": {
        "native_libs": attrs.source(allow_directory = True),
    },
    "robolectric_test": {
        "abi_generation_mode": attrs.option(attrs.enum(AbiGenerationMode), default = None),
        "android_optional_jars": attrs.option(attrs.list(attrs.dep()), default = None),
        "java_agents": attrs.list(attrs.source(), default = []),
        "resources_root": attrs.option(attrs.string(), default = None),
        "robolectric_runtime_dependencies": attrs.list(attrs.source(), default = []),
        "test_class_names_file": attrs.option(attrs.source(), default = None),
        "unbundled_resources_root": attrs.option(attrs.source(allow_directory = True), default = None),
        "_android_toolchain": toolchains_common.android(),
        "_build_only_native_code": attrs.default_only(attrs.bool(default = is_build_only_native_code())),
        "_exec_os_type": buck.exec_os_type_arg(),
        "_is_building_android_binary": attrs.default_only(attrs.bool(default = False)),
        "_java_test_toolchain": toolchains_common.java_test(),
        "_java_toolchain": toolchains_common.java_for_host_test(),
        "_kotlin_toolchain": toolchains_common.kotlin(),
    },
}
