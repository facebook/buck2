load("@fbcode//buck2/prelude:attributes.bzl", "AaptMode", "DuplicateResourceBehaviour", "TargetCpuType")
load("@fbcode//buck2/prelude/java:dex_toolchain.bzl", "DexToolchainInfo")
load("@fbcode//buck2/prelude/java:java.bzl", "select_junit_toolchain")
load("@fbcode//buck2/prelude/java:java_toolchain.bzl", "JUnitToolchainInfo", "JavaPlatformInfo", "JavaToolchainInfo")
load("@fbcode//buck2/prelude/kotlin:kotlin_toolchain.bzl", "KotlinToolchainInfo")
load(":android_apk.bzl", "android_apk_impl")
load(":android_build_config.bzl", "android_build_config_impl")
load(":android_instrumentation_apk.bzl", "android_instrumentation_apk_impl")
load(":android_instrumentation_test.bzl", "android_instrumentation_test_impl")
load(":android_library.bzl", "android_library_impl")
load(":android_manifest.bzl", "android_manifest_impl")
load(":android_prebuilt_aar.bzl", "android_prebuilt_aar_impl")
load(":android_resource.bzl", "android_resource_impl")
load(":android_toolchain.bzl", "AndroidPlatformInfo", "AndroidToolchainInfo")
load(":configuration.bzl", "cpu_split_transition", "cpu_transition")
load(":gen_aidl.bzl", "gen_aidl_impl")
load(":robolectric_test.bzl", "robolectric_test_impl")

def android_toolchain():
    return attr.toolchain_dep(
        default = "fbcode//buck2/platform/toolchain:android",
        providers = [
            AndroidPlatformInfo,
            AndroidToolchainInfo,
        ],
    )

def _dex_toolchain():
    return attr.toolchain_dep(
        default = "fbcode//buck2/platform/toolchain:dex_for_android",
        providers = [
            DexToolchainInfo,
        ],
    )

def _java_toolchain():
    return attr.toolchain_dep(
        default = "fbcode//buck2/platform/toolchain:java_for_android",
        providers = [
            JavaPlatformInfo,
            JavaToolchainInfo,
        ],
    )

def _kotlin_toolchain():
    return attr.toolchain_dep(
        default = "fbcode//buck2/platform/toolchain:kotlin",
        providers = [
            KotlinToolchainInfo,
        ],
    )

implemented_rules = {
    "android_binary": android_apk_impl,
    "android_build_config": android_build_config_impl,
    "android_instrumentation_apk": android_instrumentation_apk_impl,
    "android_instrumentation_test": android_instrumentation_test_impl,
    "android_library": android_library_impl,
    "android_manifest": android_manifest_impl,
    "android_prebuilt_aar": android_prebuilt_aar_impl,
    "android_resource": android_resource_impl,
    "gen_aidl": gen_aidl_impl,
    "robolectric_test": robolectric_test_impl,
}

extra_attributes = {
    "android_aar": {
        "resources_root": attrs.option(attrs.string(), default = None),
    },
    "android_binary": {
        "aapt_mode": attr.enum(AaptMode, default = "aapt1"),  # Match default in V1
        "build_config_values_file": attr.option(attr.one_of(attr.transition_dep(cfg = cpu_transition), attr.source()), default = None),
        "deps": attr.list(attr.split_transition_dep(cfg = cpu_split_transition), default = []),
        "dex_tool": attr.string(default = "d8"),  # Match default in V1
        "duplicate_resource_behavior": attr.enum(DuplicateResourceBehaviour, default = "allow_by_default"),  # Match default in V1
        "manifest": attr.option(attr.one_of(attr.transition_dep(cfg = cpu_transition), attr.source()), default = None),
        "_android_toolchain": android_toolchain(),
        "_dex_toolchain": _dex_toolchain(),
        "_java_toolchain": _java_toolchain(),
    },
    "android_build_config": {
        "_android_toolchain": android_toolchain(),
        "_java_toolchain": _java_toolchain(),
    },
    "android_instrumentation_apk": {
        "aapt_mode": attr.enum(AaptMode, default = "aapt1"),  # Match default in V1
        "cpu_filters": attr.list(attr.enum(TargetCpuType), default = []),
        "deps": attr.list(attr.split_transition_dep(cfg = cpu_split_transition), default = []),
        "dex_tool": attr.string(default = "d8"),  # Match default in V1
        "manifest": attr.option(attr.one_of(attr.transition_dep(cfg = cpu_transition), attr.source()), default = None),
        "_android_toolchain": android_toolchain(),
        "_dex_toolchain": _dex_toolchain(),
        "_java_toolchain": _java_toolchain(),
    },
    "android_instrumentation_test": {
        "_android_toolchain": android_toolchain(),
        "_java_toolchain": _java_toolchain(),
    },
    "android_library": {
        "resources_root": attr.option(attr.string(), default = None),
        "_android_toolchain": android_toolchain(),
        "_dex_toolchain": _dex_toolchain(),
        "_java_toolchain": _java_toolchain(),
        "_kotlin_toolchain": _kotlin_toolchain(),
    },
    "android_manifest": {
        "_android_toolchain": android_toolchain(),
    },
    "android_prebuilt_aar": {
        # Prebuilt jars are quick to build, and often contain third-party code, which in turn is
        # often a source of annotations and constants. To ease migration to ABI generation from
        # source without deps, we have them present during ABI gen by default.
        "required_for_source_only_abi": attr.bool(default = True),
        "_android_toolchain": android_toolchain(),
        "_dex_toolchain": _dex_toolchain(),
        "_java_toolchain": _java_toolchain(),
    },
    "android_resource": {
        "assets": attr.option(attr.one_of(attr.source(allow_directory = True), attr.dict(key = attr.string(), value = attr.source(), sorted = True)), default = None),
        "project_assets": attr.option(attr.source(allow_directory = True), default = None),
        "project_res": attr.option(attr.source(allow_directory = True), default = None),
        "res": attr.option(attr.one_of(attr.source(allow_directory = True), attr.dict(key = attr.string(), value = attr.source(), sorted = True)), default = None),
        "_android_toolchain": android_toolchain(),
    },
    "gen_aidl": {
        "_android_toolchain": android_toolchain(),
        "_java_toolchain": _java_toolchain(),
    },
    "robolectric_test": {
        "resources_root": attr.option(attr.string(), default = None),
        "_android_toolchain": android_toolchain(),
        "_java_toolchain": _java_toolchain(),
        "_junit_toolchain": attr.exec_dep(
            default = select_junit_toolchain(),
            providers = [
                JUnitToolchainInfo,
            ],
        ),
        "_kotlin_toolchain": _kotlin_toolchain(),
    },
}
