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
load(":apk_genrule.bzl", "apk_genrule_impl")
load(":configuration.bzl", "cpu_split_transition", "cpu_transition")
load(":gen_aidl.bzl", "gen_aidl_impl")
load(":prebuilt_native_library.bzl", "prebuilt_native_library_impl")
load(":robolectric_test.bzl", "robolectric_test_impl")
load(":voltron.bzl", "android_app_modularity_impl")

def android_toolchain():
    return attrs.toolchain_dep(
        default = "fbcode//buck2/platform/toolchain:android",
        providers = [
            AndroidPlatformInfo,
            AndroidToolchainInfo,
        ],
    )

def _dex_toolchain():
    return attrs.toolchain_dep(
        default = "fbcode//buck2/platform/toolchain:dex_for_android",
        providers = [
            DexToolchainInfo,
        ],
    )

def _java_toolchain():
    return attrs.toolchain_dep(
        default = "fbcode//buck2/platform/toolchain:java_for_android",
        providers = [
            JavaPlatformInfo,
            JavaToolchainInfo,
        ],
    )

def _kotlin_toolchain():
    return attrs.toolchain_dep(
        default = "fbcode//buck2/platform/toolchain:kotlin",
        providers = [
            KotlinToolchainInfo,
        ],
    )

implemented_rules = {
    "android_app_modularity": android_app_modularity_impl,
    "android_binary": android_apk_impl,
    "android_build_config": android_build_config_impl,
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

extra_attributes = {
    "android_aar": {
        "resources_root": attrs.option(attrs.string(), default = None),
    },
    "android_app_modularity": {
        "_android_toolchain": android_toolchain(),
    },
    "android_binary": {
        "aapt_mode": attrs.enum(AaptMode, default = "aapt1"),  # Match default in V1
        "application_module_configs": attrs.dict(key = attrs.string(), value = attrs.list(attrs.transition_dep(cfg = cpu_transition)), sorted = False, default = {}),
        "build_config_values_file": attrs.option(attrs.one_of(attrs.transition_dep(cfg = cpu_transition), attrs.source()), default = None),
        "deps": attrs.list(attrs.split_transition_dep(cfg = cpu_split_transition), default = []),
        "dex_tool": attrs.string(default = "d8"),  # Match default in V1
        "duplicate_resource_behavior": attrs.enum(DuplicateResourceBehaviour, default = "allow_by_default"),  # Match default in V1
        "manifest": attrs.option(attrs.one_of(attrs.transition_dep(cfg = cpu_transition), attrs.source()), default = None),
        "_android_installer": attrs.label(
            default = "buck//src/com/facebook/buck/installer/android:android_installer",
        ),
        "_android_toolchain": android_toolchain(),
        "_dex_toolchain": _dex_toolchain(),
        "_java_toolchain": _java_toolchain(),
    },
    "android_build_config": {
        "_android_toolchain": android_toolchain(),
        "_java_toolchain": _java_toolchain(),
    },
    "android_instrumentation_apk": {
        "aapt_mode": attrs.enum(AaptMode, default = "aapt1"),  # Match default in V1
        "cpu_filters": attrs.list(attrs.enum(TargetCpuType), default = []),
        "deps": attrs.list(attrs.split_transition_dep(cfg = cpu_split_transition), default = []),
        "dex_tool": attrs.string(default = "d8"),  # Match default in V1
        "manifest": attrs.option(attrs.one_of(attrs.transition_dep(cfg = cpu_transition), attrs.source()), default = None),
        "_android_toolchain": android_toolchain(),
        "_dex_toolchain": _dex_toolchain(),
        "_java_toolchain": _java_toolchain(),
    },
    "android_instrumentation_test": {
        "_android_toolchain": android_toolchain(),
        "_java_toolchain": _java_toolchain(),
    },
    "android_library": {
        "resources_root": attrs.option(attrs.string(), default = None),
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
        "required_for_source_only_abi": attrs.bool(default = True),
        "_android_toolchain": android_toolchain(),
        "_dex_toolchain": _dex_toolchain(),
        "_java_toolchain": _java_toolchain(),
    },
    "android_resource": {
        "assets": attrs.option(attrs.one_of(attrs.source(allow_directory = True), attrs.dict(key = attrs.string(), value = attrs.source(), sorted = True)), default = None),
        "project_assets": attrs.option(attrs.source(allow_directory = True), default = None),
        "project_res": attrs.option(attrs.source(allow_directory = True), default = None),
        "res": attrs.option(attrs.one_of(attrs.source(allow_directory = True), attrs.dict(key = attrs.string(), value = attrs.source(), sorted = True)), default = None),
        "_android_toolchain": android_toolchain(),
    },
    "apk_genrule": {
        "type": attrs.string(default = "apk"),
    },
    "gen_aidl": {
        "_android_toolchain": android_toolchain(),
        "_java_toolchain": _java_toolchain(),
    },
    "prebuilt_native_library": {
        "native_libs": attrs.source(allow_directory = True),
    },
    "robolectric_test": {
        "resources_root": attrs.option(attrs.string(), default = None),
        "_android_toolchain": android_toolchain(),
        "_java_toolchain": _java_toolchain(),
        "_junit_toolchain": attrs.exec_dep(
            default = select_junit_toolchain(),
            providers = [
                JUnitToolchainInfo,
            ],
        ),
        "_kotlin_toolchain": _kotlin_toolchain(),
    },
}
