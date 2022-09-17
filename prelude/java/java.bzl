load("@prelude//java:dex_toolchain.bzl", "DexToolchainInfo")
load(
    "@prelude//java:java_toolchain.bzl",
    "JUnitToolchainInfo",
    "JavaPlatformInfo",
    "JavaToolchainInfo",
    "PrebuiltJarToolchainInfo",
)
load("@prelude//java/plugins:java_annotation_processor.bzl", "java_annotation_processor_impl")
load("@prelude//java/plugins:java_plugin.bzl", "java_plugin_impl")
load(":jar_genrule.bzl", "jar_genrule_impl")
load(":java_binary.bzl", "java_binary_impl")
load(":java_library.bzl", "java_library_impl")
load(":java_test.bzl", "java_test_impl")
load(":keystore.bzl", "keystore_impl")
load(":prebuilt_jar.bzl", "prebuilt_jar_impl")

def _select_java_toolchain():
    # FIXME: prelude// should be standalone (not refer to fbcode//, buck// or ovr_config//)
    return select(
        {
            # TODO: add buck specific platform constraints
            # use .buckconfig from buck cell by default
            "DEFAULT": "buck//config/buck_2:java_bootstrap",
            # if target is meant to run on host but with an android environment then use .buckconfig from fbsource cell
            "ovr_config//runtime/constraints:android-host-test": "fbsource//xplat/buck2/platform/java:java-for-host-tests",
            # if target is with fbcode constraint then use .buckconfig from fbcode cell
            "ovr_config//runtime:fbcode": "fbcode//buck2/platform:java_fbcode",
            # if target is for android (fbsource repo) then use .buckconfig from fbsource cell
            "ovr_config//toolchain/fb:android-ndk": "fbsource//xplat/buck2/platform/java:java",
        },
    )

def select_dex_toolchain():
    # FIXME: prelude// should be standalone (not refer to fbsource//, ovr_config//)
    return select(
        {
            # Only need a Dex toolchain for Android builds.
            "DEFAULT": None,
            "ovr_config//os/constraints:android": "fbsource//xplat/buck2/platform/java:dex",
        },
    )

def select_junit_toolchain():
    # FIXME: prelude// should be standalone (not refer to fbsource//)
    return "fbsource//xplat/buck2/platform/java:junit"

def select_prebuilt_jar_toolchain():
    # FIXME: prelude// should be standalone (not refer to fbcode//)
    return "fbcode//buck2/platform:prebuilt_jar"

implemented_rules = {
    "jar_genrule": jar_genrule_impl,
    "java_annotation_processor": java_annotation_processor_impl,
    "java_binary": java_binary_impl,
    "java_library": java_library_impl,
    "java_plugin": java_plugin_impl,
    "java_test": java_test_impl,
    "keystore": keystore_impl,
    "prebuilt_jar": prebuilt_jar_impl,
}

extra_attributes = {
    "jar_genrule": {
        # FIXME: prelude// should be standalone (not refer to fbsource//)
        "_cache_mode": attrs.dep(default = "fbsource//xplat/buck2/platform/cache_mode:cache_mode"),
        "_java_toolchain": attrs.exec_dep(
            default = _select_java_toolchain(),
            providers = [
                JavaToolchainInfo,
            ],
        ),
    },
    "java_binary": {
        "meta_inf_directory": attrs.option(attrs.source(allow_directory = True), default = None),
        "_java_toolchain": attrs.exec_dep(
            default = _select_java_toolchain(),
            providers = [
                JavaPlatformInfo,
                JavaToolchainInfo,
            ],
        ),
    },
    "java_library": {
        "javac": attrs.option(attrs.one_of(attrs.dep(), attrs.source()), default = None),
        "resources_root": attrs.option(attrs.string(), default = None),
        "_dex_toolchain": attrs.option(attrs.exec_dep(
            providers = [
                DexToolchainInfo,
            ],
        ), default = select_dex_toolchain()),
        "_java_toolchain": attrs.exec_dep(
            default = _select_java_toolchain(),
            providers = [
                JavaPlatformInfo,
                JavaToolchainInfo,
            ],
        ),
    },
    "java_test": {
        "javac": attrs.option(attrs.one_of(attrs.dep(), attrs.source()), default = None),
        "resources_root": attrs.option(attrs.string(), default = None),
        "_java_toolchain": attrs.exec_dep(
            default = _select_java_toolchain(),
            providers = [
                JavaPlatformInfo,
                JavaToolchainInfo,
            ],
        ),
        "_junit_toolchain": attrs.exec_dep(
            default = select_junit_toolchain(),
            providers = [
                JUnitToolchainInfo,
            ],
        ),
    },
    "java_test_runner": {
        "resources_root": attrs.option(attrs.string(), default = None),
    },
    "prebuilt_jar": {
        "generate_abi": attrs.bool(default = True),
        # Prebuilt jars are quick to build, and often contain third-party code, which in turn is
        # often a source of annotations and constants. To ease migration to ABI generation from
        # source without deps, we have them present during ABI gen by default.
        "required_for_source_only_abi": attrs.bool(default = True),
        "_dex_toolchain": attrs.option(attrs.exec_dep(
            providers = [
                DexToolchainInfo,
            ],
        ), default = select_dex_toolchain()),
        "_prebuilt_jar_toolchain": attrs.exec_dep(
            default = select_prebuilt_jar_toolchain(),
            providers = [
                PrebuiltJarToolchainInfo,
            ],
        ),
    },
}
