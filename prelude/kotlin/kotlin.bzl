load("@prelude//java:dex_toolchain.bzl", "DexToolchainInfo")
load("@prelude//java:java.bzl", "dex_min_sdk_version", "select_dex_toolchain", "select_junit_toolchain")
load(
    "@prelude//java:java_toolchain.bzl",
    "JUnitToolchainInfo",
    "JavaPlatformInfo",
    "JavaToolchainInfo",
)
load(
    "@prelude//kotlin:kotlin_toolchain.bzl",
    "KotlinToolchainInfo",
)
load(":kotlin_library.bzl", "kotlin_library_impl")
load(":kotlin_test.bzl", "kotlin_test_impl")

def _select_java_toolchain():
    # FIXME: prelude// should be standalone (not refer to fbsource//)
    return "fbsource//xplat/buck2/platform/java:java"

def _select_kotlin_toolchain():
    # FIXME: prelude// should be standalone (not refer to fbsource//)
    return "fbsource//xplat/buck2/platform/kotlin:kotlin"

implemented_rules = {
    "kotlin_library": kotlin_library_impl,
    "kotlin_test": kotlin_test_impl,
}

extra_attributes = {
    "kotlin_library": {
        "javac": attrs.option(attrs.one_of(attrs.dep(), attrs.source()), default = None),
        "resources_root": attrs.option(attrs.string(), default = None),
        "_dex_min_sdk_version": attrs.option(attrs.int(), default = dex_min_sdk_version()),
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
        "_kotlin_toolchain": attrs.exec_dep(
            default = _select_kotlin_toolchain(),
            providers = [
                KotlinToolchainInfo,
            ],
        ),
    },
    "kotlin_test": {
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
        "_kotlin_toolchain": attrs.exec_dep(
            default = _select_kotlin_toolchain(),
            providers = [
                KotlinToolchainInfo,
            ],
        ),
    },
}
