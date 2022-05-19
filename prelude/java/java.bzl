load("@fbcode//buck2/prelude/java:dex_toolchain.bzl", "DexToolchainInfo")
load(
    "@fbcode//buck2/prelude/java:java_toolchain.bzl",
    "JUnitToolchainInfo",
    "JavaPlatformInfo",
    "JavaToolchainInfo",
)
load("@fbcode//buck2/prelude/java/plugins:java_annotation_processor.bzl", "java_annotation_processor_impl")
load("@fbcode//buck2/prelude/java/plugins:java_plugin.bzl", "java_plugin_impl")
load(":jar_genrule.bzl", "jar_genrule_impl")
load(":java_binary.bzl", "java_binary_impl")
load(":java_library.bzl", "java_library_impl")
load(":java_test.bzl", "java_test_impl")
load(":keystore.bzl", "keystore_impl")
load(":prebuilt_jar.bzl", "prebuilt_jar_impl")

def _select_java_toolchain():
    return select(
        {
            # TODO: add buck specific platform constraints
            # use .buckconfig from buck cell by default
            "DEFAULT": "buck//config/buck_2:java_bootstrap",
            # if target is for android (fbsource repo) then use .buckconfig from fbsource cell
            "ovr_config//os/constraints:android": "fbsource//xplat/buck2/platform/java:java",
            # if target is with fbcode constraint then use .buckconfig from fbcode cell
            "ovr_config//toolchain/fb/constraints:fbcode": "fbcode//buck2/platform:java_fbcode",
        },
    )

def select_dex_toolchain():
    return select(
        {
            # Only need a Dex toolchain for Android builds.
            "DEFAULT": None,
            "ovr_config//os/constraints:android": "fbsource//xplat/buck2/platform/java:dex",
        },
    )

def select_junit_toolchain():
    return "fbsource//xplat/buck2/platform/java:junit"

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
        "_java_toolchain": attr.exec_dep(
            default = _select_java_toolchain(),
            providers = [
                JavaToolchainInfo,
            ],
        ),
    },
    "java_binary": {
        "meta_inf_directory": attr.option(attr.source(allow_directory = True), default = None),
        "_java_toolchain": attr.exec_dep(
            default = _select_java_toolchain(),
            providers = [
                JavaPlatformInfo,
                JavaToolchainInfo,
            ],
        ),
    },
    "java_library": {
        "javac": attr.option(attr.one_of(attr.dep(), attr.source()), default = None),
        "resources_root": attr.option(attr.string(), default = None),
        "_dex_toolchain": attr.option(attr.exec_dep(
            providers = [
                DexToolchainInfo,
            ],
        ), default = select_dex_toolchain()),
        "_java_toolchain": attr.exec_dep(
            default = _select_java_toolchain(),
            providers = [
                JavaPlatformInfo,
                JavaToolchainInfo,
            ],
        ),
    },
    "java_test": {
        "javac": attr.option(attr.one_of(attr.dep(), attr.source()), default = None),
        "resources_root": attr.option(attr.string(), default = None),
        "_java_toolchain": attr.exec_dep(
            default = _select_java_toolchain(),
            providers = [
                JavaPlatformInfo,
                JavaToolchainInfo,
            ],
        ),
        "_junit_toolchain": attr.exec_dep(
            default = select_junit_toolchain(),
            providers = [
                JUnitToolchainInfo,
            ],
        ),
    },
    "java_test_runner": {
        "resources_root": attr.option(attr.string(), default = None),
    },
    "prebuilt_jar": {
        "generate_abi": attr.bool(default = True),
        "_dex_toolchain": attr.option(attr.exec_dep(
            providers = [
                DexToolchainInfo,
            ],
        ), default = select_dex_toolchain()),
        "_java_toolchain": attr.exec_dep(
            default = _select_java_toolchain(),
            providers = [
                JavaPlatformInfo,
                JavaToolchainInfo,
            ],
        ),
    },
}
