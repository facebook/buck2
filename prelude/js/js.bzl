load("@fbcode//buck2/prelude/android:android.bzl", "select_android_toolchain")
load("@fbcode//buck2/prelude/android:android_toolchain.bzl", "AndroidPlatformInfo", "AndroidToolchainInfo")
load("@fbcode//buck2/prelude/js:js_bundle.bzl", "js_bundle_impl")
load("@fbcode//buck2/prelude/js:js_bundle_genrule.bzl", "js_bundle_genrule_impl")
load("@fbcode//buck2/prelude/js:js_library.bzl", "js_library_impl")

def _select_platform():
    return select({
        "DEFAULT": "android",
        "ovr_config//os/constraints:iphoneos": "ios",
    })

def _is_release():
    return select({
        "DEFAULT": False,
        "ovr_config//build_mode/constraints:release": True,
    })

implemented_rules = {
    "js_bundle": js_bundle_impl,
    "js_bundle_genrule": js_bundle_genrule_impl,
    "js_library": js_library_impl,
}

extra_attributes = {
    "js_bundle": {
        "_android_toolchain": attr.exec_dep(
            default = select_android_toolchain(),
            providers = [
                AndroidPlatformInfo,
                AndroidToolchainInfo,
            ],
        ),
        "_is_release": attr.bool(
            default = _is_release(),
        ),
        "_platform": attr.string(
            default = _select_platform(),
        ),
    },
    "js_bundle_genrule": {
        "type": attr.string(
            default = "js_bundle_genrule",
        ),
        "_is_release": attr.bool(
            default = _is_release(),
        ),
        "_platform": attr.string(
            default = _select_platform(),
        ),
    },
    "js_library": {
        "_is_release": attr.bool(
            default = _is_release(),
        ),
        "_platform": attr.string(
            default = _select_platform(),
        ),
    },
}
