load("@prelude//android:android.bzl", "android_toolchain")
load("@prelude//js:js_bundle.bzl", "js_bundle_impl")
load("@prelude//js:js_bundle_genrule.bzl", "js_bundle_genrule_impl")
load("@prelude//js:js_library.bzl", "js_library_impl")
load("@prelude//js:js_providers.bzl", "JsToolchainInfo")

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

def _select_js_toolchain():
    return "fbsource//xplat/buck2/platform/js:js"

implemented_rules = {
    "js_bundle": js_bundle_impl,
    "js_bundle_genrule": js_bundle_genrule_impl,
    "js_library": js_library_impl,
}

extra_attributes = {
    "js_bundle": {
        "_android_toolchain": android_toolchain(),
        "_is_release": attrs.bool(
            default = _is_release(),
        ),
        "_js_toolchain": attrs.exec_dep(
            default = _select_js_toolchain(),
            providers = [
                JsToolchainInfo,
            ],
        ),
        "_platform": attrs.string(
            default = _select_platform(),
        ),
    },
    "js_bundle_genrule": {
        "type": attrs.string(
            default = "js_bundle_genrule",
        ),
        "_cache_mode": attrs.dep(default = "fbcode//buck2/platform/cache_mode:cache_mode"),
        "_is_release": attrs.bool(
            default = _is_release(),
        ),
        "_platform": attrs.string(
            default = _select_platform(),
        ),
    },
    "js_library": {
        "_is_release": attrs.bool(
            default = _is_release(),
        ),
        "_js_toolchain": attrs.exec_dep(
            default = _select_js_toolchain(),
            providers = [
                JsToolchainInfo,
            ],
        ),
        "_platform": attrs.string(
            default = _select_platform(),
        ),
    },
}
