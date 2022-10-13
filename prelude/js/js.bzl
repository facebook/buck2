load("@prelude//android:android.bzl", "android_toolchain")
load("@prelude//js:js_bundle.bzl", "js_bundle_impl")
load("@prelude//js:js_bundle_genrule.bzl", "js_bundle_genrule_impl")
load("@prelude//js:js_library.bzl", "js_library_impl")
load("@prelude//js:js_providers.bzl", "JsToolchainInfo")
load("@prelude//genrule.bzl", "genrule_attributes")

def _select_platform():
    # FIXME: prelude// should be standalone (not refer to ovr_config//)
    return select({
        "DEFAULT": "android",
        "ovr_config//os/constraints:iphoneos": "ios",
    })

def _is_release():
    # FIXME: prelude// should be standalone (not refer to ovr_config//)
    return select({
        "DEFAULT": False,
        "ovr_config//build_mode/constraints:release": True,
    })

def _select_js_toolchain():
    # FIXME: prelude// should be standalone (not refer to fbsource//)
    return "fbsource//xplat/buck2/platform/js:js"

def _is_build_only_native_code():
    return select(
        {
            "DEFAULT": False,
            "fbsource//xplat/buck2/platform/android:build_only_native_code": True,
        },
    )

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
    "js_bundle_genrule": genrule_attributes() | {
        "type": attrs.string(
            default = "js_bundle_genrule",
        ),
        "_is_release": attrs.bool(
            default = _is_release(),
        ),
        "_platform": attrs.string(
            default = _select_platform(),
        ),
    },
    "js_library": {
        "_build_only_native_code": attrs.bool(default = _is_build_only_native_code()),
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
