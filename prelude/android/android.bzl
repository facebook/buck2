# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

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
