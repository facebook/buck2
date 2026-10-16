# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

load(":android_aar.bzl", "android_aar")
load(":android_apk.bzl", "android_binary")
load(":android_build_config.bzl", "android_build_config")
load(":android_bundle.bzl", "android_bundle")
load(":android_instrumentation_apk.bzl", "android_instrumentation_apk")
load(":android_instrumentation_test.bzl", "android_instrumentation_test")
load(":android_library.bzl", "android_library")
load(":android_manifest.bzl", "android_manifest")
load(":android_prebuilt_aar.bzl", "android_prebuilt_aar")
load(":android_resource.bzl", "android_resource")
load(":apk_genrule.bzl", "apk_genrule")
load(":gen_aidl.bzl", "gen_aidl")
load(":keystore.bzl", "keystore")
load(":prebuilt_native_library.bzl", "prebuilt_native_library")
load(":robolectric_test.bzl", "robolectric_test")
load(":voltron.bzl", "android_app_modularity")

rules = {
    "android_aar": android_aar,
    "android_app_modularity": android_app_modularity,
    "android_binary": android_binary,
    "android_build_config": android_build_config,
    "android_bundle": android_bundle,
    "android_instrumentation_apk": android_instrumentation_apk,
    "android_instrumentation_test": android_instrumentation_test,
    "android_library": android_library,
    "android_manifest": android_manifest,
    "android_prebuilt_aar": android_prebuilt_aar,
    "android_resource": android_resource,
    "apk_genrule": apk_genrule,
    "gen_aidl": gen_aidl,
    "keystore": keystore,
    "prebuilt_native_library": prebuilt_native_library,
    "robolectric_test": robolectric_test,
}
