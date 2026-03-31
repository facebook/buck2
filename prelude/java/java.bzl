# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

load("@prelude//java/plugins:java_annotation_processor.bzl", "java_annotation_processor_impl")
load("@prelude//java/plugins:java_plugin.bzl", "java_plugin_impl")
load(":gwt_binary.bzl", "gwt_binary_impl")
load(":jar_genrule.bzl", "jar_genrule_impl")
load(":java_binary.bzl", "java_binary_impl")
load(":java_library.bzl", "java_library_impl")
load(":java_test.bzl", "java_test_impl")
load(":keystore.bzl", "keystore_impl")
load(":prebuilt_jar.bzl", "prebuilt_jar_impl")

implemented_rules = {
    "gwt_binary": gwt_binary_impl,
    "jar_genrule": jar_genrule_impl,
    "java_annotation_processor": java_annotation_processor_impl,
    "java_binary": java_binary_impl,
    "java_library": java_library_impl,
    "java_plugin": java_plugin_impl,
    "java_test": java_test_impl,
    "keystore": keystore_impl,
    "prebuilt_jar": prebuilt_jar_impl,
}
