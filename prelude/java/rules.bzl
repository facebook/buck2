# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

load("@prelude//java/plugins:java_annotation_processor.bzl", "java_annotation_processor")
load("@prelude//java/plugins:java_plugin.bzl", "java_plugin")
load(":gwt_binary.bzl", "gwt_binary")
load(":jar_genrule.bzl", "jar_genrule")
load(":java_binary.bzl", "java_binary")
load(":java_library.bzl", "java_library")
load(":java_test.bzl", "java_test")
load(":prebuilt_jar.bzl", "prebuilt_jar")

rules = {
    "gwt_binary": gwt_binary,
    "jar_genrule": jar_genrule,
    "java_annotation_processor": java_annotation_processor,
    "java_binary": java_binary,
    "java_library": java_library,
    "java_plugin": java_plugin,
    "java_test": java_test,
    "prebuilt_jar": prebuilt_jar,
}
