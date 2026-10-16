# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

load(":js_bundle.bzl", "js_bundle")
load(":js_bundle_genrule.bzl", "js_bundle_genrule")
load(":js_library.bzl", "js_library")

rules = {
    "js_bundle": js_bundle,
    "js_bundle_genrule": js_bundle_genrule,
    "js_library": js_library,
}
