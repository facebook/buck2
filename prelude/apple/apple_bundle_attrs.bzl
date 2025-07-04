# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

load("@prelude//decls:apple_rules.bzl", "apple_bundle_default_attrs")

def apple_watchos_bundle_attrs():
    attributes = apple_bundle_default_attrs()
    attributes.update({
        "bundle_type": attrs.string(default = "watchapp"),
    })
    return attributes
