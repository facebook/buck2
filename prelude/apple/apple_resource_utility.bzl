# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

load(":apple_bundle_destination.bzl", "AppleBundleDestination")
load(
    ":apple_resource_types.bzl",
    "AppleResourceDestination",  # @unused Used as a type
)

def apple_bundle_destination_from_resource_destination(res_destination: AppleResourceDestination) -> AppleBundleDestination:
    return AppleBundleDestination(res_destination.value)
